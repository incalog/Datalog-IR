package org.inca.incer.indices;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.scope.IBaseIndex;
import org.eclipse.viatra.query.runtime.api.scope.IIndexingErrorListener;
import org.eclipse.viatra.query.runtime.api.scope.IInstanceObserver;
import org.eclipse.viatra.query.runtime.api.scope.ViatraBaseIndexChangeListener;
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import org.inca.incer.Incrementalizable;
import org.inca.incer.listeners.IDataTypeInstanceListener;
import org.inca.incer.listeners.IInstanceListener;
import org.inca.incer.listeners.INodeLinkInstanceListener;
import org.inca.incer.listeners.INodeTypeInstanceListener;
import org.inca.meta.MetaElements;
import org.inca.meta.MetaElements.DataType;
import org.inca.meta.MetaElements.NodeLink;
import org.inca.meta.MetaElements.NodeType;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;

public class Indices implements IBaseIndex {

    final Map<NodeType, Set<Object>> nodeTypeInstances;
    final Map<NodeType, Set<INodeTypeInstanceListener>> nodeTypeInstanceListeners;

    final Map<DataType, Multiset<Object>> dataTypeInstances;
    final Map<DataType, Set<IDataTypeInstanceListener>> dataTypeInstanceListeners;

    // source -> {targets}
    final Map<NodeLink, Map<Object, Set<Object>>> nodeLinkInstances;
    // target -> {sources}
    final Map<NodeLink, Map<Object, Set<Object>>> nodeLinkInstancesReversed;
    final Map<NodeLink, Set<INodeLinkInstanceListener>> nodeLinkInstanceListeners;

    public static final Map<Class<?>, Set<Class<?>>> subTypeMap = new HashMap<>();
    public static final Map<Class<?>, Set<Class<?>>> superTypeMap = new HashMap<>();

    private final Set<ViatraBaseIndexChangeListener> changeListeners;
    private AdvancedViatraQueryEngine engine;

    private boolean isDirty;

    public Indices() {
        this(null);
    }

    public Indices(final AdvancedViatraQueryEngine engine) {
        this.nodeTypeInstances = new HashMap<>();
        this.nodeTypeInstanceListeners = new HashMap<>();
        this.dataTypeInstances = new HashMap<>();
        this.dataTypeInstanceListeners = new HashMap<>();
        this.nodeLinkInstances = new HashMap<>();
        this.nodeLinkInstancesReversed = new HashMap<>();
        this.nodeLinkInstanceListeners = new HashMap<>();
        this.changeListeners = new HashSet<>();
        this.engine = engine;
    }

    public void initializeWith(final Incrementalizable root) {
        root.insert(this);
    }

    public void dispose() {
        this.nodeTypeInstances.clear();
        this.nodeTypeInstanceListeners.clear();
        this.dataTypeInstances.clear();
        this.dataTypeInstanceListeners.clear();
        this.nodeLinkInstances.clear();
        this.nodeLinkInstancesReversed.clear();
        this.nodeLinkInstanceListeners.clear();
        this.changeListeners.clear();
        this.engine = null;
    }

    public <V> V update(final Callable<V> callable) {
        V result = null;
        try {
            // the engine may be null in debug scenarios
            if (engine == null) {
                result = callable.call();
            } else {
                result = engine.delayUpdatePropagation(callable);
            }
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }

        notifyBaseIndexChangeListeners(this.isDirty);

        return result;
    }

    private static void addType(final Class<?> key, final Class<?> value, final Map<Class<?>, Set<Class<?>>> map) {
        if (key == null) {
            throw new IllegalArgumentException("Key must not be null!");
        }
        map.compute(key, (k, v) -> {
            if (v == null) {
                v = new HashSet<>();
            }
            if (value != null) {
                v.add(value);
            }
            return v;
        });
    }

    public static void registerType(final Class<?> sub, final Class<?> sup) {
        // sub -> existing U {sup} into superType map
        addType(sub, sup, superTypeMap);

        if (sup != null) {
            // sup -> existing U {sub} into subType map
            addType(sup, sub, subTypeMap);
        }

        // add sup as supertype for all subtypes of sub
        final Set<Class<?>> subSubs = subTypeMap.get(sub);
        if (subSubs != null) {
            for (final Class<?> subSub : subSubs) {
                addType(subSub, sup, superTypeMap);
            }
        }

        if (sup != null) {
            // add sub as subtype for all supertypes of sup
            final Set<Class<?>> supSups = superTypeMap.get(sup);
            if (supSups != null) {
                for (final Class<?> supSup : supSups) {
                    addType(supSup, sub, subTypeMap);
                }
            }
        }
    }

    public void insertNodeTypeInstance(final NodeType type, final Object instance) {
        this.nodeTypeInstances.compute(type, (k, v) -> {
            if (v == null) {
                v = new HashSet<>();
            }
            if (v.add(instance)) {
                notifyNodeTypeInstanceListeners(type, instance, true);
            } else {
                throw new RuntimeException("Already known  " + type + " instance: " + instance);
            }
            return v;
        });
    }

    public void deleteNodeTypeInstance(final NodeType type, final Object instance) {
        this.nodeTypeInstances.compute(type, (k, v) -> {
            if (v == null) {
                throw new RuntimeException("Unknown  " + type + " instance: " + instance);
            } else {
                if (v.remove(instance)) {
                    notifyNodeTypeInstanceListeners(type, instance, false);
                } else {
                    throw new RuntimeException("Unknown  " + type + " instance: " + instance);
                }
                if (v.isEmpty()) {
                    return null;
                } else {
                    return v;
                }
            }
        });
    }

    public void insertDataTypeInstance(final Object instance) {
        final DataType type = new DataType(instance.getClass());
        this.dataTypeInstances.compute(type, (k, v) -> {
            if (v == null) {
                v = HashMultiset.create();
            }
            final boolean isFirstOccurrence = !v.contains(instance);
            v.add(instance);
            if (isFirstOccurrence) {
                notifyDataTypeInstanceListeners(type, instance, true);
            }
            return v;
        });
    }

    public void deleteDataTypeInstance(final Object instance) {
        final DataType type = new DataType(instance.getClass());
        this.dataTypeInstances.compute(type, (k, v) -> {
            if (v == null) {
                throw new RuntimeException("Unknown  " + type + " instance: " + instance);
            } else {
                final boolean isLastOccurrence = v.count(instance) == 1;
                v.remove(instance);
                if (isLastOccurrence) {
                    notifyDataTypeInstanceListeners(type, instance, false);
                }
                if (v.isEmpty()) {
                    return null;
                } else {
                    return v;
                }
            }
        });
    }

    private void insertNodeLinkInstanceInternal(final Object source, final NodeLink link, final Object target,
                                                final Map<NodeLink, Map<Object, Set<Object>>> map, final boolean notifyAbout) {
        map.compute(link, (ok, ov) -> {
            if (ov == null) {
                ov = new HashMap<>();
            }
            ov.compute(source, (ik, iv) -> {
                if (iv == null) {
                    iv = new HashSet<>();
                }
                if (iv.add(target)) {
                    if (notifyAbout) {
                        notifyNodeLinkInstanceListeners(link, source, target, true);
                    }
                } else {
                    throw new RuntimeException("Already known  " + link + " instance: " + source + " -> " + target);
                }
                return iv;
            });
            return ov;
        });
    }

    public void insertNodeLinkInstance(final Object source, final NodeLink link, final Object target) {
        insertNodeLinkInstanceInternal(source, link, target, this.nodeLinkInstances, true);
        insertNodeLinkInstanceInternal(target, link, source, this.nodeLinkInstancesReversed, false);
    }

    private void deleteNodeLinkInstanceInternal(final Object source, final NodeLink link, final Object target,
                                                final Map<NodeLink, Map<Object, Set<Object>>> map, final boolean notifyAbout) {
        map.compute(link, (ok, ov) -> {
            if (ov == null) {
                throw new RuntimeException("Unknown  " + link + " instance: " + source + " -> " + target);
            }
            ov.compute(source, (ik, iv) -> {
                if (iv == null) {
                    throw new RuntimeException("Unknown  " + link + " instance: " + source + " -> " + target);
                }
                if (iv.remove(target)) {
                    if (notifyAbout) {
                        notifyNodeLinkInstanceListeners(link, source, target, false);
                    }
                } else {
                    throw new RuntimeException("Unknown  " + link + " instance: " + source + " -> " + target);
                }
                if (iv.isEmpty()) {
                    return null;
                } else {
                    return iv;
                }
            });
            if (ov.isEmpty()) {
                return null;
            } else {
                return ov;
            }
        });
    }

    public void deleteNodeLinkInstance(final Object source, final NodeLink link, final Object target) {
        deleteNodeLinkInstanceInternal(source, link, target, this.nodeLinkInstances, true);
        deleteNodeLinkInstanceInternal(target, link, source, this.nodeLinkInstancesReversed, false);
    }

    private void notifyNodeTypeInstanceListeners(final NodeType type, final Object instance, final boolean isInsertion) {
        final Set<INodeTypeInstanceListener> listeners =
                this.nodeTypeInstanceListeners.getOrDefault(type,
                        Collections.emptySet());
        isDirty |= !listeners.isEmpty();
        for (final INodeTypeInstanceListener listener : listeners) {
            if (isInsertion) {
                listener.insert(type, instance);
            } else {
                listener.delete(type, instance);
            }
        }
    }

    private void notifyDataTypeInstanceListeners(final DataType type, final Object instance, final boolean isInsertion) {
        final Set<IDataTypeInstanceListener> listeners =
                this.dataTypeInstanceListeners.getOrDefault(type,
                        Collections.emptySet());
        isDirty |= !listeners.isEmpty();
        for (final IDataTypeInstanceListener listener : listeners) {
            if (isInsertion) {
                listener.insert(type, instance);
            } else {
                listener.delete(type, instance);
            }
        }
    }

    private void notifyNodeLinkInstanceListeners(final NodeLink type, final Object source, Object target, final boolean isInsertion) {
        final Set<INodeLinkInstanceListener> listeners =
                this.nodeLinkInstanceListeners.getOrDefault(type,
                        Collections.emptySet());
        isDirty |= !listeners.isEmpty();
        for (final INodeLinkInstanceListener listener : listeners) {
            if (isInsertion) {
                listener.insert(type, source, target);
            } else {
                listener.delete(type, source, target);
            }
        }
    }

    private static class Change {

        private IInputKey key;
        private Tuple tuple;
        private boolean isInsertion;

        private Change(final IInputKey key, final Tuple tuple, final boolean isInsertion) {
            this.key = key;
            this.tuple = tuple;
            this.isInsertion = isInsertion;
        }

        private static Change insertion(final IInputKey key, final Tuple tuple) {
            return new Change(key, tuple, true);
        }

        private static Change deletion(final IInputKey key, final Tuple tuple) {
            return new Change(key, tuple, false);
        }

        @Override
        public int hashCode() {
            return Objects.hash(this.isInsertion, this.key, this.tuple);
        }

        @Override
        public boolean equals(final Object obj) {
            if (obj == null || obj.getClass() != this.getClass()) {
                return false;
            } else if (this == obj) {
                return true;
            } else {
                final Change that = (Change) obj;
                return this.isInsertion == that.isInsertion && this.key.equals(that.key) && this.tuple.equals(that.tuple);
            }
        }

        @Override
        public String toString() {
            return (this.isInsertion ? "+" : "-") + this.key + " -> " + this.tuple;
        }
    }

    @Override
    public <V> V coalesceTraversals(final Callable<V> callable) throws InvocationTargetException {
        V result = null;
        Exception e = null;
        try {
            result = callable.call();
        } catch (final Exception ex) {
            e = ex;
        }
        if (e != null) {
            throw new InvocationTargetException(e);
        } else {
            return result;
        }
    }

    private void notifyBaseIndexChangeListeners(final boolean baseIndexChanged) {
        for (ViatraBaseIndexChangeListener listener : this.changeListeners) {
            if (!listener.onlyOnIndexChange() || baseIndexChanged) {
                listener.notifyChanged(baseIndexChanged);
            }
        }
    }

    @Override
    public void addBaseIndexChangeListener(final ViatraBaseIndexChangeListener listener) {
        this.changeListeners.add(listener);
    }

    @Override
    public void removeBaseIndexChangeListener(final ViatraBaseIndexChangeListener listener) {
        this.changeListeners.remove(listener);
    }

    @Override
    public void resampleDerivedFeatures() {

    }

    @Override
    public boolean addIndexingErrorListener(final IIndexingErrorListener listener) {
        return false;
    }

    @Override
    public boolean removeIndexingErrorListener(final IIndexingErrorListener listener) {
        return false;
    }

    @Override
    public boolean addInstanceObserver(final IInstanceObserver observer, final Object observedObject) {
        return false;
    }

    @Override
    public boolean removeInstanceObserver(final IInstanceObserver observer, final Object observedObject) {
        return false;
    }

    private <K extends MetaElements.MetaElement, V extends IInstanceListener> void addInstanceListener(
            final K type,
            final V listener,
            final Map<K, Set<V>> listenerMap) {
        listenerMap.compute(type, (k, v) -> {
            if (v == null) {
                v = new HashSet<>();
            }
            v.add(listener);
            return v;
        });
    }

    private <K extends MetaElements.MetaElement, V extends IInstanceListener> void removeInstanceListener(
            final K type,
            final V listener,
            final Map<K, Set<V>> listenerMap) {
        listenerMap.compute(type, (k, v) -> {
            if (v == null) {
                throw new RuntimeException("No listeners registered for type " + type + "!");
            }
            v.remove(listener);
            if (v.isEmpty()) {
                return null;
            } else {
                return v;
            }
        });
    }

    void addNodeTypeInstanceListener(final NodeType type, final INodeTypeInstanceListener listener) {
        addInstanceListener(type, listener, this.nodeTypeInstanceListeners);
    }

    void removeNodeTypeInstanceListener(final NodeType type, final INodeTypeInstanceListener listener) {
        removeInstanceListener(type, listener, this.nodeTypeInstanceListeners);
    }

    void addDataTypeInstanceListener(final DataType type, final IDataTypeInstanceListener listener) {
        addInstanceListener(type, listener, this.dataTypeInstanceListeners);
    }

    void removeDataTypeInstanceListener(final DataType type, final IDataTypeInstanceListener listener) {
        removeInstanceListener(type, listener, this.dataTypeInstanceListeners);
    }

    void addNodeLinkInstanceListener(final NodeLink type, final INodeLinkInstanceListener listener) {
        addInstanceListener(type, listener, this.nodeLinkInstanceListeners);
    }

    void removedNodeLinkInstanceListener(final NodeLink type, final INodeLinkInstanceListener listener) {
        removeInstanceListener(type, listener, this.nodeLinkInstanceListeners);
    }

}
