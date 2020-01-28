package org.inca.incer.indices;

import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.scope.IBaseIndex;
import org.eclipse.viatra.query.runtime.api.scope.IIndexingErrorListener;
import org.eclipse.viatra.query.runtime.api.scope.IInstanceObserver;
import org.eclipse.viatra.query.runtime.api.scope.ViatraBaseIndexChangeListener;
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.inca.incer.Incrementalizable;
import org.inca.meta.MetaElements.DataType;
import org.inca.meta.MetaElements.NodeLink;
import org.inca.meta.MetaElements.NodeType;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;

public class Indices implements IBaseIndex {

    final Map<NodeType, Set<Object>> nodeTypeInstances;
    final Map<DataType, Set<Object>> dataTypeInstances;

    // source -> {targets}
    final Map<NodeLink, Map<Object, Set<Object>>> nodeLinkInstances;

    // target -> {sources}
    final Map<NodeLink, Map<Object, Set<Object>>> nodeLinkInstancesReversed;

    public static final Map<Class<?>, Set<Class<?>>> subTypeMap = new HashMap<>();
    public static final Map<Class<?>, Set<Class<?>>> superTypeMap = new HashMap<>();

    /**
     * Remains null until we actually start listening to program changes.
     * This usually happens after the initialization of the indices.
     */
    private Set<Change> changeStore;

    private AdvancedViatraQueryEngine engine;

    public Indices() {
        this(null);
    }

    public Indices(final AdvancedViatraQueryEngine engine) {
        this.nodeTypeInstances = new HashMap<>();
        this.dataTypeInstances = new HashMap<>();
        this.nodeLinkInstances = new HashMap<>();
        this.nodeLinkInstancesReversed = new HashMap<>();
        this.changeStore = new HashSet<>();
        this.engine = engine;
    }

    public void initializeWith(final Incrementalizable root) {
        root.insert(this);
    }

    public void dispose() {
        this.nodeTypeInstances.clear();
        this.dataTypeInstances.clear();
        this.nodeLinkInstances.clear();
        this.nodeLinkInstancesReversed.clear();
        if (this.changeStore != null) {
            this.changeStore.clear();
        }
        this.engine = null;
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
        insertInstance(type, instance, Change.insertion(new TFInputKey.NodeTypeKey(type),
                Tuples.staticArityFlatTupleOf(instance)), this.nodeTypeInstances);
    }

    public void deleteNodeTypeInstance(final NodeType type, final Object instance) {
        deleteInstance(type, instance, Change.deletion(new TFInputKey.NodeTypeKey(type),
                Tuples.staticArityFlatTupleOf(instance)), this.nodeTypeInstances);
    }

    public void insertDataTypeInstance(final Object instance) {
        final DataType type = new DataType(instance.getClass());
        insertInstance(type, instance, Change.insertion(new TFInputKey.DataTypeKey(type),
                Tuples.staticArityFlatTupleOf(instance)), this.dataTypeInstances);
    }

    public void deleteDataTypeInstance(final Object instance) {
        final DataType type = new DataType(instance.getClass());
        deleteInstance(type, instance, Change.deletion(new TFInputKey.DataTypeKey(type),
                Tuples.staticArityFlatTupleOf(instance)), this.dataTypeInstances);
    }

    private void insertNodeLinkInstanceInternal(final Object source, final NodeLink link, final Object target,
                                                final Map<NodeLink, Map<Object, Set<Object>>> map, final boolean registerChange) {
        map.compute(link, (ok, ov) -> {
            if (ov == null) {
                ov = new HashMap<>();
            }
            ov.compute(source, (ik, iv) -> {
                if (iv == null) {
                    iv = new HashSet<>();
                }
                if (iv.add(target)) {
                    if (registerChange) {
                        this.registerChange(Change.insertion(new TFInputKey.NodeLinkKey(link),
                                Tuples.staticArityFlatTupleOf(source, target)));
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
                                                final Map<NodeLink, Map<Object, Set<Object>>> map, final boolean registerChange) {
        map.compute(link, (ok, ov) -> {
            if (ov == null) {
                throw new RuntimeException("Unknown  " + link + " instance: " + source + " -> " + target);
            }
            ov.compute(source, (ik, iv) -> {
                if (iv == null) {
                    throw new RuntimeException("Unknown  " + link + " instance: " + source + " -> " + target);
                }
                if (iv.remove(target)) {
                    if (registerChange) {
                        this.registerChange(Change.deletion(new TFInputKey.NodeLinkKey(link), Tuples.staticArityFlatTupleOf(source, target)));
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

    private <T> void insertInstance(final T type, final Object instance, final Change change,
                                    Map<T, Set<Object>> instanceMap) {
        instanceMap.compute(type, (k, v) -> {
            if (v == null) {
                v = new HashSet<>();
            }
            if (v.add(instance)) {
                this.registerChange(change);
            } else {
                throw new RuntimeException("Already known  " + type + " instance: " + instance);
            }
            return v;
        });
    }

    private <T> void deleteInstance(final T type, final Object instance, final Change change,
                                    Map<T, Set<Object>> instanceMap) {
        instanceMap.compute(type, (k, v) -> {
            if (v == null) {
                throw new RuntimeException("Unknown  " + type + " instance: " + instance);
            } else {
                if (v.remove(instance)) {
                    this.registerChange(change);
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

    private void registerChange(final Change change) {
        if (this.changeStore != null) {
            this.changeStore.add(change);
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

    @Override
    public void addBaseIndexChangeListener(final ViatraBaseIndexChangeListener listener) {

    }

    @Override
    public void removeBaseIndexChangeListener(final ViatraBaseIndexChangeListener listener) {

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

}
