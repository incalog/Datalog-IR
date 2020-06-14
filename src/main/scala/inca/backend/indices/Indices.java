package inca.backend.indices;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import inca.MetaElements;
import inca.backend.virtual.VirtualIndex;
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.scope.IBaseIndex;
import org.eclipse.viatra.query.runtime.api.scope.IIndexingErrorListener;
import org.eclipse.viatra.query.runtime.api.scope.IInstanceObserver;
import org.eclipse.viatra.query.runtime.api.scope.ViatraBaseIndexChangeListener;
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import inca.backend.listeners.IDataTypeInstanceListener;
import inca.backend.listeners.IInstanceListener;
import inca.backend.listeners.INodeLinkInstanceListener;
import inca.backend.listeners.INodeTypeInstanceListener;
import inca.MetaElements.DataType;
import inca.MetaElements.Link;
import inca.MetaElements.NodeType;
import scala.Tuple2;
import scala.collection.Iterator;
import truechange.*;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;

public class Indices implements IBaseIndex {

    final Map<NodeType, Set<Object>> nodeTypeInstances;
    final Map<NodeType, Set<INodeTypeInstanceListener>> nodeTypeInstanceListeners;

    final Map<DataType, Multiset<Object>> dataTypeInstances;
    final Map<DataType, Set<IDataTypeInstanceListener>> dataTypeInstanceListeners;

    // source -> {targets}
    final Map<Link, Map<Object, Set<Object>>> nodeLinkInstances;
    // target -> {sources}
    final Map<Link, Map<Object, Set<Object>>> nodeLinkInstancesReversed;
    final Map<Link, Set<INodeLinkInstanceListener>> nodeLinkInstanceListeners;

    // TODO we need to populate these maps somehow
    public static final Map<Class<?>, Set<Class<?>>> subTypeMap = new HashMap<>();
    public static final Map<Class<?>, Set<Class<?>>> superTypeMap = new HashMap<>();

//    public ParentIndex parentIndex;

    final Set<VirtualIndex> virtualIndices;

    private final Set<ViatraBaseIndexChangeListener> changeListeners;
    private AdvancedViatraQueryEngine engine;

    private boolean isDirty;

    public Indices() {
        this(null, Collections.emptySet());
    }

    public Indices(final AdvancedViatraQueryEngine engine, final Set<VirtualIndex> virtualIndices) {
        this.nodeTypeInstances = new HashMap<>();
        this.nodeTypeInstanceListeners = new HashMap<>();
        this.dataTypeInstances = new HashMap<>();
        this.dataTypeInstanceListeners = new HashMap<>();
        this.nodeLinkInstances = new HashMap<>();
        this.nodeLinkInstancesReversed = new HashMap<>();
        this.nodeLinkInstanceListeners = new HashMap<>();
        this.changeListeners = new HashSet<>();
//        this.parentIndex = new ParentIndex();
        this.virtualIndices = virtualIndices;
        this.engine = engine;
    }

    public void processChangeset(Changeset changeset) {
        Iterator<truechange.Change> changesetIterator = changeset.cmds().iterator();
        while (changesetIterator.hasNext()) {
            truechange.Change change = changesetIterator.next();
            // process virtual indices
            virtualIndices.forEach((vIndex) -> {
                vIndex.processChange(change);
            });
            if (change instanceof DetachNode) {
                // delete nodeLinkInstance
                DetachNode detach = (DetachNode) change;
                deleteNodeLinkInstance(detach.parent(), convertLinkToNodeLink(detach.link()), detach.node());
            } else if (change instanceof UnloadNode) {
                UnloadNode unload = (UnloadNode) change;
                // insert nodeTypeInstance
                NodeType nodeType = convertNodeTagToNodeType(unload.tag());
                deleteNodeTypeInstance(nodeType, unload.node());
                // delete for parent types
                Set<Class<?>> supertypes = superTypeMap.getOrDefault(nodeType.cls(), Collections.emptySet());
                for (Class<?> supertype : supertypes) {
                    NodeType nodeSupertype = new NodeType(supertype);
                    deleteNodeTypeInstance(nodeSupertype, unload.node());
                }
                // delete nodeLinkInstance for each kid
                Iterator<Tuple2<String, NodeURI>> kidsIterator = unload.kids().iterator();
                while(kidsIterator.hasNext()) {
                    Tuple2<String, NodeURI> kid = kidsIterator.next();
                    deleteNodeLinkInstance(unload.node(), convertNodeAndStringToNodeLink(unload.tag(), kid._1), kid._2);
                }
                // delete dataTypeInstance for each lit
                // delete nodeLinkInstances for each lit
                Iterator<Tuple2<String, Literal<?>>> litsIterator = unload.lits().iterator();
                while(litsIterator.hasNext()) {
                    Tuple2<String, Literal<?>> lit = litsIterator.next();
                    // TODO do we want to pass the literal or the value that the literal wraps?
                    deleteDataTypeInstance(lit._2.value());
                    deleteNodeLinkInstance(unload.node(), convertNodeAndStringToNodeLink(unload.tag(), lit._1), lit._2.value());
                }
            } else if (change instanceof AttachNode) {
                // insert nodeLinkInstance
                AttachNode attach = (AttachNode) change;
                insertNodeLinkInstance(attach.parent(), convertLinkToNodeLink(attach.link()), attach.node());
            } else if (change instanceof LoadNode) {
                // insert nodeTypeInstance
                LoadNode load = (LoadNode) change;
                NodeType nodeType = convertNodeTagToNodeType(load.tag());
                insertNodeTypeInstance(nodeType, load.node());
                // insert for every parent type
                Set<Class<?>> supertypes = superTypeMap.getOrDefault(nodeType.cls(), Collections.emptySet());
                for (Class<?> supertype : supertypes) {
                    NodeType nodeSupertype = new NodeType(supertype);
                    insertNodeTypeInstance(nodeSupertype, load.node());
                }
                Iterator<Tuple2<String, NodeURI>> kidsIterator = load.kids().iterator();
                while(kidsIterator.hasNext()) {
                    Tuple2<String, NodeURI> kid = kidsIterator.next();
                    insertNodeLinkInstance(load.node(), convertNodeAndStringToNodeLink(load.tag(), kid._1), kid._2);
                }
                Iterator<Tuple2<String, Literal<?>>> litsIterator = load.lits().iterator();
                while(litsIterator.hasNext()) {
                    Tuple2<String, Literal<?>> lit = litsIterator.next();
                    // TODO do we want to pass the literal or the value that the literal wraps?
                    insertDataTypeInstance(lit._2.value());
                    insertNodeLinkInstance(load.node(), convertNodeAndStringToNodeLink(load.tag(), lit._1), lit._2.value());
                }
            }
        }
    }

	private NodeType convertNodeTagToNodeType(truechange.Type type) {
        if (type instanceof SortType) {
            SortType stype = (SortType) type;
            return new NodeType(stype.tag());
        } else if (type instanceof ListType) {
            ListType ltype = (ListType) type;
            return convertNodeTagToNodeType(ltype.ty());
        }
        // TODO implement more
        return null;
    }
    private MetaElements.Link convertNodeAndStringToNodeLink(truechange.Type type, String linkName) {
       NodeType nodeType = convertNodeTagToNodeType(type);
       return nodeType.apply(linkName);
    }

    private MetaElements.Link convertLinkToNodeLink(truechange.Link link) {
        if (link instanceof NamedLink) {
            NamedLink nlink = (NamedLink) link;
            return convertNodeTagToNodeType(nlink.tag()).apply(nlink.name());
        }
        // TODO implement more
        return null;
    }

    private DataType convertLiteralToDataType(Literal literal) {
        return new DataType(literal.tag());
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
        this.virtualIndices.clear();
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

        final boolean[] virtualIsDirty = {false};
        virtualIndices.forEach((vIndex) -> {
            virtualIsDirty[0] |= vIndex.isDirty();
        });

        notifyBaseIndexChangeListeners(this.isDirty || virtualIsDirty[0]);

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
            v.add(instance);
            notifyNodeTypeInstanceListeners(type, instance, true);
//            } else {
//                throw new RuntimeException("Already known  " + type + " instance: " + instance);
//            }
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

    private void insertNodeLinkInstanceInternal(final Object source, final Link link, final Object target,
                                                final Map<Link, Map<Object, Set<Object>>> map, final boolean notifyAbout) {
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
                }
                else {
                    throw new RuntimeException("Already known  " + link + " instance: " + source + " -> " + target);
                }
                return iv;
            });
            return ov;
        });
    }

    public void insertNodeLinkInstance(final Object source, final Link link, final Object target) {
        insertNodeLinkInstanceInternal(source, link, target, this.nodeLinkInstances, true);
        insertNodeLinkInstanceInternal(target, link, source, this.nodeLinkInstancesReversed, false);
    }

    private void deleteNodeLinkInstanceInternal(final Object source, final Link link, final Object target,
                                                final Map<Link, Map<Object, Set<Object>>> map, final boolean notifyAbout) {
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

    public void deleteNodeLinkInstance(final Object source, final Link link, final Object target) {
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

    private void notifyNodeLinkInstanceListeners(final Link type, final Object source, Object target, final boolean isInsertion) {
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

    private <K, V extends IInstanceListener> void addInstanceListener(
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

    private <K, V extends IInstanceListener> void removeInstanceListener(
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

    void addNodeLinkInstanceListener(final Link type, final INodeLinkInstanceListener listener) {
        addInstanceListener(type, listener, this.nodeLinkInstanceListeners);
    }

    void removedNodeLinkInstanceListener(final Link type, final INodeLinkInstanceListener listener) {
        removeInstanceListener(type, listener, this.nodeLinkInstanceListeners);
    }
}
