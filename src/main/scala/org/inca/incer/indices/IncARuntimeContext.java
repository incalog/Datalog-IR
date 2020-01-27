package org.inca.incer.indices;

import org.eclipse.viatra.query.runtime.matchers.context.*;
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey;
import org.eclipse.viatra.query.runtime.matchers.tuple.ITuple;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import org.eclipse.viatra.query.runtime.matchers.tuple.TupleMask;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.eclipse.viatra.query.runtime.matchers.util.Accuracy;
import org.inca.meta.MetaElements.DataType;
import org.inca.meta.MetaElements.NodeLink;
import org.inca.meta.MetaElements.NodeType;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;

public class IncARuntimeContext implements IQueryRuntimeContext {

    public final Map<NodeType, Set<Object>> nodeTypeInstances;
    public final Map<DataType, Set<Object>> dataTypeInstances;

    // source -> {targets}
    public final Map<NodeLink, Map<Object, Set<Object>>> nodeLinkInstances;

    // target -> {sources}
    public final Map<NodeLink, Map<Object, Set<Object>>> nodeLinkInstancesReversed;

    public static final Map<Class<?>, Set<Class<?>>> subTypeMap = new HashMap<>();
    public static final Map<Class<?>, Set<Class<?>>> superTypeMap = new HashMap<>();
    private final boolean isDebugMode;
    private IQueryMetaContext metaContext;

    /**
     * Remains null until we actually start listening to program changes.
     * This usually happens after the initalization of the indices.
     */
    private Set<Change> changeStore;

    public IncARuntimeContext() {
        this(false);
    }

    public IncARuntimeContext(final boolean isDebugMode) {
        this.nodeTypeInstances = new HashMap<>();
        this.dataTypeInstances = new HashMap<>();
        this.nodeLinkInstances = new HashMap<>();
        this.nodeLinkInstancesReversed = new HashMap<>();
        this.changeStore = new HashSet<>();
        this.isDebugMode = isDebugMode;
        this.metaContext = new IncAMetaContext();
    }

    public void dispose() {
        this.nodeTypeInstances.clear();
        this.dataTypeInstances.clear();
        this.nodeLinkInstances.clear();
        this.nodeLinkInstancesReversed.clear();
        if (this.changeStore != null) {
            this.changeStore.clear();
        }
        this.metaContext = null;
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
        insertInstance(type, instance, Change.insertion(new IncAInputKey.NodeTypeKey(type),
                Tuples.staticArityFlatTupleOf(instance)), this.nodeTypeInstances);
    }

    public void deleteNodeTypeInstance(final NodeType type, final Object instance) {
        deleteInstance(type, instance, Change.deletion(new IncAInputKey.NodeTypeKey(type),
                Tuples.staticArityFlatTupleOf(instance)), this.nodeTypeInstances);
    }

    public void insertDataTypeInstance(final Object instance) {
        final DataType type = new DataType(instance.getClass());
        insertInstance(type, instance, Change.insertion(new IncAInputKey.DataTypeKey(type),
                Tuples.staticArityFlatTupleOf(instance)), this.dataTypeInstances);
    }

    public void deleteDataTypeInstance(final Object instance) {
        final DataType type = new DataType(instance.getClass());
        deleteInstance(type, instance, Change.deletion(new IncAInputKey.DataTypeKey(type),
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
                        this.registerChange(Change.insertion(new IncAInputKey.NodeLinkKey(link),
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
                        this.registerChange(Change.deletion(new IncAInputKey.NodeLinkKey(link), Tuples.staticArityFlatTupleOf(source, target)));
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
    public IQueryMetaContext getMetaContext() {
        return this.metaContext;
    }

    @Override
    public <V> V coalesceTraversals(final Callable<V> callable) throws InvocationTargetException {
        try {
            return callable.call();
        } catch (final Exception e) {
            throw new InvocationTargetException(e);
        }
    }

    @Override
    public boolean isCoalescing() {
        return false;
    }

    @Override
    public boolean isIndexed(final IInputKey key, final IndexingService service) {
        return true;
    }

    @Override
    public void ensureIndexed(final IInputKey key, final IndexingService service) {

    }

    @Override
    public int countTuples(final IInputKey key, final TupleMask mask, final ITuple seed) {
        int result = 0;

        if (key instanceof IncAInputKey.NodeTypeKey) {
            final NodeType type = ((IncAInputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.nodeTypeInstances.getOrDefault(type, Collections.emptySet()).size();
            } else {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            }
        } else if (key instanceof IncAInputKey.DataTypeKey) {
            final DataType type = ((IncAInputKey.DataTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.dataTypeInstances.getOrDefault(type, Collections.emptySet()).size();
            } else {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            }
        } else if (key instanceof IncAInputKey.NodeLinkKey) {
            final NodeLink link = ((IncAInputKey.NodeLinkKey) key).type;

            boolean isSourceBound = false;
            int sourceIndex = -1;
            boolean isTargetBound = false;
            int targetIndex = -1;
            for (int i = 0; i < mask.getSize(); i++) {
                final int index = mask.indices[i];
                if (index == 0) {
                    isSourceBound = true;
                    sourceIndex = i;
                } else if (index == 1) {
                    isTargetBound = true;
                    targetIndex = i;
                }
            }

            if (!(isSourceBound) && isTargetBound) {
                final Object seedTarget = seed.get(targetIndex);
                final Map<Object, Set<Object>> linkValues = this.nodeLinkInstancesReversed.get(link);
                if (linkValues != null) {
                    result = linkValues.getOrDefault(seedTarget, Collections.emptySet()).size();
                }
            } else if (isSourceBound && isTargetBound) {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            } else if (!(isSourceBound) && !(isTargetBound)) {
                // fully unseeded
                final Map<Object, Set<Object>> linkValues = this.nodeLinkInstances.get(link);
                for (final Map.Entry<Object, Set<Object>> entry : linkValues.entrySet()) {
                    result += entry.getValue().size();
                }
            } else if (isSourceBound && !(isTargetBound)) {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Set<Object>> linkValues = this.nodeLinkInstances.get(link);
                if (linkValues != null) {
                    result = linkValues.getOrDefault(seedSource, Collections.emptySet()).size();
                }
            }
        }

        if (this.isDebugMode) {
            System.out.println("countTuples key: " + key + " tuple: " + seed + " result: " + result);
        }

        return result;
    }

    @Override
    public Optional<Long> estimateCardinality(final IInputKey key, final TupleMask mask, final Accuracy requiredAccuracy) {
        return Optional.empty();
    }

    @Override
    public Iterable<Tuple> enumerateTuples(final IInputKey key, final TupleMask mask, final ITuple seed) {
        final Collection<Tuple> result = new HashSet<>();

        if (key instanceof IncAInputKey.NodeTypeKey) {
            final NodeType type = ((IncAInputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                this.nodeTypeInstances.getOrDefault(type, Collections.emptySet()).forEach(e -> result.add(Tuples.staticArityFlatTupleOf(e)));
            } else {
                final Object seedInstance = mask.getValue(seed, 0);
                if (containsTuple(key, seed)) {
                    result.add(Tuples.staticArityFlatTupleOf(seedInstance));
                }
            }
        } else if (key instanceof IncAInputKey.DataTypeKey) {
            final DataType type = ((IncAInputKey.DataTypeKey) key).type;

            if (mask.indices.length == 0) {
                this.dataTypeInstances.getOrDefault(type, Collections.emptySet()).forEach(e -> result.add(Tuples.staticArityFlatTupleOf(e)));
            } else {
                final Object seedInstance = mask.getValue(seed, 0);
                if (containsTuple(key, seed)) {
                    result.add(Tuples.staticArityFlatTupleOf(seedInstance));
                }
            }
        } else if (key instanceof IncAInputKey.NodeLinkKey) {
            final NodeLink link = ((IncAInputKey.NodeLinkKey) key).type;

            boolean isSourceBound = false;
            int sourceIndex = -1;
            boolean isTargetBound = false;
            int targetIndex = -1;
            for (int i = 0; i < mask.getSize(); i++) {
                final int index = mask.indices[i];
                if (index == 0) {
                    isSourceBound = true;
                    sourceIndex = i;
                } else if (index == 1) {
                    isTargetBound = true;
                    targetIndex = i;
                }
            }

            if (!(isSourceBound) && isTargetBound) {
                final Object seedTarget = seed.get(targetIndex);
                final Map<Object, Set<Object>> linkValues = this.nodeLinkInstancesReversed.get(link);
                if (linkValues != null) {
                    linkValues.getOrDefault(seedTarget, Collections.emptySet()).forEach(source -> result.add(Tuples.staticArityFlatTupleOf(source, seedTarget)));
                }
            } else if (isSourceBound && isTargetBound) {
                // fully seeded
                final Object seedSource = seed.get(sourceIndex);
                final Object seedTarget = seed.get(targetIndex);
                if (containsTuple(key, seed)) {
                    result.add(Tuples.staticArityFlatTupleOf(seedSource, seedTarget));
                }
            } else if (!(isSourceBound) && !(isTargetBound)) {
                // fully unseeded
                final Map<Object, Set<Object>> linkValues = this.nodeLinkInstances.get(link);
                if (linkValues != null) {
                    for (final Map.Entry<Object, Set<Object>> entry : linkValues.entrySet()) {
                        for (final Object target : entry.getValue()) {
                            result.add(Tuples.staticArityFlatTupleOf(entry.getKey(), target));
                        }
                    }
                }
            } else if (isSourceBound && !(isTargetBound)) {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Set<Object>> linkValues = this.nodeLinkInstances.get(link);
                if (linkValues != null) {
                    linkValues.getOrDefault(seedSource, Collections.emptySet()).forEach(target -> result.add(Tuples.staticArityFlatTupleOf(seedSource, target)));
                }
            }
        }

        if (this.isDebugMode) {
            System.out.println("enumerateTuples key: " + key + " tuple: " + seed + " result size: " + result.size());
        }

        return result;
    }

    @Override
    public Iterable<? extends Object> enumerateValues(final IInputKey key, final TupleMask mask, final ITuple seed) {
        Collection result = null;

        if (key instanceof IncAInputKey.NodeTypeKey) {
            final NodeType type = ((IncAInputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.nodeTypeInstances.get(type);
            } else {
                // must be unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        } else if (key instanceof IncAInputKey.DataTypeKey) {
            final DataType type = ((IncAInputKey.DataTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.dataTypeInstances.get(type);
            } else {
                // must be unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        } else if (key instanceof IncAInputKey.NodeLinkKey) {
            final NodeLink link = ((IncAInputKey.NodeLinkKey) key).type;

            boolean isSourceBound = false;
            int sourceIndex = -1;
            boolean isTargetBound = false;
            int targetIndex = -1;
            for (int i = 0; i < mask.getSize(); i++) {
                final int index = mask.indices[i];
                if (index == 0) {
                    isSourceBound = true;
                    sourceIndex = i;
                } else if (index == 1) {
                    isTargetBound = true;
                    targetIndex = i;
                }
            }

            if (!(isSourceBound) && isTargetBound) {
                final Object seedTarget = seed.get(targetIndex);
                final Map<Object, Set<Object>> linkValues = this.nodeLinkInstancesReversed.get(link);
                if (linkValues != null) {
                    result = linkValues.get(seedTarget);
                }
            } else if (isSourceBound && !(isTargetBound)) {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Set<Object>> linkValues = this.nodeLinkInstances.get(link);
                if (linkValues != null) {
                    result = linkValues.get(seedSource);
                }
            } else {
                // must be singly unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        }

        if (this.isDebugMode) {
            System.out.println("enumerateValues key: " + key + " tuple: " + seed + " result size: " + ((result == null ? "null" : result.size())) + " result: " + result);
        }

        return (Iterable<?>) result;
    }

    public void illegalEnumerateValues(final ITuple seed) {
        throw new IllegalArgumentException("Must have exactly one unseeded element in enumerateValues() invocation, received instead: " + seed);
    }

    @Override
    public boolean containsTuple(final IInputKey key, final ITuple tuple) {
        boolean result = false;

        if (key instanceof JavaTransitiveInstancesKey) {
            final Class<?> clazz = forceGetWrapperInstanceClass(((JavaTransitiveInstancesKey) key));
            if (clazz != null) {
                result = clazz.isInstance(getFromTuple(tuple, 0));
            } else {
                result = false;
            }
        } else if (key instanceof IncAInputKey.NodeTypeKey) {
            final NodeType type = ((IncAInputKey.NodeTypeKey) key).type;
            result = this.nodeTypeInstances.getOrDefault(type, Collections.emptySet()).contains(getFromTuple(tuple, 0));
        } else if (key instanceof IncAInputKey.DataTypeKey) {
            final DataType type = ((IncAInputKey.DataTypeKey) key).type;
            result = this.dataTypeInstances.getOrDefault(type, Collections.emptySet()).contains(getFromTuple(tuple, 0));
        } else if (key instanceof IncAInputKey.NodeLinkKey) {
            final NodeLink link = ((IncAInputKey.NodeLinkKey) key).type;
            final Map<Object, Set<Object>> linkValues = this.nodeLinkInstances.get(link);
            if (linkValues != null) {
                final Object source = getFromTuple(tuple, 1);
                final Object target = getFromTuple(tuple, 0);
                result = linkValues.getOrDefault(source, Collections.emptySet()).contains(target);
            }
        }

        if (this.isDebugMode) {
            System.out.println("containsTuple key: " + key + " tuple: " + tuple + " result: " + result);
        }

        return result;
    }

    private Object getFromTuple(final ITuple tuple, final int index) {
        return (tuple == null ? null : tuple.get(index));
    }

    private Class<?> forceGetWrapperInstanceClass(final JavaTransitiveInstancesKey key) {
        Class<?> instanceClass = null;
        try {
            instanceClass = key.forceGetWrapperInstanceClass();
        } catch (final ClassNotFoundException e) {
            throw new RuntimeException("Could not load instance class for type constraint " + key.getWrappedKey(), e);
        }
        return instanceClass;
    }

    @Override
    public void addUpdateListener(final IInputKey key, final Tuple seed, final IQueryRuntimeContextListener listener) {

    }

    @Override
    public void removeUpdateListener(final IInputKey key, final Tuple seed, final IQueryRuntimeContextListener listener) {

    }

    @Override
    public Object wrapElement(final Object element) {
        return element;
    }

    @Override
    public Object unwrapElement(final Object element) {
        return element;
    }

    @Override
    public Tuple wrapTuple(final Tuple tuple) {
        return tuple;
    }

    @Override
    public Tuple unwrapTuple(final Tuple tuple) {
        return tuple;
    }

    @Override
    public void ensureWildcardIndexing(final IndexingService service) {

    }

    @Override
    public void executeAfterTraversal(final Runnable runnable) throws InvocationTargetException {

    }

}
