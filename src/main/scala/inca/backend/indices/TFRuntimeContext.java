package inca.backend.indices;

import com.google.common.collect.Multiset;
import inca.backend.virtual.VirtualIndex;
import org.eclipse.viatra.query.runtime.matchers.context.*;
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey;
import org.eclipse.viatra.query.runtime.matchers.tuple.ITuple;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import org.eclipse.viatra.query.runtime.matchers.tuple.TupleMask;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.eclipse.viatra.query.runtime.matchers.util.Accuracy;
import inca.backend.listeners.DataTypeInstanceAdapter;
import inca.backend.listeners.NodeLinkInstanceAdapter;
import inca.backend.listeners.NodeTypeInstanceAdapter;
import inca.MetaElements;
import inca.MetaElements.DataType;
import inca.MetaElements.Link;
import inca.MetaElements.NodeType;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;
import scala.jdk.CollectionConverters.*;
import scala.jdk.javaapi.CollectionConverters;

public class TFRuntimeContext implements IQueryRuntimeContext {

    private final Indices indices;
    private final boolean isDebugMode;

    public TFRuntimeContext(final Indices indices) {
        this(indices, false);
    }

    public TFRuntimeContext(final Indices indices, final boolean isDebugMode) {
        this.indices = indices;
        this.isDebugMode = isDebugMode;
    }

    @Override
    public IQueryMetaContext getMetaContext() {
        return TFMetaContext.INSTANCE;
    }

    @Override
    public <V> V coalesceTraversals(final Callable<V> callable) throws InvocationTargetException {
        return this.indices.coalesceTraversals(callable);
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

        if (key instanceof TFInputKey.NodeTypeKey) {
            final NodeType type = ((TFInputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.indices.nodeTypeInstances.getOrDefault(type, Collections.emptySet()).size();
            } else {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            }
        } else if (key instanceof TFInputKey.DataTypeKey) {
            final DataType type = ((TFInputKey.DataTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                final Multiset<Object> instances = this.indices.dataTypeInstances.get(type);
                if (instances != null) {
                    result = instances.elementSet().size();
                }
            } else {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            }
        } else if (key instanceof TFInputKey.NodeLinkKey) {
            final Link link = ((TFInputKey.NodeLinkKey) key).type;

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
                final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstancesReversed.get(link);
                if (linkValues != null) {
                    result = linkValues.getOrDefault(seedTarget, Collections.emptySet()).size();
                }
            } else if (isSourceBound && isTargetBound) {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            } else if (!(isSourceBound) /*&& !(isTargetBound)*/) {
                // fully unseeded
                final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstances.get(link);
                for (final Map.Entry<Object, Set<Object>> entry : linkValues.entrySet()) {
                    result += entry.getValue().size();
                }
            } else /*if (isSourceBound && !(isTargetBound))*/ {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstances.get(link);
                if (linkValues != null) {
                    result = linkValues.getOrDefault(seedSource, Collections.emptySet()).size();
                }
            }
        } else {
            for (VirtualIndex vIndex : indices.virtualIndices) {
                if (vIndex.isSupported(key)) {
                    result = vIndex.countTuples(mask, seed);
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

        if (key instanceof TFInputKey.NodeTypeKey) {
            final NodeType type = ((TFInputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                this.indices.nodeTypeInstances.getOrDefault(type, Collections.emptySet()).forEach(e -> result.add(Tuples.staticArityFlatTupleOf(e)));
            } else {
                final Object seedInstance = mask.getValue(seed, 0);
                if (containsTuple(key, seed)) {
                    result.add(Tuples.staticArityFlatTupleOf(seedInstance));
                }
            }
        } else if (key instanceof TFInputKey.DataTypeKey) {
            final DataType type = ((TFInputKey.DataTypeKey) key).type;

            if (mask.indices.length == 0) {
                final Multiset<Object> instances = this.indices.dataTypeInstances.get(type);
                if (instances != null) {
                    instances.elementSet().forEach(e -> result.add(Tuples.staticArityFlatTupleOf(e)));
                }
            } else {
                final Object seedInstance = mask.getValue(seed, 0);
                if (containsTuple(key, seed)) {
                    result.add(Tuples.staticArityFlatTupleOf(seedInstance));
                }
            }
        } else if (key instanceof TFInputKey.NodeLinkKey) {
            final Link link = ((TFInputKey.NodeLinkKey) key).type;

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
                final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstancesReversed.get(link);
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
            } else if (!(isSourceBound) /*&& !(isTargetBound)*/) {
                // fully unseeded
                final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstances.get(link);
                if (linkValues != null) {
                    for (final Map.Entry<Object, Set<Object>> entry : linkValues.entrySet()) {
                        for (final Object target : entry.getValue()) {
                            result.add(Tuples.staticArityFlatTupleOf(entry.getKey(), target));
                        }
                    }
                }
            } else /*if (isSourceBound && !(isTargetBound))*/ {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstances.get(link);
                if (linkValues != null) {
                    linkValues.getOrDefault(seedSource, Collections.emptySet()).forEach(target -> result.add(Tuples.staticArityFlatTupleOf(seedSource, target)));
                }
            }
        } else {
            for (VirtualIndex vIndex : indices.virtualIndices) {
                if (vIndex.isSupported(key)) {
                    result.addAll(CollectionConverters.asJavaCollection(vIndex.enumerateTuples(mask, seed)));
                }
            }
        }

        if (this.isDebugMode) {
            System.out.println("enumerateTuples key: " + key + " tuple: " + seed + " result size: " + result.size());
        }

        return result;
    }

    @Override
    public Iterable<?> enumerateValues(final IInputKey key, final TupleMask mask, final ITuple seed) {
        Collection<?> result = null;

        if (key instanceof TFInputKey.NodeTypeKey) {
            final NodeType type = ((TFInputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.indices.nodeTypeInstances.get(type);
            } else {
                // must be unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        } else if (key instanceof TFInputKey.DataTypeKey) {
            final DataType type = ((TFInputKey.DataTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.indices.dataTypeInstances.get(type);
            } else {
                // must be unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        } else if (key instanceof TFInputKey.NodeLinkKey) {
            final Link link = ((TFInputKey.NodeLinkKey) key).type;

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
                final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstancesReversed.get(link);
                if (linkValues != null) {
                    result = linkValues.get(seedTarget);
                }
            } else if (isSourceBound && !(isTargetBound)) {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstances.get(link);
                if (linkValues != null) {
                    result = linkValues.get(seedSource);
                }
            } else {
                // must be singly unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        } else {
            for (VirtualIndex vIndex : indices.virtualIndices) {
                if (vIndex.isSupported(key)) {
                    result = CollectionConverters.asJavaCollection(vIndex.enumerateValues(mask, seed));
                }
            }
        }

        if (this.isDebugMode) {
            System.out.println("enumerateValues key: " + key + " tuple: " + seed + " result size: " + ((result == null ? "null" : result.size())) + " result: " + result);
        }

        return result;
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
        } else if (key instanceof TFInputKey.NodeTypeKey) {
            final NodeType type = ((TFInputKey.NodeTypeKey) key).type;
            result = this.indices.nodeTypeInstances.getOrDefault(type, Collections.emptySet()).contains(getFromTuple(tuple, 0));
        } else if (key instanceof TFInputKey.DataTypeKey) {
            final DataType type = ((TFInputKey.DataTypeKey) key).type;
            final Multiset<Object> instances = this.indices.dataTypeInstances.get(type);
            if (instances != null) {
                result = instances.contains(getFromTuple(tuple, 0));
            }
        } else if (key instanceof TFInputKey.NodeLinkKey) {
            final Link link = ((TFInputKey.NodeLinkKey) key).type;
            final Map<Object, Set<Object>> linkValues = this.indices.nodeLinkInstances.get(link);
            if (linkValues != null) {
                final Object source = getFromTuple(tuple, 1);
                final Object target = getFromTuple(tuple, 0);
                result = linkValues.getOrDefault(source, Collections.emptySet()).contains(target);
            }
        } else {
            for (VirtualIndex vIndex : indices.virtualIndices) {
                if (vIndex.isSupported(key)) {
                    result = vIndex.containsTuple(tuple);
                }
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
        Class<?> instanceClass;
        try {
            instanceClass = key.forceGetWrapperInstanceClass();
        } catch (final ClassNotFoundException e) {
            throw new RuntimeException("Could not load instance class for type constraint " + key.getWrappedKey(), e);
        }
        return instanceClass;
    }

    @Override
    public void addUpdateListener(final IInputKey key, final Tuple seed, final IQueryRuntimeContextListener listener) {
        if (key instanceof JavaTransitiveInstancesKey) {
            // stateless, so NOP
        } else if (key instanceof TFInputKey.NodeTypeKey) {
            final NodeType type = ((TFInputKey.NodeTypeKey) key).type;
            this.indices.addNodeTypeInstanceListener(type, new NodeTypeInstanceAdapter(listener, seed.get(0)));
        } else if (key instanceof TFInputKey.DataTypeKey) {
            final DataType type = ((TFInputKey.DataTypeKey) key).type;
            this.indices.addDataTypeInstanceListener(type, new DataTypeInstanceAdapter(listener, seed.get(0)));
        } else if (key instanceof TFInputKey.NodeLinkKey) {
            final Link type = ((TFInputKey.NodeLinkKey) key).type;
            this.indices.addNodeLinkInstanceListener(type, new NodeLinkInstanceAdapter(listener, seed.get(0), seed.get(1)));
        } else {
            indices.virtualIndices.forEach((vIndex) -> {
                if (vIndex.isSupported(key)) {
                    vIndex.addListener(listener, seed);
                }
            });
        }
    }

    @Override
    public void removeUpdateListener(final IInputKey key, final Tuple seed, final IQueryRuntimeContextListener listener) {
        if (key instanceof JavaTransitiveInstancesKey) {
            // stateless, so NOP
        } else if (key instanceof TFInputKey.NodeTypeKey) {
            final NodeType type = ((TFInputKey.NodeTypeKey) key).type;
            this.indices.removeNodeTypeInstanceListener(type, new NodeTypeInstanceAdapter(listener, seed.get(0)));
        } else if (key instanceof TFInputKey.DataTypeKey) {
            final DataType type = ((TFInputKey.DataTypeKey) key).type;
            this.indices.removeDataTypeInstanceListener(type, new DataTypeInstanceAdapter(listener, seed.get(0)));
        } else if (key instanceof TFInputKey.NodeLinkKey) {
            final MetaElements.Link type = ((TFInputKey.NodeLinkKey) key).type;
            this.indices.removedNodeLinkInstanceListener(type, new NodeLinkInstanceAdapter(listener, seed.get(0), seed.get(1)));
        } else {
            indices.virtualIndices.forEach((vIndex) -> {
                if (vIndex.isSupported(key)) {
                   vIndex.removeListener(listener, seed);
                }
            });
        }
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
    public void executeAfterTraversal(final Runnable runnable) {

    }

}
