package inca.backend.indices;

import com.google.common.collect.Multiset;
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
import inca.MetaElements.PrimitiveType;
import inca.MetaElements.Link;
import inca.MetaElements.LinkedType;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;

import scala.jdk.javaapi.CollectionConverters;

public class TFRuntimeContext implements IQueryRuntimeContext {

    private final Indices indices;
    private final MetaContext metaCtx;
    private final boolean isDebugMode;

    public TFRuntimeContext(final Indices indices, final MetaContext metaCtx) {
        this(indices, metaCtx, false);
    }

    public TFRuntimeContext(final Indices indices, final MetaContext metaCtx, boolean isDebugMode) {
        this.indices = indices;
        this.metaCtx = metaCtx;
        this.isDebugMode = isDebugMode;
    }

    @Override
    public IQueryMetaContext getMetaContext() {
        return this.metaCtx;
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

        if (key instanceof InputKey.NodeTypeKey) {
            final LinkedType type = ((InputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.indices.linkedTypeInstances.getOrDefault(type, Collections.emptySet()).size();
            } else {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            }
        } else if (key instanceof InputKey.PrimitiveKey) {
            final PrimitiveType type = ((InputKey.PrimitiveKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                final Multiset<Object> instances = this.indices.primitiveTypeInstances.get(type);
                if (instances != null) {
                    result = instances.elementSet().size();
                }
            } else {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            }
        } else if (key instanceof InputKey.LinkKey) {
            final Link link = ((InputKey.LinkKey) key).type;

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
                final Map<Object, Object> linkValues = this.indices.linkInstancesReversed.get(link);
                if (linkValues != null) {
                    result = linkValues.containsKey(seedTarget) ? 1 : 0;
                }
            } else if (isSourceBound && isTargetBound) {
                // fully seeded
                result = ((containsTuple(key, seed)) ? 1 : 0);
            } else if (!(isSourceBound) /*&& !(isTargetBound)*/) {
                // fully unseeded
                final Map<Object, Object> linkValues = this.indices.linkInstances.get(link);
                result += linkValues.size();
            } else /*if (isSourceBound && !(isTargetBound))*/ {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Object> linkValues = this.indices.linkInstances.get(link);
                if (linkValues != null) {
                    result = linkValues.containsKey(seedSource) ? 1 : 0;
                }
            }
        } else {
            result = indices.virtualIndices.get(key.getStringID()).countTuples(mask, seed);
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

        if (key instanceof InputKey.NodeTypeKey) {
            final LinkedType type = ((InputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                this.indices.linkedTypeInstances.getOrDefault(type, Collections.emptySet()).forEach(e -> result.add(Tuples.staticArityFlatTupleOf(e)));
            } else {
                final Object seedInstance = mask.getValue(seed, 0);
                if (containsTuple(key, seed)) {
                    result.add(Tuples.staticArityFlatTupleOf(seedInstance));
                }
            }
        } else if (key instanceof InputKey.PrimitiveKey) {
            final PrimitiveType type = ((InputKey.PrimitiveKey) key).type;

            if (mask.indices.length == 0) {
                final Multiset<Object> instances = this.indices.primitiveTypeInstances.get(type);
                if (instances != null) {
                    instances.elementSet().forEach(e -> result.add(Tuples.staticArityFlatTupleOf(e)));
                }
            } else {
                final Object seedInstance = mask.getValue(seed, 0);
                if (containsTuple(key, seed)) {
                    result.add(Tuples.staticArityFlatTupleOf(seedInstance));
                }
            }
        } else if (key instanceof InputKey.LinkKey) {
            final Link link = ((InputKey.LinkKey) key).type;

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
                final Map<Object, Object> linkValues = this.indices.linkInstancesReversed.get(link);
                if (linkValues != null) {
                    Object source = linkValues.get(seedTarget);
                    if (source != null) {
                        result.add(Tuples.staticArityFlatTupleOf(source, seedTarget));
                    }
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
                final Map<Object, Object> linkValues = this.indices.linkInstances.get(link);
                if (linkValues != null) {
                    for (final Map.Entry<Object, Object> entry : linkValues.entrySet()) {
                        result.add(Tuples.staticArityFlatTupleOf(entry.getKey(), entry.getValue()));
                    }
                }
            } else /*if (isSourceBound && !(isTargetBound))*/ {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Object> linkValues = this.indices.linkInstances.get(link);
                if (linkValues != null) {
                    Object target = linkValues.get(seedSource);
                    if (target != null) {
                        result.add(Tuples.staticArityFlatTupleOf(seedSource, target));
                    }
                }
            }
        } else {
            scala.collection.Iterable<Tuple> tuples = indices.virtualIndices.get(key.getStringID()).enumerateTuples(mask, seed);
            result.addAll(CollectionConverters.asJavaCollection(tuples));
        }

        if (this.isDebugMode) {
            System.out.println("enumerateTuples key: " + key + " tuple: " + seed + " result size: " + result.size());
        }

        return result;
    }

    @Override
    public Iterable<?> enumerateValues(final IInputKey key, final TupleMask mask, final ITuple seed) {
        Collection<?> result = null;

        if (key instanceof InputKey.NodeTypeKey) {
            final LinkedType type = ((InputKey.NodeTypeKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.indices.linkedTypeInstances.get(type);
            } else {
                // must be unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        } else if (key instanceof InputKey.PrimitiveKey) {
            final PrimitiveType type = ((InputKey.PrimitiveKey) key).type;
            if (mask.indices.length == 0) {
                // unseeded
                result = this.indices.primitiveTypeInstances.get(type);
            } else {
                // must be unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        } else if (key instanceof InputKey.LinkKey) {
            final Link link = ((InputKey.LinkKey) key).type;

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
                final Map<Object, Object> linkValues = this.indices.linkInstancesReversed.get(link);
                if (linkValues != null) {
                    result = Collections.singletonList(linkValues.get(seedTarget));
                }
            } else if (isSourceBound && !(isTargetBound)) {
                final Object seedSource = seed.get(sourceIndex);
                final Map<Object, Object> linkValues = this.indices.linkInstances.get(link);
                if (linkValues != null) {
                    result = Collections.singletonList(linkValues.get(seedSource));
                }
            } else {
                // must be singly unseeded, this is enumerateValues after all!
                illegalEnumerateValues(seed);
            }
        } else {
            scala.collection.Iterable<?> values = indices.virtualIndices.get(key.getStringID()).enumerateValues(mask, seed);
            result = CollectionConverters.asJavaCollection(values);
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
        } else if (key instanceof InputKey.NodeTypeKey) {
            final LinkedType type = ((InputKey.NodeTypeKey) key).type;
            result = this.indices.linkedTypeInstances.getOrDefault(type, Collections.emptySet()).contains(getFromTuple(tuple, 0));
        } else if (key instanceof InputKey.PrimitiveKey) {
            final PrimitiveType type = ((InputKey.PrimitiveKey) key).type;
            final Multiset<Object> instances = this.indices.primitiveTypeInstances.get(type);
            if (instances != null) {
                result = instances.contains(getFromTuple(tuple, 0));
            }
        } else if (key instanceof InputKey.LinkKey) {
            final Link link = ((InputKey.LinkKey) key).type;
            final Map<Object, Object> linkValues = this.indices.linkInstances.get(link);
            if (linkValues != null) {
                final Object source = getFromTuple(tuple, 1);
                final Object target = getFromTuple(tuple, 0);
                result = linkValues.get(source) == target;
            }
        } else {
            result = indices.virtualIndices.get(key.getStringID()).containsTuple(tuple);
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
        } else if (key instanceof InputKey.NodeTypeKey) {
            final LinkedType type = ((InputKey.NodeTypeKey) key).type;
            this.indices.addLinkedTypeInstanceListener(type, new NodeTypeInstanceAdapter(listener, seed.get(0)));
        } else if (key instanceof InputKey.PrimitiveKey) {
            final PrimitiveType type = ((InputKey.PrimitiveKey) key).type;
            this.indices.addPrimitiveTypeInstanceListener(type, new DataTypeInstanceAdapter(listener, seed.get(0)));
        } else if (key instanceof InputKey.LinkKey) {
            final Link type = ((InputKey.LinkKey) key).type;
            this.indices.addLinkInstanceListener(type, new NodeLinkInstanceAdapter(listener, seed.get(0), seed.get(1)));
        } else {
            indices.virtualIndices.get(key.getStringID()).addListener(listener, seed);
        }
    }

    @Override
    public void removeUpdateListener(final IInputKey key, final Tuple seed, final IQueryRuntimeContextListener listener) {
        if (key instanceof JavaTransitiveInstancesKey) {
            // stateless, so NOP
        } else if (key instanceof InputKey.NodeTypeKey) {
            final LinkedType type = ((InputKey.NodeTypeKey) key).type;
            this.indices.removeLinkedTypeInstanceListener(type, new NodeTypeInstanceAdapter(listener, seed.get(0)));
        } else if (key instanceof InputKey.PrimitiveKey) {
            final PrimitiveType type = ((InputKey.PrimitiveKey) key).type;
            this.indices.removePrimitiveTypeInstanceListener(type, new DataTypeInstanceAdapter(listener, seed.get(0)));
        } else if (key instanceof InputKey.LinkKey) {
            final MetaElements.Link type = ((InputKey.LinkKey) key).type;
            this.indices.removeLinkInstanceListener(type, new NodeLinkInstanceAdapter(listener, seed.get(0), seed.get(1)));
        } else {
            indices.virtualIndices.get(key.getStringID()).removeListener(listener, seed);
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
