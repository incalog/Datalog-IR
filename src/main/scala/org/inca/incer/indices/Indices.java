package org.inca.incer.indices;

import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.inca.meta.MetaElements.DataType;
import org.inca.meta.MetaElements.NodeLink;
import org.inca.meta.MetaElements.NodeType;

import java.util.*;

public class Indices {

    public final Map<NodeType, Set<Tuple>> nodeTypeInstances;
    public final Map<DataType, Set<Tuple>> dataTypeInstances;
    public final Map<NodeLink, Map<Object, Set<Object>>> nodeLinkInstances;
    public static final Map<Class<?>, Set<Class<?>>> subTypeMap = new HashMap<>();
    public static final Map<Class<?>, Set<Class<?>>> superTypeMap = new HashMap<>();

    /**
     * Remains null until we actually start listening to program changes.
     * This usually happens after the initalization of the indices.
     */
    private Set<Change> changeStore;

    public Indices() {
        this.nodeTypeInstances = new HashMap<>();
        this.dataTypeInstances = new HashMap<>();
        this.nodeLinkInstances = new HashMap<>();
        this.changeStore = new HashSet<>();
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
        final Tuple tuple = Tuples.staticArityFlatTupleOf(instance);
        insertInstance(type, tuple, Change.insertion(new InputKey.NodeTypeKey(type), tuple), this.nodeTypeInstances);
    }

    public void deleteNodeTypeInstance(final NodeType type, final Object instance) {
        final Tuple tuple = Tuples.staticArityFlatTupleOf(instance);
        deleteInstance(type, tuple, Change.deletion(new InputKey.NodeTypeKey(type), tuple), this.nodeTypeInstances);
    }

    public void insertDataTypeInstance(final Object instance) {
        final DataType type = new DataType(instance.getClass());
        final Tuple tuple = Tuples.staticArityFlatTupleOf(instance);
        insertInstance(type, tuple, Change.insertion(new InputKey.DataTypeKey(type), tuple), this.dataTypeInstances);
    }

    public void deleteDataTypeInstance(final Object instance) {
        final DataType type = new DataType(instance.getClass());
        final Tuple tuple = Tuples.staticArityFlatTupleOf(instance);
        deleteInstance(type, tuple, Change.deletion(new InputKey.DataTypeKey(type), tuple), this.dataTypeInstances);
    }

    public void insertNodeLinkInstance(final Object source, final NodeLink link, final Object target) {

    }

    public void deleteNodeLinkInstance(final Object source, final NodeLink link, final Object target) {

    }

    private <T> void insertInstance(final T type, final Tuple tuple, final Change change, Map<T, Set<Tuple>> instanceMap) {
        instanceMap.compute(type, (k, v) -> {
            if (v == null) {
                v = new HashSet<>();
            }
            if (v.add(tuple)) {
                this.registerChange(change);
            } else {
                throw new RuntimeException("Already known  " + type + " instance: " + tuple);
            }
            return v;
        });
    }

    private <T> void deleteInstance(final T type, final Tuple tuple, final Change change, Map<T, Set<Tuple>> instanceMap) {
        instanceMap.compute(type, (k, v) -> {
            if (v == null) {
                throw new RuntimeException("Unknown  " + type + " instance: " + tuple);
            } else {
                if (!v.remove(tuple)) {
                    throw new RuntimeException("Unknown  " + type + " instance: " + tuple);
                } else {
                    this.registerChange(change);
                }
                return v;
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

}
