package org.inca.incer.indices;

import org.eclipse.viatra.query.runtime.matchers.context.AbstractQueryMetaContext;
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;
import org.eclipse.viatra.query.runtime.matchers.context.InputKeyImplication;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.inca.meta.MetaElements.DataType;
import org.inca.meta.MetaElements.NodeLink;
import org.inca.meta.MetaElements.NodeType;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class Indices {

    private final CollectionsFactory collectionsFactory;
    private final Map<NodeType, Set<Tuple>> nodeTypeInstances;
    private final Map<DataType, Set<Tuple>> dataTypeInstances;
    private final Map<NodeLink, Map<Object, Set<Object>>> nodeLinkInstances;

    /**
     * Remains null until we actually start listening to program changes.
     * This usually happens after the initalization of the indices.
     */
    private Set<Change> changeStore;

    public Indices(final CollectionsFactory collectionsFactory) {
        this.collectionsFactory = collectionsFactory;
        this.nodeTypeInstances = collectionsFactory.createMap();
        this.dataTypeInstances = collectionsFactory.createMap();
        this.nodeLinkInstances = collectionsFactory.createMap();
    }

    public void insertNodeTypeInstance(final NodeType type, final Object instance) {
        final Tuple tuple = Tuples.staticArityFlatTupleOf(instance);
        insertInstance(type, tuple, Change.insertion(new InputKey.NodeTypeKey(type), tuple), this.nodeTypeInstances);
    }

    public void deleteNodeTypeInstance(final NodeType type, final Object instance) {
        final Tuple tuple = Tuples.staticArityFlatTupleOf(instance);
        deleteInstance(type, tuple, Change.deletion(new InputKey.NodeTypeKey(type), tuple), this.nodeTypeInstances);
    }

    public void insertDataTypeInstance(final DataType type, final Object instance) {
        final Tuple tuple = Tuples.staticArityFlatTupleOf(instance);
        insertInstance(type, tuple, Change.insertion(new InputKey.DataTypeKey(type), tuple), this.dataTypeInstances);
    }

    public void deleteDataTypeInstance(final DataType type, final Object instance) {
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
                v = this.collectionsFactory.createSet();
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
    }

}
