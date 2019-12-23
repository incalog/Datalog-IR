package org.inca.incer.indices;

import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;
import org.inca.meta.MetaElements.DataType;
import org.inca.meta.MetaElements.MetaElement;
import org.inca.meta.MetaElements.NodeLink;
import org.inca.meta.MetaElements.NodeType;

public abstract class InputKey<T extends MetaElement> implements IInputKey {

    protected final T type;

    public InputKey(final T type) {
        this.type = type;
    }

    @Override
    public String getPrettyPrintableName() {
        return type.toString();
    }

    @Override
    public String getStringID() {
        return type.toString();
    }

    @Override
    public boolean isEnumerable() {
        return true;
    }

    @Override
    public int hashCode() {
        return this.type.hashCode();
    }

    @Override
    public String toString() {
        return this.getPrettyPrintableName();
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == null || obj.getClass() != this.getClass()) {
            return false;
        } else if (this == obj) {
            return true;
        } else {
            final InputKey that = (InputKey) obj;
            return this.type.equals(that.type);
        }
    }

    public static class NodeTypeKey extends InputKey<NodeType> {

        public NodeTypeKey(final NodeType type) {
            super(type);
        }

        @Override
        public int getArity() {
            return 1;
        }

    }

    public static class DataTypeKey extends InputKey<DataType> {

        public DataTypeKey(final DataType type) {
            super(type);
        }

        @Override
        public int getArity() {
            return 1;
        }

    }

    public static class NodeLinkKey extends InputKey<NodeLink> {

        public NodeLinkKey(final NodeLink link) {
            super(link);
        }

        @Override
        public int getArity() {
            return 2;
        }

    }

}
