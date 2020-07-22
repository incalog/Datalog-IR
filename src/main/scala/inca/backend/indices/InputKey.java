package inca.backend.indices;

import inca.MetaElements.Link;
import inca.MetaElements.LinkedType;
import inca.MetaElements.PrimitiveType;
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;

public abstract class InputKey<T> implements IInputKey {

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

    public static class NodeTypeKey extends InputKey<LinkedType> {

        public NodeTypeKey(final LinkedType type) {
            super(type);
        }

        @Override
        public int getArity() {
            return 1;
        }

    }

    public static class PrimitiveKey extends InputKey<PrimitiveType> {

        public PrimitiveKey(final PrimitiveType type) {
            super(type);
        }

        @Override
        public int getArity() {
            return 1;
        }

    }

    public static class LinkKey extends InputKey<Link> {

        public LinkKey(final Link link) {
            super(link);
        }

        @Override
        public int getArity() {
            return 2;
        }

    }

}
