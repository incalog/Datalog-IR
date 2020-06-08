package inca.backend.virtual;


import inca.MetaElements;
import inca.backend.indices.TFInputKey;

public abstract class CustomKey<T extends MetaElements.MetaElement> extends TFInputKey<T> {
    public CustomKey(T type) {
        super(type);
    }
}
