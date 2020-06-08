package org.inca.incer.indices;


import org.inca.meta.MetaElements;

public abstract class CustomKey<T extends MetaElements.MetaElement> extends TFInputKey<T> {
    public CustomKey(T type) {
        super(type);
    }
}
