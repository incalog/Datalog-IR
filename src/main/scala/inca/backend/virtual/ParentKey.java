package inca.backend.virtual;

import inca.MetaElements;

public class ParentKey extends inca.backend.indices.TFInputKey<MetaElements.ParentLink> {
    public ParentKey(MetaElements.ParentLink type) {
        super(type);
    }

    @Override
    public int getArity() {
        return 2;
    }
}
