package inca.backend.virtual;

import inca.MetaElements;
import inca.backend.virtual.CustomKey;

public class ParentKey extends CustomKey<MetaElements.ParentLink> {
    public ParentKey(MetaElements.ParentLink type) {
        super(type);
    }

    @Override
    public int getArity() {
        return 2;
    }
}
