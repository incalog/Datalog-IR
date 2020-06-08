package org.inca.incer.indices;

import org.inca.meta.MetaElements;

public class ParentKey extends CustomKey<MetaElements.ParentLink> {
    public ParentKey(MetaElements.ParentLink type) {
        super(type);
    }

    @Override
    public int getArity() {
        return 2;
    }
}
