package org.inca.incer.backend;

import org.inca.meta.MetaElements;

public interface INodeLinkInstanceListener {
    void conceptFeatureInserted(final MetaElements.NodeLink type, final Object source, final Object target);

    void conceptFeatureDeleted(final MetaElements.NodeLink type, final Object source, final Object target);
}
