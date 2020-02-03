package org.inca.incer.listeners;

import org.inca.meta.MetaElements;

public interface INodeLinkInstanceListener extends IInstanceListener {
    void insert(final MetaElements.NodeLink type, final Object source, final Object target);

    void delete(final MetaElements.NodeLink type, final Object source, final Object target);
}
