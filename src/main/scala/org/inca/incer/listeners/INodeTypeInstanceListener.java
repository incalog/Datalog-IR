package org.inca.incer.listeners;

import org.inca.meta.MetaElements;

public interface INodeTypeInstanceListener extends IInstanceListener {
    void insert(final MetaElements.NodeType type, final Object instance);

    void delete(final MetaElements.NodeType type, final Object instance);
}
