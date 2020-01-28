package org.inca.incer.backend;

import org.inca.meta.MetaElements;

public interface INodeTypeInstanceListener {
    void instanceInserted(final MetaElements.NodeType type, final Object instance);

    void instanceDeleted(final MetaElements.NodeType type, final Object instance);
}
