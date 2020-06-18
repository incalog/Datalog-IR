package inca.backend.listeners;

import inca.MetaElements.Linked;

public interface INodeTypeInstanceListener extends IInstanceListener {
    void insert(final Linked type, final Object instance);

    void delete(final Linked type, final Object instance);
}
