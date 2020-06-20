package inca.backend.listeners;

import inca.MetaElements.LinkedType;

public interface INodeTypeInstanceListener extends IInstanceListener {
    void insert(final LinkedType type, final Object instance);

    void delete(final LinkedType type, final Object instance);
}
