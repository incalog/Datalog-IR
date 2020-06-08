package inca.backend.listeners;

import inca.MetaElements.Link;

public interface INodeLinkInstanceListener extends IInstanceListener {
    void insert(final Link type, final Object source, final Object target);

    void delete(final Link type, final Object source, final Object target);
}
