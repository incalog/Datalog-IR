package inca.runtime.listeners;

import inca.runtime.MetaElements.Link;

public interface INodeLinkInstanceListener extends IInstanceListener {
    void insert(final Link type, final Object source, final Object target);

    void delete(final Link type, final Object source, final Object target);
}
