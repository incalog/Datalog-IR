package org.inca.incer.listeners;

import org.inca.meta.MetaElements.Link;

public interface INodeLinkInstanceListener extends IInstanceListener {
    void insert(final Link type, final Object source, final Object target);

    void delete(final Link type, final Object source, final Object target);
}
