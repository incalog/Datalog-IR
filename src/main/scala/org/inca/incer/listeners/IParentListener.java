package org.inca.incer.listeners;

public interface IParentListener extends ICustomListener {
    public void insert(Object node, Object parent);
    public void delete(Object node, Object parent);
}
