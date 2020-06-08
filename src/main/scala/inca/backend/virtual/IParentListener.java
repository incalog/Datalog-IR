package inca.backend.virtual;

import inca.backend.virtual.ICustomListener;

public interface IParentListener extends ICustomListener {
    public void insert(Object node, Object parent);
    public void delete(Object node, Object parent);
}
