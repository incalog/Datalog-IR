package inca.backend.listeners;

import inca.MetaElements;

public interface IDataTypeInstanceListener extends IInstanceListener {
    void insert(MetaElements.Primitive type, Object value);

    void delete(MetaElements.Primitive type, Object value);
}
