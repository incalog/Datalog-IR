package inca.backend.listeners;

import inca.MetaElements;

public interface IDataTypeInstanceListener extends IInstanceListener {
    void insert(MetaElements.PrimitiveType type, Object value);

    void delete(MetaElements.PrimitiveType type, Object value);
}
