package inca.runtime.listeners;

import inca.runtime.MetaElements;

public interface IDataTypeInstanceListener extends IInstanceListener {
    void insert(MetaElements.PrimitiveType type, Object value);

    void delete(MetaElements.PrimitiveType type, Object value);
}
