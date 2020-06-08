package inca.backend.listeners;

import inca.MetaElements;

public interface IDataTypeInstanceListener extends IInstanceListener {
    void insert(MetaElements.DataType type, Object value);

    void delete(MetaElements.DataType type, Object value);
}
