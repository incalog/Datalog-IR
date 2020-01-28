package org.inca.incer.listeners;

import org.inca.meta.MetaElements;

public interface IDataTypeInstanceListener extends IInstanceListener {
    void insert(MetaElements.DataType type, Object value);

    void delete(MetaElements.DataType type, Object value);
}
