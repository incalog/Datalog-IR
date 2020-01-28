package org.inca.incer.backend;

import org.inca.meta.MetaElements;

public interface IDataTypeInstanceListener {
    void dataTypeInstanceInserted(MetaElements.DataType type, Object value, boolean firstOccurrence);

    void dataTypeInstanceDeleted(MetaElements.DataType type, Object value, boolean lastOccurrence);
}
