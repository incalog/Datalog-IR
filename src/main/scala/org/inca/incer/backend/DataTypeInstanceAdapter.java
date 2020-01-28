package org.inca.incer.backend;

import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.inca.incer.indices.TFInputKey;
import org.inca.meta.MetaElements;

public class DataTypeInstanceAdapter extends ListenerAdapter implements IDataTypeInstanceListener {

    private final Object value;

    public DataTypeInstanceAdapter(final IQueryRuntimeContextListener listener, final Object value) {
        super(listener, value);
        this.value = value;
    }

    @Override
    public void dataTypeInstanceInserted(final MetaElements.DataType type, final Object value, final boolean firstOccurrence) {
        if (firstOccurrence) {
            if (this.value != null && !(this.value.equals(value))) {
                return;
            }
            this.listener.update(new TFInputKey.DataTypeKey(type), Tuples.staticArityFlatTupleOf(value), true);
        }
    }

    @Override
    public void dataTypeInstanceDeleted(final MetaElements.DataType type, final Object value, final boolean lastOccurrence) {
        if (lastOccurrence) {
            if (this.value != null && !(this.value.equals(value))) {
                return;
            }
            this.listener.update(new TFInputKey.DataTypeKey(type), Tuples.staticArityFlatTupleOf(value), false);
        }
    }

}
