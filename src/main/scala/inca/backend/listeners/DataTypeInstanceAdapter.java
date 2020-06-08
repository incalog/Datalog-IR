package inca.backend.listeners;

import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import inca.backend.indices.TFInputKey;
import inca.MetaElements;

public class DataTypeInstanceAdapter extends ListenerAdapter implements IDataTypeInstanceListener {

    private final Object value;

    public DataTypeInstanceAdapter(final IQueryRuntimeContextListener listener, final Object value) {
        super(listener, value);
        this.value = value;
    }

    @Override
    public void insert(final MetaElements.DataType type, final Object value) {
        if (this.value != null && !(this.value.equals(value))) {
            return;
        }
        this.listener.update(new TFInputKey.DataTypeKey(type), Tuples.staticArityFlatTupleOf(value), true);
    }

    @Override
    public void delete(final MetaElements.DataType type, final Object value) {
        if (this.value != null && !(this.value.equals(value))) {
            return;
        }
        this.listener.update(new TFInputKey.DataTypeKey(type), Tuples.staticArityFlatTupleOf(value), false);
    }

}
