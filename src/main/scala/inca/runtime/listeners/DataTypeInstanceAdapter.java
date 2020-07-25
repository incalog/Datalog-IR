package inca.runtime.listeners;

import inca.runtime.index.MetaElements;
import inca.runtime.index.PrimitiveTypeKey;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;

public class DataTypeInstanceAdapter extends ListenerAdapter implements IDataTypeInstanceListener {

    private final Object value;

    public DataTypeInstanceAdapter(final IQueryRuntimeContextListener listener, final Object value) {
        super(listener, value);
        this.value = value;
    }

    @Override
    public void insert(final MetaElements.PrimitiveType type, final Object value) {
        if (this.value != null && !(this.value.equals(value))) {
            return;
        }
        this.listener.update(new PrimitiveTypeKey(type), Tuples.staticArityFlatTupleOf(value), true);
    }

    @Override
    public void delete(final MetaElements.PrimitiveType type, final Object value) {
        if (this.value != null && !(this.value.equals(value))) {
            return;
        }
        this.listener.update(new PrimitiveTypeKey(type), Tuples.staticArityFlatTupleOf(value), false);
    }

}
