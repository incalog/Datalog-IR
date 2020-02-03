package org.inca.incer.listeners;


import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;

import java.util.Objects;

public abstract class ListenerAdapter {

    protected final IQueryRuntimeContextListener listener;
    protected final Tuple tuple;


    public ListenerAdapter(final IQueryRuntimeContextListener listener, final Object... values) {
        this.listener = listener;
        this.tuple = Tuples.flatTupleOf(values);
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.listener, this.tuple);
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        } else if (obj == null || obj.getClass() != this.getClass()) {
            return false;
        } else {
            final ListenerAdapter that = (ListenerAdapter) obj;
            return Objects.equals(this.tuple, that.tuple) && Objects.equals(this.listener, that.listener);
        }
    }

    @Override
    public String toString() {
        return "Wrapped<Seed:" + this.tuple + ">#" + this.listener;
    }

}