package inca.backend.virtual;

import inca.backend.listeners.ListenerAdapter;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;

public class ListElementsListener extends ListenerAdapter {
    public final truechange.NodeURI list;
    public final truechange.NodeURI element;

    public ListElementsListener(IQueryRuntimeContextListener listener, final truechange.NodeURI list, final truechange.NodeURI element) {
        super(listener, list, element);
        this.list = list;
        this.element = element;
    }

    public void insert(truechange.NodeURI list, truechange.NodeURI element) {
        if (this.list != null && !(this.list.equals(list))) {
            return;
        }
        if (this.element != null && !(this.element.equals(element))) {
            return;
        }
        this.listener.update(ListElementsKey$.MODULE$, Tuples.staticArityFlatTupleOf(list, element), true);
    }

    public void delete(truechange.NodeURI list, truechange.NodeURI element) {
        if (this.list != null && !(this.list.equals(list))) {
            return;
        }
        if (this.element != null && !(this.element.equals(element))) {
            return;
        }
        this.listener.update(ListElementsKey$.MODULE$, Tuples.staticArityFlatTupleOf(list, element), false);
    }
}
