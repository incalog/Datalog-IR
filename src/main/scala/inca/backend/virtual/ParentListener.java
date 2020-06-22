package inca.backend.virtual;

import inca.backend.listeners.ListenerAdapter;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;

public class ParentListener extends ListenerAdapter {
    public final truechange.NodeURI node;

    // we are only interest in the node, because the parent is always unique
    public ParentListener(final IQueryRuntimeContextListener listener, final truechange.NodeURI node) {
         super(listener, node);
         this.node = node;
    }

    public void insert(truechange.NodeURI node, truechange.NodeURI parent) {
        if (this.node != null && !(this.node.equals(node))) {
            return;
        }
        this.listener.update(ParentKey$.MODULE$, Tuples.staticArityFlatTupleOf(node, parent), true);
    }

    public void delete(truechange.NodeURI node, truechange.NodeURI parent) {
        if (this.node != null && !(this.node.equals(node))) {
            return;
        }
        this.listener.update(ParentKey$.MODULE$, Tuples.staticArityFlatTupleOf(node, parent), false);

    }
}
