package inca.backend.virtual;

import inca.backend.listeners.ListenerAdapter;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;

public class ParentListener extends ListenerAdapter {
    public final truechange.NodeURI node;
    public final truechange.NodeURI parent;

    public ParentListener(final IQueryRuntimeContextListener listener, final truechange.NodeURI node, final truechange.NodeURI parent) {
         super(listener, node, parent);
         this.node = node;
         this.parent = parent;
    }

    public void insert(truechange.NodeURI node, truechange.NodeURI parent) {
        if (this.node != null && !(this.node.equals(node))) {
            return;
        }
        if (this.parent != null && !(this.parent.equals(parent))) {
            return;
        }
        this.listener.update(ParentKey$.MODULE$, Tuples.staticArityFlatTupleOf(node, parent), true);
    }

    public void delete(truechange.NodeURI node, truechange.NodeURI parent) {
        if (this.node != null && !(this.node.equals(node))) {
            return;
        }
        if (this.parent != null && !(this.parent.equals(parent))) {
            return;
        }
        this.listener.update(ParentKey$.MODULE$, Tuples.staticArityFlatTupleOf(node, parent), false);

    }
}
