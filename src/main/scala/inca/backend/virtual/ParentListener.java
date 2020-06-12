package inca.backend.virtual;

import inca.backend.listeners.ListenerAdapter;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import inca.MetaElements;

public class ParentListener extends ListenerAdapter {
    public final truechange.Node node;
    public final truechange.Node parent;

    public ParentListener(final IQueryRuntimeContextListener listener, final truechange.Node node, final truechange.Node parent) {
         super(listener, node, parent);
         this.node = node;
         this.parent = parent;
    }

    public void insert(truechange.Node node, truechange.Node parent) {
        if (this.node != null && !(this.node.equals(node))) {
            return;
        }
        if (this.parent != null && !(this.parent.equals(parent))) {
            return;
        }
        this.listener.update(new ParentKey(new MetaElements.ParentLink()), Tuples.staticArityFlatTupleOf(node, parent), true);
    }

    public void delete(truechange.Node node, truechange.Node parent) {
        if (this.node != null && !(this.node.equals(node))) {
            return;
        }
        if (this.parent != null && !(this.parent.equals(parent))) {
            return;
        }
        this.listener.update(new ParentKey(new MetaElements.ParentLink()), Tuples.staticArityFlatTupleOf(node, parent), false);

    }
}
