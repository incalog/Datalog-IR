package org.inca.incer.listeners;

import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.inca.incer.indices.ParentKey;
import org.inca.meta.MetaElements;

public class ParentAdapter extends ListenerAdapter implements IParentListener {
    public final Object node;
    public final Object parent;

    public ParentAdapter(final IQueryRuntimeContextListener listener, final Object node, final Object parent) {
         super(listener, node, parent);
         this.node = node;
         this.parent = parent;
    }

    @Override
    public void insert(Object node, Object parent) {
        if (this.node != null && !(this.node.equals(node))) {
            return;
        }
        if (this.parent != null && !(this.parent.equals(parent))) {
            return;
        }
        this.listener.update(new ParentKey(new MetaElements.ParentLink()), Tuples.staticArityFlatTupleOf(node, parent), true);
    }

    @Override
    public void delete(Object node, Object parent) {
        if (this.node != null && !(this.node.equals(node))) {
            return;
        }
        if (this.parent != null && !(this.parent.equals(parent))) {
            return;
        }
        this.listener.update(new ParentKey(new MetaElements.ParentLink()), Tuples.staticArityFlatTupleOf(node, parent), false);

    }
}
