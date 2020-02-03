package org.inca.incer.listeners;

import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.inca.incer.indices.TFInputKey;
import org.inca.meta.MetaElements;

public class NodeLinkInstanceAdapter extends ListenerAdapter implements INodeLinkInstanceListener {

    private final Object source;
    private final Object target;

    public NodeLinkInstanceAdapter(final IQueryRuntimeContextListener listener, final Object source, final Object target) {
        super(listener, source, target);
        this.source = source;
        this.target = target;
    }

    @Override
    public void insert(final MetaElements.NodeLink type, final Object source, final Object target) {
        if (this.source != null && !(this.source.equals(source))) {
            return;
        }
        if (this.target != null && !(this.target.equals(target))) {
            return;
        }
        this.listener.update(new TFInputKey.NodeLinkKey(type), Tuples.staticArityFlatTupleOf(source, target), true);
    }

    @Override
    public void delete(final MetaElements.NodeLink type, final Object source, final Object target) {
        if (this.source != null && !(this.source.equals(source))) {
            return;
        }
        if (this.target != null && !(this.target.equals(target))) {
            return;
        }
        this.listener.update(new TFInputKey.NodeLinkKey(type), Tuples.staticArityFlatTupleOf(source, target), false);
    }

}
