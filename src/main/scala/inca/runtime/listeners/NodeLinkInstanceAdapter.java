package inca.runtime.listeners;

import inca.runtime.index.LinkKey;
import inca.runtime.index.MetaElements.Link;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;

public class NodeLinkInstanceAdapter extends ListenerAdapter implements INodeLinkInstanceListener {

    private final Object source;
    private final Object target;

    public NodeLinkInstanceAdapter(final IQueryRuntimeContextListener listener, final Object source, final Object target) {
        super(listener, source, target);
        this.source = source;
        this.target = target;
    }

    @Override
    public void insert(final Link type, final Object source, final Object target) {
        if (this.source != null && !(this.source.equals(source))) {
            return;
        }
        if (this.target != null && !(this.target.equals(target))) {
            return;
        }
        this.listener.update(new LinkKey(type), Tuples.staticArityFlatTupleOf(source, target), true);
    }

    @Override
    public void delete(final Link type, final Object source, final Object target) {
        if (this.source != null && !(this.source.equals(source))) {
            return;
        }
        if (this.target != null && !(this.target.equals(target))) {
            return;
        }
        this.listener.update(new LinkKey(type), Tuples.staticArityFlatTupleOf(source, target), false);
    }

}
