package inca.backend.listeners;

import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import inca.backend.indices.TFInputKey;
import inca.MetaElements.Linked;

public class NodeTypeInstanceAdapter extends ListenerAdapter implements INodeTypeInstanceListener {

    private final Object instance;

    public NodeTypeInstanceAdapter(final IQueryRuntimeContextListener listener, final Object instance) {
        super(listener, instance);
        this.instance = instance;
    }

    @Override
    public void insert(final Linked type, final Object instance) {
        if (this.instance != null && !(this.instance.equals(instance))) {
            return;
        }
        this.listener.update(new TFInputKey.NodeTypeKey(type), Tuples.staticArityFlatTupleOf(instance), true);
    }

    @Override
    public void delete(final Linked type, final Object instance) {
        if (this.instance != null && !(this.instance.equals(instance))) {
            return;
        }
        this.listener.update(new TFInputKey.NodeTypeKey(type), Tuples.staticArityFlatTupleOf(instance), false);
    }

}
