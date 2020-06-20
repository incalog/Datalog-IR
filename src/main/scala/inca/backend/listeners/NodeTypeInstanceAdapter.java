package inca.backend.listeners;

import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import inca.backend.indices.InputKey;
import inca.MetaElements.LinkedType;

public class NodeTypeInstanceAdapter extends ListenerAdapter implements INodeTypeInstanceListener {

    private final Object instance;

    public NodeTypeInstanceAdapter(final IQueryRuntimeContextListener listener, final Object instance) {
        super(listener, instance);
        this.instance = instance;
    }

    @Override
    public void insert(final LinkedType type, final Object instance) {
        if (this.instance != null && !(this.instance.equals(instance))) {
            return;
        }
        this.listener.update(new InputKey.NodeTypeKey(type), Tuples.staticArityFlatTupleOf(instance), true);
    }

    @Override
    public void delete(final LinkedType type, final Object instance) {
        if (this.instance != null && !(this.instance.equals(instance))) {
            return;
        }
        this.listener.update(new InputKey.NodeTypeKey(type), Tuples.staticArityFlatTupleOf(instance), false);
    }

}
