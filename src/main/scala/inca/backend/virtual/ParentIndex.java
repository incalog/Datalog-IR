package inca.backend.virtual;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class ParentIndex {
    public final Map<Object, Object> parents = new HashMap<>();
    // public final Map<Object, Object> reversedParent = new HashMap<>();
    public final Set<IParentListener> parentListeners = new HashSet<>();

    public boolean isDirty = false;

    public ParentIndex() {
    }

    // TODO macro method to implement insert and delete logic

    public void insertParent(Object node, Object parent) {
        this.parents.compute(node, (k, v) -> {
            notifyParentListeners(node, parent, true);
            return parent;
        });
//        this.reversedParent.compute(parent, (k, v) -> {
//            return node;
//        });
    }

    private void notifyParentListeners(final Object node, Object parent, final boolean isInsertion) {
        isDirty |= !this.parentListeners.isEmpty();
        for (final IParentListener listener : this.parentListeners) {
            if (isInsertion) {
                listener.insert(node, parent);
            } else {
                listener.delete(node, parent);
            }
        }
    }

    public void deleteParent(Object node, Object parent) {
        this.parents.compute(node, (k, v) -> {
            notifyParentListeners(node, parent, false);
            return null;
        });
//        this.reversedParent.compute(parent, (k, v) -> {
//            return null;
//        });
    }

    public void addParentListener(final IParentListener listener) {
        this.parentListeners.add(listener);
    }

    public void removeParentListener(final IParentListener listener) {
        this.parentListeners.remove(listener);
    }
}
