package org.inca.incer.indices.custom;

import org.inca.incer.Incrementalizable;
import org.inca.incer.listeners.ICustomListener;

import java.util.HashSet;
import java.util.Set;

public abstract class CustomIndex<K, L extends ICustomListener> {

    public Set<L> listeners;

    public CustomIndex() {
        this.listeners = new HashSet<L>();
    }

    public
    // initialize
    // maybe just update method
    abstract void update();
    // insert
    // delete
    // notify listener
    abstract void clear();
}
