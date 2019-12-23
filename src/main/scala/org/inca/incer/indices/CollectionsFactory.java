package org.inca.incer.indices;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public abstract class CollectionsFactory {

    public abstract <V> Set<V> createSet();

    public abstract <K, V> Map<K, V> createMap();

    public final static CollectionsFactory JAVA = new CollectionsFactory() {
        @Override
        public <V> Set<V> createSet() {
            return new HashSet<>();
        }

        @Override
        public <K, V> Map<K, V> createMap() {
            return new HashMap<>();
        }
    };

}
