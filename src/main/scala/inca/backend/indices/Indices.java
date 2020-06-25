package inca.backend.indices;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;
import inca.MetaElements;
import inca.backend.virtual.VirtualIndex;
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.scope.IBaseIndex;
import org.eclipse.viatra.query.runtime.api.scope.IIndexingErrorListener;
import org.eclipse.viatra.query.runtime.api.scope.IInstanceObserver;
import org.eclipse.viatra.query.runtime.api.scope.ViatraBaseIndexChangeListener;
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple;
import inca.backend.listeners.IDataTypeInstanceListener;
import inca.backend.listeners.IInstanceListener;
import inca.backend.listeners.INodeLinkInstanceListener;
import inca.backend.listeners.INodeTypeInstanceListener;
import inca.MetaElements.PrimitiveType;
import inca.MetaElements.Link;
import inca.MetaElements.NodeType;
import inca.MetaElements.LinkedType;
import scala.Tuple2;
import scala.collection.Iterator;
import truechange.*;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Callable;

public class Indices implements IBaseIndex {

    final Map<LinkedType, Set<Object>> linkedTypeInstances;
    final Map<LinkedType, Set<INodeTypeInstanceListener>> linkedTypeInstancesListener;

    final Map<PrimitiveType, Multiset<Object>> primitiveTypeInstances;
    final Map<PrimitiveType, Set<IDataTypeInstanceListener>> primitiveTypeInstanceListeners;

    // source -> target
    final Map<Link, Map<Object, Object>> linkInstances;
    // target -> source
    final Map<Link, Map<Object, Object>> linkInstancesReversed;
    final Map<Link, Set<INodeLinkInstanceListener>> linkInstanceListeners;

    public final Map<String, Set<String>> subtypeMap = new HashMap<>();
    public final Map<String, Set<String>> supertypeMap = new HashMap<>();

    final Map<String, VirtualIndex> virtualIndices;

    private final Set<ViatraBaseIndexChangeListener> changeListeners;
    private AdvancedViatraQueryEngine engine;

    private boolean isDirty;

    public Indices() {
        this(null, Collections.emptyMap(), Collections.emptyMap(), Collections.emptyMap());
    }

    public Indices(final AdvancedViatraQueryEngine engine, final Map<String, Set<String>> subtypeMap, final Map<String, Set<String>> supertypeMap, final Map<String, VirtualIndex> virtualIndices) {
        this.linkedTypeInstances = new HashMap<>();
        this.linkedTypeInstancesListener = new HashMap<>();
        this.primitiveTypeInstances = new HashMap<>();
        this.primitiveTypeInstanceListeners = new HashMap<>();
        this.linkInstances = new HashMap<>();
        this.linkInstancesReversed = new HashMap<>();
        this.linkInstanceListeners = new HashMap<>();
        this.changeListeners = new HashSet<>();
        this.virtualIndices = virtualIndices;
        if (subtypeMap != null) {
            this.subtypeMap.putAll(subtypeMap);
        }
        if (supertypeMap != null) {
            this.supertypeMap.putAll(supertypeMap);
        }

        this.engine = engine;
    }

    public void processChangeset(Changeset changeset) {
        Iterator<truechange.Change> changesetIterator = changeset.changes().iterator();
        while (changesetIterator.hasNext()) {
            truechange.Change change = changesetIterator.next();
            // process virtual indices
            virtualIndices.values().forEach((vIndex) -> {
                vIndex.processChange(change);
            });
            if (change instanceof Detach) {
                // delete nodeLinkInstance
                Detach detach = (Detach) change;
                if (detach.link() instanceof RootLink$) {
                    // just skip because we do not keep track of root link
                } else {
                    deleteLinkInstance(detach.parent(), convertNodeAndLinkToNodeLink(detach.ptag(), detach.link()), detach.node());
                }
            } else if (change instanceof Unload) {
                Unload unload = (Unload) change;
                // insert nodeTypeInstance
                LinkedType node = convertTagToNodeType(unload.tag());
                Set<String> supertypes = supertypeMap.getOrDefault(node.name(), Collections.emptySet());
                for (String supertype : supertypes) {
                    NodeType nodeTypeSupertype = new NodeType(supertype);
                    deleteLinkedTypeInstance(nodeTypeSupertype, unload.node());
                }
                deleteLinkedTypeInstance(node, unload.node());
                Iterator<Tuple2<String, NodeURI>> kidsIterator = unload.kids().iterator();
                while(kidsIterator.hasNext()) {
                    Tuple2<String, NodeURI> kid = kidsIterator.next();
                    deleteLinkInstance(unload.node(), convertNodeAndStringToNodeLink(unload.tag(), kid._1), kid._2);
                }
                // delete dataTypeInstance for each lit
                // delete nodeLinkInstances for each lit
                Iterator<Tuple2<String, Object>> litsIterator = unload.lits().iterator();
                while(litsIterator.hasNext()) {
                    Tuple2<String, Object> lit = litsIterator.next();
                    deletePrimitiveTypeInstance(lit._2);
                    deleteLinkInstance(unload.node(), convertNodeAndStringToNodeLink(unload.tag(), lit._1), lit._2);
                }
            } else if (change instanceof Attach) {
                // insert nodeLinkInstance
                Attach attach = (Attach) change;
                if (attach.link() instanceof RootLink$) {
                    // just skip because we do not keep track of root link
                } else {
                    insertLinkInstance(attach.parent(), convertNodeAndLinkToNodeLink(attach.ptag(), attach.link()), attach.node());
                }
            } else if (change instanceof Load) {
                // insert nodeTypeInstance
                Load load = (Load) change;
                LinkedType node = convertTagToNodeType(load.tag());

                Set<String> supertypes = supertypeMap.getOrDefault(node.name(), Collections.emptySet());
                for (String supertype : supertypes) {
                    NodeType nodeTypeSupertype = new NodeType(supertype);
                    insertLinkedTypeInstance(nodeTypeSupertype, load.node());
                }

                insertLinkedTypeInstance(node, load.node());
                Iterator<Tuple2<String, NodeURI>> kidsIterator = load.kids().iterator();
                while(kidsIterator.hasNext()) {
                    Tuple2<String, NodeURI> kid = kidsIterator.next();
                    insertLinkInstance(load.node(), convertNodeAndStringToNodeLink(load.tag(), kid._1), kid._2);
                }
                Iterator<Tuple2<String, Object>> litsIterator = load.lits().iterator();
                while(litsIterator.hasNext()) {
                    Tuple2<String, Object> lit = litsIterator.next();
                    insertPrimitiveTypeInstance(lit._2);
                    insertLinkInstance(load.node(), convertNodeAndStringToNodeLink(load.tag(), lit._1), lit._2);
                }
            }
        }
    }

    private LinkedType convertTagToNodeType(truechange.NodeTag tag) {
        if (tag instanceof ConstrTag) {
            ConstrTag constrTag = (ConstrTag) tag;
            return new MetaElements.NodeType(constrTag.c());
        } else if (tag instanceof ListTag) {
            ListTag listTag = (ListTag) tag;
            // TODO currently assume that it wraps sorttype
            SortType wrapped = (SortType) listTag.ty();
            return new MetaElements.ListType(new MetaElements.NodeType(wrapped.tag().getCanonicalName()));
        } else {
            throw new IllegalArgumentException("Expected ConstrTag but got: " + tag);
        }
    }

    private Link convertNodeAndLinkToNodeLink(truechange.NodeTag tag, truechange.Link link) {
       LinkedType node = convertTagToNodeType(tag);
       if (link instanceof NamedLink) {
           NamedLink namedLink = (NamedLink) link;
           return node.apply(namedLink.name());
       } else if (link instanceof truechange.ListFirstLink) {
           truechange.ListFirstLink firstLink = (truechange.ListFirstLink)  link;
           return new MetaElements.ListFirstLink((MetaElements.ListType) node);
       } else if (link instanceof truechange.ListNextLink) {
           return new MetaElements.ListNextLink();
       }
       return null;
    }

    private Link convertNodeAndStringToNodeLink(truechange.NodeTag tag, String linkName) {
        LinkedType node = convertTagToNodeType(tag);
        return node.apply(linkName);
    }

    public void dispose() {
        this.linkedTypeInstances.clear();
        this.linkedTypeInstancesListener.clear();
        this.primitiveTypeInstances.clear();
        this.primitiveTypeInstanceListeners.clear();
        this.linkInstances.clear();
        this.linkInstancesReversed.clear();
        this.linkInstanceListeners.clear();
        this.changeListeners.clear();
        this.virtualIndices.clear();
        this.subtypeMap.clear();
        this.supertypeMap.clear();
        this.engine = null;
    }

    public <V> V update(final Callable<V> callable) {
        V result = null;
        try {
            // the engine may be null in debug scenarios
            if (engine == null) {
                result = callable.call();
            } else {
                result = engine.delayUpdatePropagation(callable);
            }
        } catch (final Exception e) {
            throw new RuntimeException(e);
        }

        final boolean[] virtualIsDirty = {false};
        virtualIndices.values().forEach((vIndex) -> {
            virtualIsDirty[0] |= vIndex.isDirty();
        });

        notifyBaseIndexChangeListeners(this.isDirty || virtualIsDirty[0]);

        return result;
    }

    public void insertLinkedTypeInstance(final LinkedType type, final Object instance) {
        this.linkedTypeInstances.compute(type, (k, v) -> {
            if (v == null) {
                v = new HashSet<>();
            }
            v.add(instance);
            notifyLinkedTypeInstanceListeners(type, instance, true);
//            } else {
//                throw new RuntimeException("Already known  " + type + " instance: " + instance);
//            }
            return v;
        });
    }

    public void deleteLinkedTypeInstance(final LinkedType type, final Object instance) {
        this.linkedTypeInstances.compute(type, (k, v) -> {
            if (v == null) {
                throw new RuntimeException("Unknown  " + type + " instance: " + instance);
            } else {
                if (v.remove(instance)) {
                    notifyLinkedTypeInstanceListeners(type, instance, false);
                } else {
                    throw new RuntimeException("Unknown  " + type + " instance: " + instance);
                }
                if (v.isEmpty()) {
                    return null;
                } else {
                    return v;
                }
            }
        });
    }

    public void insertPrimitiveTypeInstance(final Object instance) {
        final PrimitiveType type = new PrimitiveType(instance.getClass().getCanonicalName());
        this.primitiveTypeInstances.compute(type, (k, v) -> {
            if (v == null) {
                v = HashMultiset.create();
            }
            final boolean isFirstOccurrence = !v.contains(instance);
            v.add(instance);
            if (isFirstOccurrence) {
                notifyPrimitiveTypeInstanceListeners(type, instance, true);
            }
            return v;
        });
    }

    public void deletePrimitiveTypeInstance(final Object instance) {
        final PrimitiveType type = new PrimitiveType(instance.getClass().getCanonicalName());
        this.primitiveTypeInstances.compute(type, (k, v) -> {
            if (v == null) {
                throw new RuntimeException("Unknown  " + type + " instance: " + instance);
            } else {
                final boolean isLastOccurrence = v.count(instance) == 1;
                v.remove(instance);
                if (isLastOccurrence) {
                    notifyPrimitiveTypeInstanceListeners(type, instance, false);
                }
                if (v.isEmpty()) {
                    return null;
                } else {
                    return v;
                }
            }
        });
    }

    private void insertLinkInstanceInternal(final Object source, final Link link, final Object target,
                                            final Map<Link, Map<Object, Object>> map, final boolean notifyAbout) {
        map.compute(link, (ok, ov) -> {
            if (ov == null) {
                ov = new HashMap<>();
            }
            ov.compute(source, (ik, iv) -> {
                if (iv != target) {
                    iv = target;
                    if (notifyAbout) {
                        notifyLinkInstanceListeners(link, source, target, true);
                    }
                } else {
                    throw new RuntimeException("Already known  " + link + " instance: " + source + " -> " + target);
                }
                return iv;
            });
            return ov;
        });
    }

    public void insertLinkInstance(final Object source, final Link link, final Object target) {
        insertLinkInstanceInternal(source, link, target, this.linkInstances, true);
        insertLinkInstanceInternal(target, link, source, this.linkInstancesReversed, false);
    }

    private void deleteLinkInstanceInternal(final Object source, final Link link, final Object target,
                                            final Map<Link, Map<Object, Object>> map, final boolean notifyAbout) {
        map.compute(link, (ok, ov) -> {
            if (ov == null) {
                throw new RuntimeException("Unknown  " + link + " instance: " + source + " -> " + target);
            }
            ov.compute(source, (ik, iv) -> {
                if (iv != target) {
                    throw new RuntimeException("Unknown  " + link + " instance: " + source + " -> " + target);
                } else {
                    if (notifyAbout) {
                        notifyLinkInstanceListeners(link, source, target, false);
                    }
                    return null;
                }
            });
            if (ov.isEmpty()) {
                return null;
            } else {
                return ov;
            }
        });
    }

    public void deleteLinkInstance(final Object source, final Link link, final Object target) {
        deleteLinkInstanceInternal(source, link, target, this.linkInstances, true);
        deleteLinkInstanceInternal(target, link, source, this.linkInstancesReversed, false);
    }

    private void notifyLinkedTypeInstanceListeners(final LinkedType type, final Object instance, final boolean isInsertion) {
        final Set<INodeTypeInstanceListener> listeners =
                this.linkedTypeInstancesListener.getOrDefault(type,
                        Collections.emptySet());
        isDirty |= !listeners.isEmpty();
        for (final INodeTypeInstanceListener listener : listeners) {
            if (isInsertion) {
                listener.insert(type, instance);
            } else {
                listener.delete(type, instance);
            }
        }
    }

    private void notifyPrimitiveTypeInstanceListeners(final PrimitiveType type, final Object instance, final boolean isInsertion) {
        final Set<IDataTypeInstanceListener> listeners =
                this.primitiveTypeInstanceListeners.getOrDefault(type,
                        Collections.emptySet());
        isDirty |= !listeners.isEmpty();
        for (final IDataTypeInstanceListener listener : listeners) {
            if (isInsertion) {
                listener.insert(type, instance);
            } else {
                listener.delete(type, instance);
            }
        }
    }

    private void notifyLinkInstanceListeners(final Link type, final Object source, Object target, final boolean isInsertion) {
        final Set<INodeLinkInstanceListener> listeners =
                this.linkInstanceListeners.getOrDefault(type,
                        Collections.emptySet());
        isDirty |= !listeners.isEmpty();
        for (final INodeLinkInstanceListener listener : listeners) {
            if (isInsertion) {
                listener.insert(type, source, target);
            } else {
                listener.delete(type, source, target);
            }
        }
    }

    @Override
    public <V> V coalesceTraversals(final Callable<V> callable) throws InvocationTargetException {
        V result = null;
        Exception e = null;
        try {
            result = callable.call();
        } catch (final Exception ex) {
            e = ex;
        }
        if (e != null) {
            throw new InvocationTargetException(e);
        } else {
            return result;
        }
    }

    private void notifyBaseIndexChangeListeners(final boolean baseIndexChanged) {
        for (ViatraBaseIndexChangeListener listener : this.changeListeners) {
            if (!listener.onlyOnIndexChange() || baseIndexChanged) {
                listener.notifyChanged(baseIndexChanged);
            }
        }
    }

    @Override
    public void addBaseIndexChangeListener(final ViatraBaseIndexChangeListener listener) {
        this.changeListeners.add(listener);
    }

    @Override
    public void removeBaseIndexChangeListener(final ViatraBaseIndexChangeListener listener) {
        this.changeListeners.remove(listener);
    }

    @Override
    public void resampleDerivedFeatures() {

    }

    @Override
    public boolean addIndexingErrorListener(final IIndexingErrorListener listener) {
        return false;
    }

    @Override
    public boolean removeIndexingErrorListener(final IIndexingErrorListener listener) {
        return false;
    }

    @Override
    public boolean addInstanceObserver(final IInstanceObserver observer, final Object observedObject) {
        return false;
    }

    @Override
    public boolean removeInstanceObserver(final IInstanceObserver observer, final Object observedObject) {
        return false;
    }

    private <K, V extends IInstanceListener> void addInstanceListener(
            final K type,
            final V listener,
            final Map<K, Set<V>> listenerMap) {
        listenerMap.compute(type, (k, v) -> {
            if (v == null) {
                v = new HashSet<>();
            }
            v.add(listener);
            return v;
        });
    }

    private <K, V extends IInstanceListener> void removeInstanceListener(
            final K type,
            final V listener,
            final Map<K, Set<V>> listenerMap) {
        listenerMap.compute(type, (k, v) -> {
            if (v == null) {
                throw new RuntimeException("No listeners registered for type " + type + "!");
            }
            v.remove(listener);
            if (v.isEmpty()) {
                return null;
            } else {
                return v;
            }
        });
    }

    void addLinkedTypeInstanceListener(final LinkedType type, final INodeTypeInstanceListener listener) {
        addInstanceListener(type, listener, this.linkedTypeInstancesListener);
    }

    void removeLinkedTypeInstanceListener(final LinkedType type, final INodeTypeInstanceListener listener) {
        removeInstanceListener(type, listener, this.linkedTypeInstancesListener);
    }

    void addPrimitiveTypeInstanceListener(final PrimitiveType type, final IDataTypeInstanceListener listener) {
        addInstanceListener(type, listener, this.primitiveTypeInstanceListeners);
    }

    void removePrimitiveTypeInstanceListener(final PrimitiveType type, final IDataTypeInstanceListener listener) {
        removeInstanceListener(type, listener, this.primitiveTypeInstanceListeners);
    }

    void addLinkInstanceListener(final Link type, final INodeLinkInstanceListener listener) {
        addInstanceListener(type, listener, this.linkInstanceListeners);
    }

    void removeLinkInstanceListener(final Link type, final INodeLinkInstanceListener listener) {
        removeInstanceListener(type, listener, this.linkInstanceListeners);
    }
}
