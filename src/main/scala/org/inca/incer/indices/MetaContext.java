package org.inca.incer.indices;

import org.eclipse.viatra.query.runtime.matchers.context.AbstractQueryMetaContext;
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey;
import org.eclipse.viatra.query.runtime.matchers.context.InputKeyImplication;
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey;
import org.inca.incer.indices.InputKey.DataTypeKey;
import org.inca.incer.indices.InputKey.NodeLinkKey;
import org.inca.incer.indices.InputKey.NodeTypeKey;
import org.inca.meta.MetaElements;
import org.inca.meta.MetaElements.DataType;
import org.inca.meta.MetaElements.NodeLink;
import org.inca.meta.MetaElements.NodeType;

import java.lang.reflect.Field;
import java.util.*;

public class MetaContext extends AbstractQueryMetaContext {

    @Override
    public boolean isEnumerable(final IInputKey key) {
        return true;
    }

    @Override
    public boolean isStateless(final IInputKey key) {
        return false;
    }

    @Override
    public Map<Set<Integer>, Set<Integer>> getFunctionalDependencies(final IInputKey key) {
        if (key instanceof NodeLinkKey) {
            final Field field = ((NodeLinkKey) key).type.fld();
            final Class<?> type = field.getType();
            final Map<Set<Integer>, Set<Integer>> result = new HashMap<>();

            // this is a "to one" link: requirement is that the type of the link is not a collection
            if (!Collection.class.isAssignableFrom(type)) {
                result.put(Collections.singleton(0), Collections.singleton(1));
            }

            // this is a "one to" link: requirement is that the link is either a containment or a one-to-many reference
            if (false /*isConceptFeatureMultiplicityOneTo(conceptFeature)*/) {
                result.put(Collections.singleton(1), Collections.singleton(0));
            }

            return result;
        } else {
            return Collections.emptyMap();
        }
    }

    @Override
    public Collection<InputKeyImplication> getImplications(final IInputKey key) {
        final Collection<InputKeyImplication> result = new HashSet<>();

        if (key instanceof NodeTypeKey || key instanceof JavaTransitiveInstancesKey) {
            Class<?> clazz = null;
            if (key instanceof NodeTypeKey) {
                clazz = ((NodeTypeKey) key).type.cls();
            } else {
                clazz = ((JavaTransitiveInstancesKey) key).getInstanceClass();
            }

            if (clazz != null) {
                // resolution successful
                // direct Java superClass
                final Class<?> superClass = clazz.getSuperclass();
                if (superClass != null) {
                    final JavaTransitiveInstancesKey impliedSuper = new JavaTransitiveInstancesKey(superClass);
                    result.add(new InputKeyImplication(key, impliedSuper, Collections.singletonList(0)));
                }
                // direct Java superInterfaces
                for (Class<?> superInterface : clazz.getInterfaces()) {
                    if (superInterface != null) {
                        final JavaTransitiveInstancesKey impliedInterface = new JavaTransitiveInstancesKey(superInterface);
                        result.add(new InputKeyImplication(key, impliedInterface, Collections.singletonList(0)));
                    }
                }
            }
        } else if (key instanceof NodeLinkKey) {
            final NodeLink nodeLink = ((NodeLinkKey) key).type;
            final NodeTypeKey impliedSource = new NodeTypeKey(nodeLink.nodeType());
            final Class<?> fieldType = nodeLink.fld().getType();
            IInputKey impliedTarget = null;

            if (MetaElements.isDataType(fieldType)) {
                impliedTarget = new DataTypeKey(new DataType(fieldType));
            } else {
                impliedTarget = new NodeTypeKey(new NodeType(fieldType));
            }

            result.add(new InputKeyImplication(key, impliedSource, Collections.singletonList(0)));
            result.add(new InputKeyImplication(key, impliedTarget, Collections.singletonList(1)));
        } else if (key instanceof DataTypeKey) {
            final Class<?> clazz = ((DataTypeKey) key).type.cls();
            final JavaTransitiveInstancesKey implied = new JavaTransitiveInstancesKey(clazz);
            result.add(new InputKeyImplication(key, implied, Collections.singletonList(0)));
        }

        return result;
    }
}
