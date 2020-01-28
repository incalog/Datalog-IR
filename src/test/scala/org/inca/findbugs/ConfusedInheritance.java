package org.inca.findbugs;

import org.eclipse.viatra.query.runtime.api.GenericPatternMatcher;
import org.eclipse.viatra.query.runtime.api.GenericQuerySpecification;
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.scope.QueryScope;
import org.eclipse.viatra.query.runtime.exception.ViatraQueryException;
import org.eclipse.viatra.query.runtime.matchers.psystem.PBody;
import org.eclipse.viatra.query.runtime.matchers.psystem.PVariable;
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.Equality;
import org.eclipse.viatra.query.runtime.matchers.psystem.basicdeferred.ExportedParameter;
import org.eclipse.viatra.query.runtime.matchers.psystem.basicenumerables.TypeConstraint;
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.BasePQuery;
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PParameter;
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PVisibility;
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.QueryInitializationException;
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples;
import org.inca.incer.indices.TFInputKey;
import org.inca.incer.indices.TFQueryScope;
import org.inca.incer.indices.TFQuerySpecification;
import org.inca.meta.MetaElements;

import java.util.*;

/**
 * Class is final but declares protected field.
 */
@SuppressWarnings("all")
public final class ConfusedInheritance extends TFQuerySpecification {

    private ConfusedInheritance() {
        super(GeneratedPQuery.INSTANCE);
    }

    @Override
    protected GenericPatternMatcher instantiate(final ViatraQueryEngine engine) throws ViatraQueryException {
        GenericPatternMatcher matcher = engine.getExistingMatcher(this);
        if (matcher == null) {
            matcher = engine.getMatcher(this);
        }
        return matcher;
    }

    @Override
    public Class<? extends QueryScope> getPreferredScopeClass() {
        return TFQueryScope.class;
    }

    public static ConfusedInheritance instance() {
        return LazyHolder.INSTANCE;
    }

    private static final class LazyHolder {

        private static final ConfusedInheritance INSTANCE = make();

        public static ConfusedInheritance make() {
            return new ConfusedInheritance();
        }

    }

    private static final class GeneratedPQuery extends BasePQuery {

        private final PParameter p_class = new PParameter("class",
                new MetaElements.NodeType(ClassDeclaration.class).toString(), new TFInputKey.NodeTypeKey(new MetaElements.NodeType(ClassDeclaration.class)));

        private static final GeneratedPQuery INSTANCE = new GeneratedPQuery();

        public GeneratedPQuery() {
            super(PVisibility.PUBLIC);
        }

        @Override
        protected Set<PBody> doGetContainedBodies() throws QueryInitializationException {
            final Set<PBody> bodies = new HashSet<>();

            {
                final PBody body = new PBody(this);
                final PVariable var_class = body.getOrCreateVariableByName("class");

                final List<ExportedParameter> exportedParameters = new ArrayList<ExportedParameter>();
                exportedParameters.add(new ExportedParameter(body, var_class, p_class));
                body.setSymbolicParameters(exportedParameters);

                final PVariable var__tmp_1 = body.getOrCreateVariableByName("tmp_1");
                final PVariable var__tmp_2 = body.getOrCreateVariableByName("tmp_2");
                final PVariable var__tmp_3 = body.getOrCreateVariableByName("tmp_3");
                final PVariable var__tmp_4 = body.getOrCreateVariableByName("tmp_4");
                final PVariable var__3909214783375021923 = body.newConstantVariable(true);
                final PVariable var__member = body.getOrCreateVariableByName("member");

                new TypeConstraint(body, Tuples.flatTupleOf(var_class),
                        new TFInputKey.NodeTypeKey(new MetaElements.NodeType(ClassDeclaration.class)));
                new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_class, var__tmp_1),
                        new TFInputKey.NodeLinkKey(new MetaElements.NodeType(ClassDeclaration.class).apply("isFinal")));
                new Equality(body, var__tmp_2, var__3909214783375021923);
                new Equality(body, var__tmp_1, var__tmp_2);
                new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var_class, var__tmp_3),
                        new TFInputKey.NodeLinkKey(new MetaElements.NodeType(ClassDeclaration.class).apply("members")));
                new Equality(body, var__member, var__tmp_3);
                new TypeConstraint(body, Tuples.flatTupleOf(var__member),
                        new TFInputKey.NodeTypeKey(new MetaElements.NodeType(FieldDeclaration.class)));
                new TypeConstraint(body, Tuples.staticArityFlatTupleOf(var__member, var__tmp_4),
                        new TFInputKey.NodeLinkKey(new MetaElements.NodeType(FieldDeclaration.class).apply("visibility")));
                new TypeConstraint(body, Tuples.flatTupleOf(var__tmp_4),
                        new TFInputKey.NodeTypeKey(new MetaElements.NodeType(ProtectedVisibility.class)));

                bodies.add(body);
            }

            return bodies;
        }

        @Override
        public String getFullyQualifiedName() {
            return "ConfusedInheritance";
        }

        @Override
        public List<PParameter> getParameters() {
            return Arrays.asList(p_class);
        }

        @Override
        public List<String> getParameterNames() {
            return Arrays.asList("class");
        }

    }

}
