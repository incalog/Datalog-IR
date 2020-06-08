package inca.backend.indices;


import org.eclipse.viatra.query.runtime.api.GenericPatternMatcher;
import org.eclipse.viatra.query.runtime.api.GenericQuerySpecification;
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PQuery;

public abstract class TFQuerySpecification extends GenericQuerySpecification<GenericPatternMatcher> {

    public TFQuerySpecification(final PQuery query) {
        super(query);
    }

    public TFQuerySpecification() {
        this(null);
    }

    @Override
    public GenericPatternMatcher instantiate() {
        return new GenericPatternMatcher(this);
    }

}