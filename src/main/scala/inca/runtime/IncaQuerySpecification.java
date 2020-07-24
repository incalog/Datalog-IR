package inca.runtime;


import org.eclipse.viatra.query.runtime.api.GenericPatternMatcher;
import org.eclipse.viatra.query.runtime.api.GenericQuerySpecification;
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PQuery;

public abstract class IncaQuerySpecification extends GenericQuerySpecification<GenericPatternMatcher> {

    public IncaQuerySpecification(final PQuery query) {
        super(query);
    }

    public IncaQuerySpecification() {
        this(null);
    }

    @Override
    public GenericPatternMatcher instantiate() {
        return new GenericPatternMatcher(this);
    }

}