package inca.backend.indices;


import org.eclipse.viatra.query.runtime.matchers.backend.IMatcherCapability;
import org.eclipse.viatra.query.runtime.matchers.backend.IQueryBackend;
import org.eclipse.viatra.query.runtime.matchers.backend.IQueryBackendFactory;
import org.eclipse.viatra.query.runtime.matchers.backend.QueryEvaluationHint;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryBackendContext;
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PQuery;

public class DummySearchBackendFactory implements IQueryBackendFactory {


    public static final DummySearchBackendFactory INSTANCE = new DummySearchBackendFactory();


    private DummySearchBackendFactory() {

    }

    @Override
    public IQueryBackend create(IQueryBackendContext context) {
        return null;
    }

    @Override
    public Class<? extends IQueryBackend> getBackendClass() {
        return null;
    }

    @Override
    public IMatcherCapability calculateRequiredCapability(final PQuery query, final QueryEvaluationHint hint) {
        return null;
    }

    @Override
    public boolean isCaching() {
        return false;
    }

}