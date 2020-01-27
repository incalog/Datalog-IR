package org.inca.incer.indices;

import org.apache.log4j.Logger;
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.scope.IBaseIndex;
import org.eclipse.viatra.query.runtime.api.scope.IEngineContext;
import org.eclipse.viatra.query.runtime.api.scope.IIndexingErrorListener;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContext;
import org.inca.incer.Incrementalizable;

public class IncAQueryScope extends org.eclipse.viatra.query.runtime.api.scope.QueryScope {

    private final Incrementalizable root;

    public IncAQueryScope(final Incrementalizable root) {
        this.root = root;
    }

    @Override
    protected IEngineContext createEngineContext(final ViatraQueryEngine engine,
                                                 final IIndexingErrorListener errorListener, final Logger logger) {
        return null;
    }

    public static final class IncAEngineContext implements IEngineContext {
        @Override
        public IBaseIndex getBaseIndex() {
            return null;
        }

        @Override
        public void dispose() {

        }

        @Override
        public IQueryRuntimeContext getQueryRuntimeContext() {
            return null;
        }
    }

}
