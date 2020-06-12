package inca.backend.indices;

import org.apache.log4j.Logger;
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.scope.IEngineContext;
import org.eclipse.viatra.query.runtime.api.scope.IIndexingErrorListener;
import truechange.Changeset;

public class TFQueryScope extends org.eclipse.viatra.query.runtime.api.scope.QueryScope {

    // TODO: delete root, add name
    protected final Object root;
    private TFEngineContext engineContext;

    public TFQueryScope(final Object root) {
        this.root = root;
    }

    @Override
    public int hashCode() {
        return this.root.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        } else if (obj == null || obj.getClass() != this.getClass()) {
            return false;
        } else {
            final TFQueryScope that = (TFQueryScope) obj;
            return this.root.equals(that.root);
        }
    }

    @Override
    public String toString() {
        return "IncAQueryScope: " + this.root;
    }

    public TFEngineContext getEngineContext() {
        return this.engineContext;
    }

    @Override
    protected IEngineContext createEngineContext(final ViatraQueryEngine engine, final IIndexingErrorListener listener, final Logger logger) {
        if (this.engineContext == null) {
            this.engineContext = new TFEngineContext(this, (AdvancedViatraQueryEngine) engine);
        }
        return this.engineContext;
    }

}
