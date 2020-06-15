package inca.backend.indices;


import inca.backend.virtual.ParentIndex;
import inca.backend.virtual.ParentKey$;
import inca.backend.virtual.VirtualIndex;
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.scope.IEngineContext;
import org.eclipse.viatra.query.runtime.exception.ViatraQueryException;
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContext;

import java.util.*;

public class TFEngineContext implements IEngineContext {

    private final TFQueryScope scope;
    private AdvancedViatraQueryEngine engine;
    private Indices indices;
    private TFRuntimeContext runtimeContext;

    public TFEngineContext(final TFQueryScope scope, final AdvancedViatraQueryEngine engine) {
        this.scope = scope;
        this.engine = engine;
    }

    @Override
    public Indices getBaseIndex() throws ViatraQueryException {
        this.initialize();
        return this.indices;
    }

    @Override
    public void dispose() {
        if (this.indices != null) {
            this.indices.dispose();
        }
        this.engine = null;
        this.indices = null;
    }

    protected void initialize() {
        if (this.runtimeContext == null) {
            Map<String, VirtualIndex> virtualIndices = new HashMap<>();
            virtualIndices.put(ParentKey$.MODULE$.getUniqueID(), new ParentIndex());
            this.indices = new Indices(this.engine, virtualIndices);
            this.runtimeContext = new TFRuntimeContext(this.indices);
        }
    }

    @Override
    public IQueryRuntimeContext getQueryRuntimeContext() throws ViatraQueryException {
        this.initialize();
        return this.runtimeContext;
    }

}