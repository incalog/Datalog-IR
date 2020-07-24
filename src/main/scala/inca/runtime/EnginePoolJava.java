package inca.runtime;


import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine;
import org.eclipse.viatra.query.runtime.api.IQuerySpecification;
import org.eclipse.viatra.query.runtime.api.ViatraQueryMatcher;
import org.eclipse.viatra.query.runtime.api.scope.QueryScope;
import org.eclipse.viatra.query.runtime.matchers.backend.IQueryBackendFactory;

import java.lang.ref.WeakReference;
import java.util.Collection;

/**
 * Wraps the Scala implementation in inca.EnginePool
 */
public class EnginePoolJava {
    public static <Matcher extends ViatraQueryMatcher<?>> Matcher getMatcher(
    		final IQuerySpecification<Matcher> specification,
            final QueryScope scope,
            final IQueryBackendFactory backendFactory) {
    	return EnginePool.getMatcher(specification, scope, backendFactory);
    }

    public static Collection<WeakReference<AdvancedViatraQueryEngine>> getEngines() {
        return EnginePool.getEngines();
    }

    public static void disposeAllEngines() {
        EnginePool.disposeAllEngines();
    }
}