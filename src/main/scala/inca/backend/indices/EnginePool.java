package inca.backend.indices;


import org.eclipse.viatra.query.runtime.api.*;
import org.eclipse.viatra.query.runtime.api.scope.QueryScope;
import org.eclipse.viatra.query.runtime.exception.ViatraQueryException;
import org.eclipse.viatra.query.runtime.matchers.backend.IQueryBackendFactory;

import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.Map;
import java.util.WeakHashMap;

public class EnginePool {

    private static final Map<QueryScope, WeakReference<AdvancedViatraQueryEngine>> engineMap =
            new WeakHashMap<>();

    public static ViatraQueryMatcher<IPatternMatch> getMatcher(final GenericQuerySpecification specification,
                                                               final QueryScope scope,
                                                               final IQueryBackendFactory backendFactory) {
        try {
            final WeakReference<AdvancedViatraQueryEngine> engineReference = EnginePool.engineMap.get(scope);
            AdvancedViatraQueryEngine engine;

            if (engineReference == null || engineReference.get() == null) {
                final ViatraQueryEngineOptions options = ViatraQueryEngineOptions.
                        defineOptions().
                        withDefaultBackend(backendFactory).
                        withDefaultCachingBackend(backendFactory).
                        withDefaultSearchBackend(DummySearchBackendFactory.INSTANCE).build();
                engine = AdvancedViatraQueryEngine.createUnmanagedEngine(scope, options);
                EnginePool.engineMap.put(scope, new WeakReference<AdvancedViatraQueryEngine>(engine));
            } else {
                engine = engineReference.get();
            }

            return engine.getMatcher(specification, null);
        } catch (ViatraQueryException e) {
            e.printStackTrace();
            return null;
        }
    }

    public static Collection<WeakReference<AdvancedViatraQueryEngine>> getEngines() {
        return EnginePool.engineMap.values();
    }

    public static void disposeAllEngines() {
        for (WeakReference<AdvancedViatraQueryEngine> ref : EnginePool.engineMap.values()) {
            final AdvancedViatraQueryEngine engine = ref.get();
            if (engine != null) {
                System.err.println("Disposing engine " + engine);
                engine.dispose();
            }
        }
        EnginePool.engineMap.clear();
    }

}