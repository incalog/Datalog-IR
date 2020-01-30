package org.inca.generators.gp.model.psystem

import org.eclipse.viatra.query.runtime.api.GenericQuerySpecification
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PQuery

abstract class ScalaQuerySpecification(query: PQuery) extends GenericQuerySpecification[ScalaPatternMatcher](query)