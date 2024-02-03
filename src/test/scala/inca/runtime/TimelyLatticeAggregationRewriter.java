//package inca.runtime;
//
//public final class TimelyLatticeAggregationRewriter extends FunPatternRewriter {
//
//
//  public static void rewriteModel(final model model, final boolean withDoubleAggregation) {
//    final set<node<IPattern>> aggregatingPatterns = new hashset<node<IPattern>>;
//    foreach oldPattern in model.nodes(IPattern).where({~it => it.@costConsistent == null; }) {
//      if (demandCreateWrappedPattern(oldPattern)) {
//        aggregatingPatterns.add(oldPattern);
//      }
//    }
//
//    if (withDoubleAggregation) {
//      final Graph<node<IPattern>> callGraph = TimelyLatticeAggregationRewriter.createCallGraph(model);
//      final Set<Set<node<IPattern>>> sccs = SCC.computeSCC(callGraph).getSccs();
//
//      foreach oldPattern in aggregatingPatterns {
//        TimelyLatticeAggregationRewriter.demandCreateDoubleAggregatingPattern(oldPattern, model, callGraph, sccs);
//      }
//    }
//  }
//
//
//  public static void demandCreateDoubleAggregatingPattern(final node<IPattern> wrapperPattern, final model model, final Graph<node<IPattern>> callGraph, final Set<Set<node<IPattern>>> sccs) {
//    Set<node<IPattern>> currentSCC = null;
//    foreach scc in sccs {
//      if (scc.contains(wrapperPattern)) {
//        currentSCC = scc;
//        break;
//      }
//    }
//    assert currentSCC != null;
//
//    node<IPattern> wrappedPattern = wrapperPattern.copy;
//    wrappedPattern.name = wrappedPattern.name + "_Collecting";
//
//    // collectthecallsthatwillneedtoberedirectedbeforeweactuallydoanyrewriting
//    foreach call in model.nodes(IPatternCall).where({~it => it.pattern == wrapperPattern; }) {
//      final node<IPattern> containerPattern = call.ancestor<concept = IPattern>;
//      if (currentSCC.contains(containerPattern)) {
//        // weonlyneedtousethewrappedpatternifthecallisinsideofthesccoftheoriginalpattern
//        call.pattern = wrappedPattern;
//      }
//    }
//
//    wrapperPattern.bodies.clear;
//    wrapperPattern.bodies.add(createAggregatingPatternBody(wrapperPattern, wrappedPattern));
//    wrapperPattern.add next-sibling(wrappedPattern);
//  }
//
//
//  private static boolean demandCreateWrappedPattern(final node<IPattern> wrapperPattern) {
//    final list<node<IParameter>> allWrapperPatternParameters = wrapperPattern.getAllParameters().toList;
//    final sequence<node<IParameter>> allWrapperPatternParametersWithSynthesisedDataType = allWrapperPatternParameters.where({~it => it.type.isInstanceOf(TypeConstructorTypeWrapper); });
//    final sequence<node<IParameter>> allWrapperPatternParametersWithSynthesisedDataTypeAndAggregation = allWrapperPatternParametersWithSynthesisedDataType.where({~it => it.type:TypeConstructorTypeWrapper.getLatticeOperation() != null; });
//
//    if (allWrapperPatternParametersWithSynthesisedDataTypeAndAggregation.size > 1) { throw new IllegalArgumentException("At most one aggregation over lattice values can occur in a pattern!"); }
//
//    final node<IParameter> wrapperPatternAggregatedParameter = allWrapperPatternParametersWithSynthesisedDataTypeAndAggregation.first;
//
//    if (wrapperPatternAggregatedParameter != null) {
//      final node<IPattern> wrappedPattern = wrapperPattern.copy;
//      wrappedPattern.name = wrappedPattern.name + "_Wrapped";
//      wrapperPattern.bodies.clear;
//      wrapperPattern.bodies.add(createAggregatingPatternBody(wrapperPattern, wrappedPattern));
//
//      // redirectoldcallsinthewrappedpattern
//      foreach call in wrappedPattern.descendants<concept = IPatternCall>.where({~it => it.pattern == wrappedPattern; }) {
//        call.pattern = wrapperPattern;
//      }
//
//      wrapperPattern.add next-sibling(wrappedPattern);
//      return true;
//    } else {
//      return false;
//    }
//  }
//
//
//  private static node<IPatternBody> createAggregatingPatternBody(final node<IPattern> wrapperPattern, final node<IPattern> wrappedPattern) {
//    final node<IPatternBody> body = new node<GraphPatternBody>();
//    final list<node<IParameter>> allWrapperPatternParameters = wrapperPattern.getAllParameters().toList;
//    final sequence<node<IParameter>> allWrapperPatternParametersWithSynthesisedDataType = allWrapperPatternParameters.where({~it => it.type.isInstanceOf(TypeConstructorTypeWrapper); });
//    final sequence<node<IParameter>> allWrapperPatternParametersWithSynthesisedDataTypeAndAggregation = allWrapperPatternParametersWithSynthesisedDataType.where({~it => it.type:TypeConstructorTypeWrapper.getLatticeOperation() != null; });
//    final node<IParameter> wrapperPatternAggregatedParameter = allWrapperPatternParametersWithSynthesisedDataTypeAndAggregation.first;
//
//    // setuppatterncallthatavoidstheblowupwithjoinnodes
//    final node<PatternCompositionConstraint> findConstraint = new node<PatternCompositionConstraint>();
//    final node<IPatternCall> findCall = new node<PatternCall>();
//    findCall.pattern = wrappedPattern;
//
//    foreach currentWrappedPatternParameter in wrapperPattern.getAllParameters() {
//      if (currentWrappedPatternParameter.type.isInstanceOf(TypeConstructorTypeWrapper) && wrapperPatternAggregatedParameter != null) {
//        final node<TemporaryVariable> var = new node<TemporaryVariable>();
//        var.name = "_";
//        findCall.arguments.add(var);
//      } else {
//        final int index = DRedLatticeAggregationRewriter.getIndexWithoutFunctionalDependencyParameters(currentWrappedPatternParameter, wrapperPattern.getAllParameters());
//        final node<IParameter> resultParameter = allWrapperPatternParameters.get(index);
//        final node<VariableReference> ref = new node<VariableReference>();
//        ref.variable = resultParameter;
//        findCall.arguments.add(ref);
//      }
//    }
//
//    findConstraint.patternCall = findCall;
//    body.contents.add(findConstraint);
//
//    // setupaggregator
//    final node<IPatternCall> aggregatorCall = new node<PatternCall>();
//    aggregatorCall.pattern = wrappedPattern;
//
//    foreach currentWrappedPatternParameter in wrapperPattern.getAllParameters() {
//      if (currentWrappedPatternParameter.type.isInstanceOf(TypeConstructorTypeWrapper)) {
//        final node<TemporaryVariable> var = new node<TemporaryVariable>();
//        var.name = "_";
//        aggregatorCall.arguments.add(var);
//
//        final int wrappedIndex = DRedLatticeAggregationRewriter.getIndexWithoutFunctionalDependencyParameters(currentWrappedPatternParameter, wrapperPattern.getAllParameters());
//        final int resultIndex = DRedLatticeAggregationRewriter.getIndexWithoutFunctionalDependencyParameters(wrapperPatternAggregatedParameter, wrapperPattern.getAllParameters());
//
//        if (resultIndex == wrappedIndex) {
//          var.@marked = new node<AggregatedValueMarker>();
//        }
//      } else {
//        final int index = DRedLatticeAggregationRewriter.getIndexWithoutFunctionalDependencyParameters(currentWrappedPatternParameter, wrapperPattern.getAllParameters());
//        final node<IParameter> resultParameter = allWrapperPatternParameters.get(index);
//        final node<VariableReference> ref = new node<VariableReference>();
//        ref.variable = resultParameter;
//        aggregatorCall.arguments.add(ref);
//      }
//    }
//
//    final node<GraphPatternCompareConstraint> compareConstraint = new node<GraphPatternCompareConstraint>();
//    final node<VariableReference> ref = new node<VariableReference>();
//    ref.variable = wrapperPatternAggregatedParameter;
//
//    compareConstraint.left = ref;
//    final node<TypeConstructorTypeWrapper> wrapper = wrapperPatternAggregatedParameter.type:TypeConstructorTypeWrapper;
//    final node<ILatticeDefinitionModule> lattice = wrapper.type.constructor as ILatticeDefinitionModule;
//    final node<ILatticeElementCombinator> combinator = wrapper.getLatticeOperation();
//    combinator as JavaMethodCombinator.lattice = lattice;
//
//    compareConstraint.right = <AggregatedValue(
//      aggregator: LatticeAggregator(operation: # combinator),
//      call: # aggregatorCall
//    )>;
//
//    body.contents.add(compareConstraint);
//
//    return body;
//  }
//
//}