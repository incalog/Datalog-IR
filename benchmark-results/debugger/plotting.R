wd <- getwd()
datapath <- paste(wd, "benchmark-results/debugger/data", sep="/")
graphpath <- paste(wd, "benchmark-results/debugger/graphs", sep="/")

read <- function(fileName) {
  csv <- read.csv2(paste(datapath, fileName, sep="/"), sep = ",", dec = ".")
  return(csv)
}


readMeasurement <- function(fileName) {
  csv <- read(fileName)
  vals <- csv$measurement
  valsInMs <- nsToMs(vals)
  return(valsInMs)
}

readSteps <- function(fileName) {
  csv <- read(fileName)
  vals <- csv$numSteps
  return(vals)
}

readTime <- function(fileName) {
  csv <- read(fileName)
  return(nsToMs(csv))
}

nsToMs <- function(ns) {
  ns / 1000000
}

### VarPointsTo Step Into

methodLookupUnOptAll <-           readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_ALL.csv")
methodLookupUnOptAtomEDB <-       readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomEDB.csv")
methodLookupUnOptAtomInto <-      readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomInto.csv")
methodLookupUnOptAtomSkip <-      readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomSkip.csv")
methodLookupUnOptAtomPrimitive <- readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomPrimitive.csv")
methodLookupUnOptRuleMerge <-     readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_RuleMerge.csv")
methodLookupUnOptRuleResult <-    readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_RuleResult.csv")
methodLookupUnOptQueryUnion <-    readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryUnion.csv")
methodLookupUnOptQueryIterate <-  readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryIterate.csv")
methodLookupUnOptQueryStable <-   readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryStable.csv")
methodLookupUnOptQueryResult <-   readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryResult.csv")

subtypeOfUnOptAll <-              readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_ALL.csv")
subtypeOfUnOptAtomEDB <-          readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomEDB.csv")
subtypeOfUnOptAtomInto <-         readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomInto.csv")
subtypeOfUnOptAtomSkip <-         readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomSkip.csv")
subtypeOfUnOptAtomEq <-           readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomEq.csv")
subtypeOfUnOptRuleMerge <-        readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_RuleMerge.csv")
subtypeOfUnOptRuleResult <-       readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_RuleResult.csv")
subtypeOfUnOptQueryUnion <-       readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryUnion.csv")
subtypeOfUnOptQueryIterate <-     readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryIterate.csv")
subtypeOfUnOptQueryStable <-      readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryStable.csv")
subtypeOfUnOptQueryResult <-      readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryResult.csv")

varPointsToUnOptAll <-            readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_ALL.csv")
varPointsToUnOptAtomEDB <-        readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomEDB.csv")
varPointsToUnOptAtomInto <-       readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomInto.csv")
varPointsToUnOptAtomSkip <-       readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomSkip.csv")
varPointsToUnOptAtomEq <-         readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomEq.csv")
varPointsToUnOptAtomNeq <-        readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomNeq.csv")
varPointsToUnOptAtomPrim <-       readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomPrimitive.csv")
varPointsToUnOptRuleMerge <-      readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_RuleMerge.csv")
varPointsToUnOptRuleResult <-     readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_RuleResult.csv")
varPointsToUnOptQueryUnion <-     readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryUnion.csv")
varPointsToUnOptQueryIterate <-   readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryIterate.csv")
varPointsToUnOptQueryStable <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryStable.csv")
varPointsToUnOptQueryResult <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryResult.csv")

combinedUnOptAll <-               c(varPointsToUnOptAll, subtypeOfUnOptAll, methodLookupUnOptAll)
combinedUnOptAtomEDB <-           c(varPointsToUnOptAtomEDB, subtypeOfUnOptAtomEDB, methodLookupUnOptAtomEDB)
combinedUnOptAtomInto <-          c(varPointsToUnOptAtomInto, subtypeOfUnOptAtomInto, methodLookupUnOptAtomInto)
combinedUnOptAtomSkip <-          c(varPointsToUnOptAtomSkip, subtypeOfUnOptAtomSkip, methodLookupUnOptAtomSkip)
combinedUnOptAtomEq <-            c(varPointsToUnOptAtomEq, subtypeOfUnOptAtomEq)
combinedUnOptAtomNeq <-           c(varPointsToUnOptAtomNeq)
combinedUnOptAtomPrim <-          c(varPointsToUnOptAtomPrim, methodLookupUnOptAtomPrimitive)
combinedUnOptRuleMerge <-         c(varPointsToUnOptRuleMerge, subtypeOfUnOptRuleMerge, methodLookupUnOptRuleMerge)
combinedUnOptRuleResult <-        c(varPointsToUnOptRuleResult, subtypeOfUnOptRuleResult, methodLookupUnOptRuleResult)
combinedUnOptQueryUnion <-        c(varPointsToUnOptQueryUnion, subtypeOfUnOptQueryUnion, methodLookupUnOptQueryUnion)
combinedUnOptQueryIterate <-      c(varPointsToUnOptQueryIterate, subtypeOfUnOptQueryIterate, methodLookupUnOptQueryIterate)
combinedUnOptQueryStable <-       c(varPointsToUnOptQueryStable, subtypeOfUnOptQueryStable, methodLookupUnOptQueryStable)
combinedUnOptQueryResult <-       c(varPointsToUnOptQueryResult, subtypeOfUnOptQueryResult, methodLookupUnOptQueryResult)

color1 <- rgb(255/256, 255/256, 204/256)
color2 <- rgb(161/256, 218/256, 180/256)
color3 <- rgb(65/256, 182/256, 196/256)
color4 <- rgb(34/256, 94/256, 168/256)


options(scipen=999)
pdf(file = paste(graphpath, "MethodLookup-Into-UnOpt.pdf", sep="/"))
boxplot(methodLookupUnOptRuleMerge, methodLookupUnOptRuleResult, methodLookupUnOptAtomEDB, methodLookupUnOptAtomPrimitive, methodLookupUnOptAtomInto, methodLookupUnOptAtomSkip, methodLookupUnOptQueryUnion, methodLookupUnOptQueryStable, methodLookupUnOptQueryIterate, methodLookupUnOptQueryResult, methodLookupUnOptAll,
        main = "MethodLookup",
        ylab = "Time per step in milliseconds",
        las = 2,
        log = "y",
        outline = FALSE,
        # ylim = c(0.001, 350),
        col = color1
)
dev.off()

pdf(file = paste(graphpath, "SubtypeOf-Into-UnOpt.pdf", sep="/"))
boxplot(subtypeOfUnOptRuleMerge, subtypeOfUnOptRuleResult, subtypeOfUnOptAtomEDB, subtypeOfUnOptAtomEq, subtypeOfUnOptAtomInto, subtypeOfUnOptAtomSkip, subtypeOfUnOptQueryUnion, subtypeOfUnOptQueryStable, subtypeOfUnOptQueryIterate, subtypeOfUnOptQueryResult, subtypeOfUnOptAll,
        main = "SubtypeOf",
        ylab = "Time per step in milliseconds",
        names = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
        las = 2,
        log = "y",
        outline = FALSE,
        col = color1
        # ylim = c(0.001, 350),
)
dev.off()

pdf(file = paste(graphpath, "VarPointsTo-Into-UnOpt.pdf", sep="/"))
boxplot(varPointsToUnOptRuleMerge, varPointsToUnOptRuleResult, varPointsToUnOptAtomEDB, varPointsToUnOptAtomEq, varPointsToUnOptAtomNeq, varPointsToUnOptAtomPrim, varPointsToUnOptAtomInto, varPointsToUnOptAtomSkip, varPointsToUnOptQueryUnion, varPointsToUnOptQueryStable, varPointsToUnOptQueryIterate, varPointsToUnOptQueryResult, varPointsToUnOptAll,
        main = "VarPointsTo",
        ylab = "Time per step in milliseconds",
        names = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Neq", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
        las = 2,
        log = "y",
        outline = FALSE,
        col = color1
        # ylim = c(0.001, 350),
)
dev.off()

pdf(file = paste(graphpath, "All-Into-UnOpt.pdf", sep="/"))
boxplot(combinedUnOptRuleMerge, combinedUnOptRuleResult, combinedUnOptAtomEDB, combinedUnOptAtomEq, combinedUnOptAtomNeq, combinedUnOptAtomPrim, combinedUnOptAtomInto, combinedUnOptAtomSkip, combinedUnOptQueryUnion, combinedUnOptQueryStable, combinedUnOptQueryIterate, combinedUnOptQueryResult, combinedUnOptAll,
        main = "(A) Reduction rule running times when only using step-into",
        ylab = "Time per reduction step (ms) as log scale",
        yaxt = "n",
        xaxt = "n",
        log = "y",
        outline = FALSE,
        col = c(rep("lightgrey", times = 12), "grey40")
        # ylim = c(0.001, 350),
        # boxwex = 0.5
)
axis(2, at = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), labels = c(0.001, 0.01, 0.1, 1, 10, 100, 1000))
axis(1, las = 2, at = c(1:13), labels = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Neq", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"))
dev.off()


### VarPointsTo Step Over

scenario1UnOptOver <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_HybridSemantics_IntoVarPointsTo_Over.csv")
scenario2UnOptOver <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_HybridSemantics_IntoVarPointsToAndStaticFieldPointsTo_Over.csv")
scenario3UnOptOver <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_HybridSemantics_IntoVarPointsToAndInstanceFieldPointsTo_Over.csv")
scenario4UnOptOver <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_HybridSemantics_IntoVarPointsToAndReachable_Over.csv")

pdf(file = paste(graphpath, "VarPointsTo-Over-UnOpt.pdf", sep="/"))
boxplot(scenario1UnOptOver, scenario2UnOptOver, scenario3UnOptOver, scenario4UnOptOver,
        main = "(B) Running times of step-over in scenarios (iiia-iiid)",
        ylab = "Time per reduction step (ms)",
        names = c("Scenario (iiia)", "Scenario (iiib)", "Scenario (iiic)", "Scenario (iiid)"),
        # las = 2,
        # log = "y",
        outline = FALSE,
        boxwex = 0.25,
        col = c(color1, color2, color3, color4)
        # ylim = c(0.001, 350),
)
dev.off()

### Path Into/Over/Bottom-Up Time
pathBUTime <- colMeans(readTime("Path-BUTime.csv"))
pathIntoTime <- colMeans(readTime("Path-IntoTime-UnOpt.csv"))
pathOverTime <- colMeans(readTime("Path-OverTime-UnOpt.csv"))

pdf(file = paste(graphpath, "Path-Time-UnOpt.pdf", sep="/"))
plot(data.matrix(pathOverTime),
     main = "(A) Path program running times until termination",
     type = "o",
     col = color2,
     ylab = "Running time (ms)",
     xlab = "X",
     xaxt = "n",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     ylim = c(0, 60000),
     lwd = 1.5
)
lines(data.matrix(pathIntoTime), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(pathBUTime), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:100), labels = seq(10, 1000, by = 10))
legend("topright", legend=c("step-into", "step-over", "bottom-up"),
       col=c(color3, color2, color4), lty=1:1, lwd = 3)
dev.off()

# Path Into/Over Steps

pathIntoSteps <- colMeans(read("Path-IntoSteps-UnOpt.csv"))
pathOverSteps <- colMeans(read("Path-OverSteps-UnOpt.csv"))

pdf(file = paste(graphpath, "Path-Steps-UnOpt.pdf", sep="/"))
plot(data.matrix(pathOverSteps),
     main = "(B) Path program number of steps until termination",
     type = "o",
     col = color2,
     ylab = "# Steps",
     xlab = "X",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 120000),
     lwd = 1.5
)
lines(data.matrix(pathIntoSteps), type = "o", col = color3, lwd = 1.5)
axis(1, at = c(1:100), labels = seq(10, 1000, by = 10))
legend("topright", legend=c("step-into", "step-over"),
       col=c(color3, color2), lty=1:1, lwd=3)
dev.off()
