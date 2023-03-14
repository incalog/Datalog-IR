
wd <- getwd()
path <- paste(wd, "benchmark-results/debugger", sep="/")

preprocessCSV <- function(fileName) {
  csv <- read.csv2(paste(path, fileName, sep="/"), sep = ",", dec = ".")
  vals <- csv$measurement
  valsInMs <- nsToMs(vals)
  return(valsInMs)
}

nsToMs <- function(ns) {
  ns / 1000000
}

methodLookupAll <-           preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_ALL.csv")
methodLookupAtomEDB <-       preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomEDB.csv")
methodLookupAtomInto <-      preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomInto.csv")
methodLookupAtomSkip <-      preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomSkip.csv")
methodLookupAtomPrimitive <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomPrimitive.csv")
methodLookupRuleMerge <-     preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_RuleMerge.csv")
methodLookupRuleResult <-    preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_RuleResult.csv")
methodLookupQueryUnion <-    preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryUnion.csv")
methodLookupQueryIterate <-  preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryIterate.csv")
methodLookupQueryStable <-   preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryStable.csv")
methodLookupQueryResult <-   preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryResult.csv")

subtypeOfAll <-              preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_ALL.csv")
subtypeOfAtomEDB <-          preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomEDB.csv")
subtypeOfAtomInto <-         preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomInto.csv")
subtypeOfAtomSkip <-         preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomSkip.csv")
subtypeOfAtomEq <-           preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomEq.csv")
subtypeOfRuleMerge <-        preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_RuleMerge.csv")
subtypeOfRuleResult <-       preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_RuleResult.csv")
subtypeOfQueryUnion <-       preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryUnion.csv")
subtypeOfQueryIterate <-     preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryIterate.csv")
subtypeOfQueryStable <-      preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryStable.csv")
subtypeOfQueryResult <-      preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryResult.csv")

varPointsToAll <-            preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_ALL.csv")
varPointsToAtomEDB <-        preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomEDB.csv")
varPointsToAtomInto <-       preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomInto.csv")
varPointsToAtomSkip <-       preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomSkip.csv")
varPointsToAtomEq <-         preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomEq.csv")
varPointsToAtomNeq <-        preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomNeq.csv")
varPointsToAtomPrim <-       preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomPrimitive.csv")
varPointsToRuleMerge <-      preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_RuleMerge.csv")
varPointsToRuleResult <-     preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_RuleResult.csv")
varPointsToQueryUnion <-     preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryUnion.csv")
varPointsToQueryIterate <-   preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryIterate.csv")
varPointsToQueryStable <-    preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryStable.csv")
varPointsToQueryResult <-    preprocessCSV("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryResult.csv")

combinedAll <-               c(varPointsToAll, subtypeOfAll, methodLookupAll)
combinedAtomEDB <-           c(varPointsToAtomEDB , subtypeOfAtomEDB, methodLookupAtomEDB)
combinedAtomInto <-          c(varPointsToAtomInto , subtypeOfAtomInto, methodLookupAtomInto)
combinedAtomSkip <-          c(varPointsToAtomSkip, subtypeOfAtomSkip, methodLookupAtomSkip)
combinedAtomEq <-            c(varPointsToAtomEq, subtypeOfAtomEq)
combinedAtomNeq <-           c(varPointsToAtomNeq)
combinedAtomPrim <-          c(varPointsToAtomPrim, methodLookupAtomPrimitive)
combinedRuleMerge <-         c(varPointsToRuleMerge, subtypeOfRuleMerge, methodLookupRuleMerge)
combinedRuleResult <-        c(varPointsToRuleResult, subtypeOfRuleResult, methodLookupRuleResult)
combinedQueryUnion <-        c(varPointsToQueryUnion, subtypeOfQueryUnion, methodLookupQueryUnion)
combinedQueryIterate <-      c(varPointsToQueryIterate, subtypeOfQueryIterate, methodLookupQueryIterate)
combinedQueryStable <-       c(varPointsToQueryStable, subtypeOfQueryStable, methodLookupQueryStable)
combinedQueryResult <-       c(varPointsToQueryResult, subtypeOfQueryResult, methodLookupQueryResult)

# subtypeOfRes <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_StepInto.csv")

typeCol <- rgb(127/256, 205/256, 187/256)
constantCol <- rgb(44/256, 127/256, 184/256)
taintCol <- rgb(237/256, 248/256, 177/256)

durationColors <- c(rgb(127/256, 205/256, 187/256), rgb(44/256, 127/256, 184/256), rgb(237/256, 248/256, 177/256))
colors <- c(rgb(127/256, 205/256, 187/256), rgb(44/256, 127/256, 184/256), rgb(44/256, 127/256, 184/256), rgb(237/256, 248/256, 177/256))
options(scipen=999)

pdf(file = paste(path, "MethodLookup-Into.pdf", sep="/"))
methodIntoPlot <- boxplot(methodLookupRuleMerge, methodLookupRuleResult, methodLookupAtomEDB, methodLookupAtomPrimitive, methodLookupAtomInto, methodLookupAtomSkip, methodLookupQueryUnion, methodLookupQueryStable, methodLookupQueryIterate, methodLookupQueryResult, methodLookupAll,
        main = "MethodLookup",
        ylab = "Time per step in milliseconds",
        names = c("R-Merge", "R-Result", "A-EDB", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
        las = 2,
        log = "y",
        outline = FALSE,
        # ylim = c(0.001, 350),
        col = durationColors
)
# TODO rotate x axis 45 degrees
dev.off()

pdf(file = paste(path, "SubtypeOf-Into.pdf", sep="/"))
subtypeOfPlot <- boxplot(subtypeOfRuleMerge, subtypeOfRuleResult, subtypeOfAtomEDB, subtypeOfAtomEq, subtypeOfAtomInto, subtypeOfAtomSkip, subtypeOfQueryUnion, subtypeOfQueryStable, subtypeOfQueryIterate, subtypeOfQueryResult, subtypeOfAll,
                          main = "SubtypeOf",
                          ylab = "Time per step in milliseconds",
                          names = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
                          las = 2,
                          log = "y",
                          outline = FALSE,
                          col = durationColors
                          # ylim = c(0.001, 350),
)
# TODO rotate x axis 45 degrees
dev.off()

pdf(file = paste(path, "VarPointsTo-Into.pdf", sep="/"))
varPointsToPlot <- boxplot(varPointsToRuleMerge, varPointsToRuleResult, varPointsToAtomEDB, varPointsToAtomEq, varPointsToAtomNeq, varPointsToAtomPrim, varPointsToAtomInto, varPointsToAtomSkip, varPointsToQueryUnion, varPointsToQueryStable, varPointsToQueryIterate, varPointsToQueryResult, varPointsToAll,
                          main = "VarPointsTo",
                          ylab = "Time per step in milliseconds",
                          names = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Neq", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
                          las = 2,
                          log = "y",
                          outline = FALSE,
                          col = durationColors
                          # ylim = c(0.001, 350),
)
dev.off()


pdf(file = paste(path, "All-Into.pdf", sep="/"))
varPointsToPlot <- boxplot(combinedRuleMerge, combinedRuleResult, combinedAtomEDB, combinedAtomEq, combinedAtomNeq, combinedAtomPrim, combinedAtomInto, combinedAtomSkip, combinedQueryUnion, combinedQueryStable, combinedQueryIterate, combinedQueryResult, combinedAll,
                           main = "SubtypeOf + MethodLookup + VarPointsTo",
                           ylab = "Time per step in milliseconds",
                           names = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Neq", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
                           las = 2,
                           log = "y",
                           outline = FALSE,
                           col = durationColors
                           # ylim = c(0.001, 350),
)
dev.off()