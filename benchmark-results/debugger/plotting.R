
wd <- getwd()
datapath <- paste(wd, "benchmark-results/debugger/data", sep="/")
graphpath <- paste(wd, "benchmark-results/debugger/graphs", sep="/")

preprocessCSV <- function(fileName) {
  csv <- read.csv2(paste(datapath, fileName, sep="/"), sep = ",", dec = ".")
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

typeCol <- rgb(127/256, 205/256, 187/256)
constantCol <- rgb(44/256, 127/256, 184/256)
taintCol <- rgb(237/256, 248/256, 177/256)

durationColors <- c(rgb(127/256, 205/256, 187/256), rgb(44/256, 127/256, 184/256), rgb(237/256, 248/256, 177/256))
colors <- c(rgb(127/256, 205/256, 187/256), rgb(44/256, 127/256, 184/256), rgb(44/256, 127/256, 184/256), rgb(237/256, 248/256, 177/256))

options(scipen=999)
pdf(file = paste(graphpath, "MethodLookup-Into.pdf", sep="/"))
boxplot(methodLookupRuleMerge, methodLookupRuleResult, methodLookupAtomEDB, methodLookupAtomPrimitive, methodLookupAtomInto, methodLookupAtomSkip, methodLookupQueryUnion, methodLookupQueryStable, methodLookupQueryIterate, methodLookupQueryResult, methodLookupAll,
        main = "MethodLookup",
        ylab = "Time per step in milliseconds",
        names = c("R-Merge", "R-Result", "A-EDB", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
        las = 2,
        log = "y",
        outline = FALSE,
        # ylim = c(0.001, 350),
        col = durationColors
)
dev.off()

pdf(file = paste(graphpath, "SubtypeOf-Into.pdf", sep="/"))
boxplot(subtypeOfRuleMerge, subtypeOfRuleResult, subtypeOfAtomEDB, subtypeOfAtomEq, subtypeOfAtomInto, subtypeOfAtomSkip, subtypeOfQueryUnion, subtypeOfQueryStable, subtypeOfQueryIterate, subtypeOfQueryResult, subtypeOfAll,
        main = "SubtypeOf",
        ylab = "Time per step in milliseconds",
        names = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
        las = 2,
        log = "y",
        outline = FALSE,
        col = durationColors
        # ylim = c(0.001, 350),
)
dev.off()

pdf(file = paste(graphpath, "VarPointsTo-Into.pdf", sep="/"))
boxplot(varPointsToRuleMerge, varPointsToRuleResult, varPointsToAtomEDB, varPointsToAtomEq, varPointsToAtomNeq, varPointsToAtomPrim, varPointsToAtomInto, varPointsToAtomSkip, varPointsToQueryUnion, varPointsToQueryStable, varPointsToQueryIterate, varPointsToQueryResult, varPointsToAll,
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

pdf(file = paste(graphpath, "All-Into.pdf", sep="/"))
boxplot(combinedRuleMerge, combinedRuleResult, combinedAtomEDB, combinedAtomEq, combinedAtomNeq, combinedAtomPrim, combinedAtomInto, combinedAtomSkip, combinedQueryUnion, combinedQueryStable, combinedQueryIterate, combinedQueryResult, combinedAll,
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

pathInto10  <- preprocessCSV("Path-StepInto10.csv")
pathInto20  <- preprocessCSV("Path-StepInto20.csv")
pathInto30  <- preprocessCSV("Path-StepInto30.csv")
pathInto40  <- preprocessCSV("Path-StepInto40.csv")
pathInto50  <- preprocessCSV("Path-StepInto50.csv")
pathInto60  <- preprocessCSV("Path-StepInto60.csv")
pathInto70  <- preprocessCSV("Path-StepInto70.csv")
pathInto80  <- preprocessCSV("Path-StepInto80.csv")
pathInto90  <- preprocessCSV("Path-StepInto90.csv")
pathInto100 <- preprocessCSV("Path-StepInto100.csv")
pathIntoAll <- c(pathInto10, pathInto20, pathInto30, pathInto40, pathInto50, pathInto60, pathInto70, pathInto80, pathInto90, pathInto100)

pathOver10  <- preprocessCSV("Path-StepOver10.csv")
pathOver20  <- preprocessCSV("Path-StepOver20.csv")
pathOver30  <- preprocessCSV("Path-StepOver30.csv")
pathOver40  <- preprocessCSV("Path-StepOver40.csv")
pathOver50  <- preprocessCSV("Path-StepOver50.csv")
pathOver60  <- preprocessCSV("Path-StepOver60.csv")
pathOver70  <- preprocessCSV("Path-StepOver70.csv")
pathOver80  <- preprocessCSV("Path-StepOver80.csv")
pathOver90  <- preprocessCSV("Path-StepOver90.csv")
pathOver100 <- preprocessCSV("Path-StepOver100.csv")
pathOver110 <- preprocessCSV("Path-StepOver110.csv")
pathOver120 <- preprocessCSV("Path-StepOver120.csv")
pathOver130 <- preprocessCSV("Path-StepOver130.csv")
pathOver140 <- preprocessCSV("Path-StepOver140.csv")
pathOver150 <- preprocessCSV("Path-StepOver150.csv")
pathOver160 <- preprocessCSV("Path-StepOver160.csv")
pathOver170 <- preprocessCSV("Path-StepOver170.csv")
pathOver180 <- preprocessCSV("Path-StepOver180.csv")
pathOver190 <- preprocessCSV("Path-StepOver190.csv")
pathOver200 <- preprocessCSV("Path-StepOver200.csv")
pathOver210 <- preprocessCSV("Path-StepOver210.csv")
pathOver220 <- preprocessCSV("Path-StepOver220.csv")
pathOver230 <- preprocessCSV("Path-StepOver230.csv")
pathOver240 <- preprocessCSV("Path-StepOver240.csv")
pathOver250 <- preprocessCSV("Path-StepOver250.csv")
pathOver260 <- preprocessCSV("Path-StepOver260.csv")
pathOver270 <- preprocessCSV("Path-StepOver270.csv")
pathOver280 <- preprocessCSV("Path-StepOver280.csv")
pathOver290 <- preprocessCSV("Path-StepOver290.csv")
pathOver300 <- preprocessCSV("Path-StepOver300.csv")
pathOver310 <- preprocessCSV("Path-StepOver310.csv")
pathOver320 <- preprocessCSV("Path-StepOver320.csv")
pathOver330 <- preprocessCSV("Path-StepOver330.csv")
pathOver340 <- preprocessCSV("Path-StepOver340.csv")
pathOver350 <- preprocessCSV("Path-StepOver350.csv")
pathOver360 <- preprocessCSV("Path-StepOver360.csv")
pathOver370 <- preprocessCSV("Path-StepOver370.csv")
pathOver380 <- preprocessCSV("Path-StepOver380.csv")
pathOver390 <- preprocessCSV("Path-StepOver390.csv")
pathOver400 <- preprocessCSV("Path-StepOver400.csv")
pathOver410 <- preprocessCSV("Path-StepOver410.csv")
pathOver420 <- preprocessCSV("Path-StepOver420.csv")
pathOver430 <- preprocessCSV("Path-StepOver430.csv")
pathOver440 <- preprocessCSV("Path-StepOver440.csv")
pathOver450 <- preprocessCSV("Path-StepOver450.csv")
pathOver460 <- preprocessCSV("Path-StepOver460.csv")
pathOver470 <- preprocessCSV("Path-StepOver470.csv")
pathOver480 <- preprocessCSV("Path-StepOver480.csv")
pathOver490 <- preprocessCSV("Path-StepOver490.csv")
pathOver500 <- preprocessCSV("Path-StepOver500.csv")
pathOverAll <- c(
  pathOver10,
  pathOver20,
  pathOver30,
  pathOver40,
  pathOver50,
  pathOver60,
  pathOver70,
  pathOver80,
  pathOver90,
  pathOver100,
  pathOver110,
  pathOver120,
  pathOver130,
  pathOver140,
  pathOver150,
  pathOver160,
  pathOver170,
  pathOver180,
  pathOver190,
  pathOver200,
  pathOver210,
  pathOver220,
  pathOver230,
  pathOver240,
  pathOver250,
  pathOver260,
  pathOver270,
  pathOver280,
  pathOver290,
  pathOver300,
  pathOver310,
  pathOver320,
  pathOver330,
  pathOver340,
  pathOver350,
  pathOver360,
  pathOver370,
  pathOver380,
  pathOver390,
  pathOver400,
  pathOver410,
  pathOver420,
  pathOver430,
  pathOver440,
  pathOver450,
  pathOver460,
  pathOver470,
  pathOver480,
  pathOver490,
  pathOver500
  )
pdf(file = paste(graphpath, "Path.pdf", sep="/"))
plot(pathOverAll,
     type = "o",
     col = durationColors[1],
     ylab = "Total Time in milliseconds",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 80000),
)
lines(pathIntoAll, type = "o", col = durationColors[2])
axis(1,at=c(1:50),labels=seq(10, 500, by = 10))
legend("topright", legend=c("step-over", "step-into"),
       col=c(durationColors[1], durationColors[2]), lty=1:1)
dev.off()
