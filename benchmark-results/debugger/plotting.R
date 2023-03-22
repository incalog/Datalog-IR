
wd <- getwd()
datapath <- paste(wd, "benchmark-results/debugger/data", sep="/")
graphpath <- paste(wd, "benchmark-results/debugger/graphs", sep="/")

readMeasurement <- function(fileName) {
  csv <- read.csv2(paste(datapath, fileName, sep="/"), sep = ",", dec = ".")
  vals <- csv$measurement
  valsInMs <- nsToMs(vals)
  return(valsInMs)
}

readSteps <- function(fileName) {
  csv <- read.csv2(paste(datapath, fileName, sep="/"), sep = ",", dec = ".")
  vals <- csv$numSteps
  return(vals)
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

subtypeOfOptAll <-              readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_ALL.csv")
subtypeOfOptAtomEDB <-          readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_AtomEDB.csv")
subtypeOfOptAtomInto <-         readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_AtomInto.csv")
subtypeOfOptAtomSkip <-         readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_AtomSkip.csv")
subtypeOfOptAtomEq <-           readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_AtomEq.csv")
subtypeOfOptRuleMerge <-        readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_RuleMerge.csv")
subtypeOfOptRuleResult <-       readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_RuleResult.csv")
subtypeOfOptQueryUnion <-       readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_QueryUnion.csv")
subtypeOfOptQueryIterate <-     readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_QueryIterate.csv")
subtypeOfOptQueryStable <-      readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_QueryStable.csv")
subtypeOfOptQueryResult <-      readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_HybridSemanticsOpt_QueryResult.csv")

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

# color1 <- rgb(127/256, 205/256, 187/256)
# color2 <- rgb(44/256, 127/256, 184/256)
# color3 <- rgb(237/256, 248/256, 177/256)
color1 <- rgb(255/256, 255/256, 204/256)
color2 <- rgb(161/256, 218/256, 180/256)
color3 <- rgb(65/256, 182/256, 196/256)
color4 <- rgb(34/256, 94/256, 168/256)


options(scipen=999)
pdf(file = paste(graphpath, "MethodLookup-Into-UnOpt.pdf", sep="/"))
boxplot(methodLookupUnOptRuleMerge, methodLookupUnOptRuleResult, methodLookupUnOptAtomEDB, methodLookupUnOptAtomPrimitive, methodLookupUnOptAtomInto, methodLookupUnOptAtomSkip, methodLookupUnOptQueryUnion, methodLookupUnOptQueryStable, methodLookupUnOptQueryIterate, methodLookupUnOptQueryResult, methodLookupUnOptAll,
        main = "MethodLookup",
        ylab = "Time per step in milliseconds",
        names = c("R-Merge", "R-Result", "A-EDB", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
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

pdf(file = paste(graphpath, "SubtypeOf-Into-Opt.pdf", sep="/"))
boxplot(subtypeOfOptRuleMerge, subtypeOfOptRuleResult, subtypeOfOptAtomEDB, subtypeOfOptAtomEq, subtypeOfOptAtomInto, subtypeOfOptAtomSkip, subtypeOfOptQueryUnion, subtypeOfOptQueryStable, subtypeOfOptQueryIterate, subtypeOfOptQueryResult, subtypeOfOptAll,
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
        main = "SubtypeOf + MethodLookup + VarPointsTo",
        ylab = "Time per step in milliseconds",
        names = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Neq", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
        las = 2,
        # yaxt = "n",
        log = "y",
        outline = FALSE,
        col = c(rep(color1, times = 12), color4)
        # ylim = c(0.001, 350),
)
# TODO what are the values?
# axis(2, at = c(0.001, 0.01, 0.1, 1, 10, 100), labels = c(0, 100, 200, 300, 400, 500))
dev.off()


### VarPointsTo Step Over

scenario1UnOptOver <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_HybridSemantics_IntoVarPointsTo_Over.csv")
scenario2UnOptOver <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_HybridSemantics_IntoVarPointsToAndStaticFieldPointsTo_Over.csv")
scenario3UnOptOver <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_HybridSemantics_IntoVarPointsToAndInstanceFieldPointsTo_Over.csv")
scenario4UnOptOver <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_HybridSemantics_IntoVarPointsToAndReachable_Over.csv")
pdf(file = paste(graphpath, "VarPointsTo-Over-UnOpt.pdf", sep="/"))
boxplot(scenario1UnOptOver, scenario2UnOptOver, scenario3UnOptOver, scenario4UnOptOver,
        main = "VarPointsTo StepOver",
        ylab = "Time per step in milliseconds",
        names = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4"),
        # las = 2,
        # log = "y",
        outline = FALSE,
        col = c(color1, color2, color3, color4)
        # ylim = c(0.001, 350),
)
dev.off()

### VarPointsTo Memory?


### Path Step Into/Over/Bottom-Up

pathIntoUnOpt10  <- mean(readMeasurement("Path-StepInto10-UnOpt.csv"))
pathIntoUnOpt20  <- mean(readMeasurement("Path-StepInto20-UnOpt.csv"))
pathIntoUnOpt30  <- mean(readMeasurement("Path-StepInto30-UnOpt.csv"))
pathIntoUnOpt40  <- mean(readMeasurement("Path-StepInto40-UnOpt.csv"))
pathIntoUnOpt50  <- mean(readMeasurement("Path-StepInto50-UnOpt.csv"))
pathIntoUnOpt60  <- mean(readMeasurement("Path-StepInto60-UnOpt.csv"))
pathIntoUnOpt70  <- mean(readMeasurement("Path-StepInto70-UnOpt.csv"))
pathIntoUnOpt80  <- mean(readMeasurement("Path-StepInto80-UnOpt.csv"))
pathIntoUnOpt90  <- mean(readMeasurement("Path-StepInto90-UnOpt.csv"))
pathIntoUnOpt100 <- mean(readMeasurement("Path-StepInto100-UnOpt.csv"))
pathIntoUnOptAll <- c(pathIntoUnOpt10, pathIntoUnOpt20, pathIntoUnOpt30, pathIntoUnOpt40, pathIntoUnOpt50, pathIntoUnOpt60, pathIntoUnOpt70, pathIntoUnOpt80, pathIntoUnOpt90, pathIntoUnOpt100)

pathBU10  <- mean(readMeasurement("Path-BottomUp10.csv"))
pathBU20  <- mean(readMeasurement("Path-BottomUp20.csv"))
pathBU30  <- mean(readMeasurement("Path-BottomUp30.csv"))
pathBU40  <- mean(readMeasurement("Path-BottomUp40.csv"))
pathBU50  <- mean(readMeasurement("Path-BottomUp50.csv"))
pathBU60  <- mean(readMeasurement("Path-BottomUp60.csv"))
pathBU70  <- mean(readMeasurement("Path-BottomUp70.csv"))
pathBU80  <- mean(readMeasurement("Path-BottomUp80.csv"))
pathBU90  <- mean(readMeasurement("Path-BottomUp90.csv"))
pathBU100 <- mean(readMeasurement("Path-BottomUp100.csv"))
pathBU110 <- mean(readMeasurement("Path-BottomUp110.csv"))
pathBU120 <- mean(readMeasurement("Path-BottomUp120.csv"))
pathBU130 <- mean(readMeasurement("Path-BottomUp130.csv"))
pathBU140 <- mean(readMeasurement("Path-BottomUp140.csv"))
pathBU150 <- mean(readMeasurement("Path-BottomUp150.csv"))
pathBU160 <- mean(readMeasurement("Path-BottomUp160.csv"))
pathBU170 <- mean(readMeasurement("Path-BottomUp170.csv"))
pathBU180 <- mean(readMeasurement("Path-BottomUp180.csv"))
pathBU190 <- mean(readMeasurement("Path-BottomUp190.csv"))
pathBU200 <- mean(readMeasurement("Path-BottomUp200.csv"))
pathBU210 <- mean(readMeasurement("Path-BottomUp210.csv"))
pathBU220 <- mean(readMeasurement("Path-BottomUp220.csv"))
pathBU230 <- mean(readMeasurement("Path-BottomUp230.csv"))
pathBU240 <- mean(readMeasurement("Path-BottomUp240.csv"))
pathBU250 <- mean(readMeasurement("Path-BottomUp250.csv"))
pathBU260 <- mean(readMeasurement("Path-BottomUp260.csv"))
pathBU270 <- mean(readMeasurement("Path-BottomUp270.csv"))
pathBU280 <- mean(readMeasurement("Path-BottomUp280.csv"))
pathBU290 <- mean(readMeasurement("Path-BottomUp290.csv"))
pathBU300 <- mean(readMeasurement("Path-BottomUp300.csv"))
pathBU310 <- mean(readMeasurement("Path-BottomUp310.csv"))
pathBU320 <- mean(readMeasurement("Path-BottomUp320.csv"))
pathBU330 <- mean(readMeasurement("Path-BottomUp330.csv"))
pathBU340 <- mean(readMeasurement("Path-BottomUp340.csv"))
pathBU350 <- mean(readMeasurement("Path-BottomUp350.csv"))
pathBU360 <- mean(readMeasurement("Path-BottomUp360.csv"))
pathBU370 <- mean(readMeasurement("Path-BottomUp370.csv"))
pathBU380 <- mean(readMeasurement("Path-BottomUp380.csv"))
pathBU390 <- mean(readMeasurement("Path-BottomUp390.csv"))
pathBU400 <- mean(readMeasurement("Path-BottomUp400.csv"))
pathBU410 <- mean(readMeasurement("Path-BottomUp410.csv"))
pathBU420 <- mean(readMeasurement("Path-BottomUp420.csv"))
pathBU430 <- mean(readMeasurement("Path-BottomUp430.csv"))
pathBU440 <- mean(readMeasurement("Path-BottomUp440.csv"))
pathBU450 <- mean(readMeasurement("Path-BottomUp450.csv"))
pathBU460 <- mean(readMeasurement("Path-BottomUp460.csv"))
pathBU470 <- mean(readMeasurement("Path-BottomUp470.csv"))
pathBU480 <- mean(readMeasurement("Path-BottomUp480.csv"))
pathBU490 <- mean(readMeasurement("Path-BottomUp490.csv"))
pathBU500 <- mean(readMeasurement("Path-BottomUp500.csv"))
pathBU510 <- mean(readMeasurement("Path-BottomUp510.csv"))
# pathBU520 <- mean(readMeasurement("Path-BottomUp520.csv"))
# pathBU530 <- mean(readMeasurement("Path-BottomUp530.csv"))
# pathBU540 <- mean(readMeasurement("Path-BottomUp540.csv"))
# pathBU550 <- mean(readMeasurement("Path-BottomUp550.csv"))
# pathBU560 <- mean(readMeasurement("Path-BottomUp560.csv"))
# pathBU570 <- mean(readMeasurement("Path-BottomUp570.csv"))
# pathBU580 <- mean(readMeasurement("Path-BottomUp580.csv"))
# pathBU590 <- mean(readMeasurement("Path-BottomUp590.csv"))
# pathBU600 <- mean(readMeasurement("Path-BottomUp600.csv"))
# pathBU610 <- mean(readMeasurement("Path-BottomUp610.csv"))
# pathBU620 <- mean(readMeasurement("Path-BottomUp620.csv"))
# pathBU630 <- mean(readMeasurement("Path-BottomUp630.csv"))
# pathBU640 <- mean(readMeasurement("Path-BottomUp640.csv"))
# pathBU650 <- mean(readMeasurement("Path-BottomUp650.csv"))
# pathBU660 <- mean(readMeasurement("Path-BottomUp660.csv"))
# pathBU670 <- mean(readMeasurement("Path-BottomUp670.csv"))
# pathBU680 <- mean(readMeasurement("Path-BottomUp680.csv"))
# pathBU690 <- mean(readMeasurement("Path-BottomUp690.csv"))
# pathBU700 <- mean(readMeasurement("Path-BottomUp700.csv"))
pathBUAll <- c(
  pathBU10,
  pathBU20,
  pathBU30,
  pathBU40,
  pathBU50,
  pathBU60,
  pathBU70,
  pathBU80,
  pathBU90,
  pathBU100,
  pathBU110,
  pathBU120,
  pathBU130,
  pathBU140,
  pathBU150,
  pathBU160,
  pathBU170,
  pathBU180,
  pathBU190,
  pathBU200,
  pathBU210,
  pathBU220,
  pathBU230,
  pathBU240,
  pathBU250,
  pathBU260,
  pathBU270,
  pathBU280,
  pathBU290,
  pathBU300,
  pathBU310,
  pathBU320,
  pathBU330,
  pathBU340,
  pathBU350,
  pathBU360,
  pathBU370,
  pathBU380,
  pathBU390,
  pathBU400,
  pathBU410,
  pathBU420,
  pathBU430,
  pathBU440,
  pathBU450,
  pathBU460,
  pathBU470,
  pathBU480,
  pathBU490,
  pathBU500
  # pathBU510,
  # pathBU520,
  # pathBU530,
  # pathBU540,
  # pathBU550,
  # pathBU560,
  # pathBU570,
  # pathBU580,
  # pathBU590,
  # pathBU600,
  # pathBU610,
  # pathBU620,
  # pathBU630,
  # pathBU640,
  # pathBU650,
  # pathBU660,
  # pathBU670,
  # pathBU680,
  # pathBU690,
  # pathBU700
)

pathOverUnOpt10  <- mean(readMeasurement("Path-StepOver10-UnOpt.csv"))
pathOverUnOpt20  <- mean(readMeasurement("Path-StepOver20-UnOpt.csv"))
pathOverUnOpt30  <- mean(readMeasurement("Path-StepOver30-UnOpt.csv"))
pathOverUnOpt40  <- mean(readMeasurement("Path-StepOver40-UnOpt.csv"))
pathOverUnOpt50  <- mean(readMeasurement("Path-StepOver50-UnOpt.csv"))
pathOverUnOpt60  <- mean(readMeasurement("Path-StepOver60-UnOpt.csv"))
pathOverUnOpt70  <- mean(readMeasurement("Path-StepOver70-UnOpt.csv"))
pathOverUnOpt80  <- mean(readMeasurement("Path-StepOver80-UnOpt.csv"))
pathOverUnOpt90  <- mean(readMeasurement("Path-StepOver90-UnOpt.csv"))
pathOverUnOpt100 <- mean(readMeasurement("Path-StepOver100-UnOpt.csv"))
pathOverUnOpt110 <- mean(readMeasurement("Path-StepOver110-UnOpt.csv"))
pathOverUnOpt120 <- mean(readMeasurement("Path-StepOver120-UnOpt.csv"))
pathOverUnOpt130 <- mean(readMeasurement("Path-StepOver130-UnOpt.csv"))
pathOverUnOpt140 <- mean(readMeasurement("Path-StepOver140-UnOpt.csv"))
pathOverUnOpt150 <- mean(readMeasurement("Path-StepOver150-UnOpt.csv"))
pathOverUnOpt160 <- mean(readMeasurement("Path-StepOver160-UnOpt.csv"))
pathOverUnOpt170 <- mean(readMeasurement("Path-StepOver170-UnOpt.csv"))
pathOverUnOpt180 <- mean(readMeasurement("Path-StepOver180-UnOpt.csv"))
pathOverUnOpt190 <- mean(readMeasurement("Path-StepOver190-UnOpt.csv"))
pathOverUnOpt200 <- mean(readMeasurement("Path-StepOver200-UnOpt.csv"))
pathOverUnOpt210 <- mean(readMeasurement("Path-StepOver210-UnOpt.csv"))
pathOverUnOpt220 <- mean(readMeasurement("Path-StepOver220-UnOpt.csv"))
pathOverUnOpt230 <- mean(readMeasurement("Path-StepOver230-UnOpt.csv"))
pathOverUnOpt240 <- mean(readMeasurement("Path-StepOver240-UnOpt.csv"))
pathOverUnOpt250 <- mean(readMeasurement("Path-StepOver250-UnOpt.csv"))
pathOverUnOpt260 <- mean(readMeasurement("Path-StepOver260-UnOpt.csv"))
pathOverUnOpt270 <- mean(readMeasurement("Path-StepOver270-UnOpt.csv"))
pathOverUnOpt280 <- mean(readMeasurement("Path-StepOver280-UnOpt.csv"))
pathOverUnOpt290 <- mean(readMeasurement("Path-StepOver290-UnOpt.csv"))
pathOverUnOpt300 <- mean(readMeasurement("Path-StepOver300-UnOpt.csv"))
pathOverUnOpt310 <- mean(readMeasurement("Path-StepOver310-UnOpt.csv"))
pathOverUnOpt320 <- mean(readMeasurement("Path-StepOver320-UnOpt.csv"))
pathOverUnOpt330 <- mean(readMeasurement("Path-StepOver330-UnOpt.csv"))
pathOverUnOpt340 <- mean(readMeasurement("Path-StepOver340-UnOpt.csv"))
pathOverUnOpt350 <- mean(readMeasurement("Path-StepOver350-UnOpt.csv"))
pathOverUnOpt360 <- mean(readMeasurement("Path-StepOver360-UnOpt.csv"))
pathOverUnOpt370 <- mean(readMeasurement("Path-StepOver370-UnOpt.csv"))
pathOverUnOpt380 <- mean(readMeasurement("Path-StepOver380-UnOpt.csv"))
pathOverUnOpt390 <- mean(readMeasurement("Path-StepOver390-UnOpt.csv"))
pathOverUnOpt400 <- mean(readMeasurement("Path-StepOver400-UnOpt.csv"))
pathOverUnOpt410 <- mean(readMeasurement("Path-StepOver410-UnOpt.csv"))
pathOverUnOpt420 <- mean(readMeasurement("Path-StepOver420-UnOpt.csv"))
pathOverUnOpt430 <- mean(readMeasurement("Path-StepOver430-UnOpt.csv"))
pathOverUnOpt440 <- mean(readMeasurement("Path-StepOver440-UnOpt.csv"))
pathOverUnOpt450 <- mean(readMeasurement("Path-StepOver450-UnOpt.csv"))
pathOverUnOpt460 <- mean(readMeasurement("Path-StepOver460-UnOpt.csv"))
pathOverUnOpt470 <- mean(readMeasurement("Path-StepOver470-UnOpt.csv"))
pathOverUnOpt480 <- mean(readMeasurement("Path-StepOver480-UnOpt.csv"))
pathOverUnOpt490 <- mean(readMeasurement("Path-StepOver490-UnOpt.csv"))
pathOverUnOpt500 <- mean(readMeasurement("Path-StepOver500-UnOpt.csv"))
# pathOverUnOpt510 <- mean(readMeasurement("Path-StepOver510-UnOpt.csv"))
# pathOverUnOpt520 <- mean(readMeasurement("Path-StepOver520-UnOpt.csv"))
# pathOverUnOpt530 <- mean(readMeasurement("Path-StepOver530-UnOpt.csv"))
# pathOverUnOpt540 <- mean(readMeasurement("Path-StepOver540-UnOpt.csv"))
# pathOverUnOpt550 <- mean(readMeasurement("Path-StepOver550-UnOpt.csv"))
# pathOverUnOpt560 <- mean(readMeasurement("Path-StepOver560-UnOpt.csv"))
# pathOverUnOpt570 <- mean(readMeasurement("Path-StepOver570-UnOpt.csv"))
# pathOverUnOpt580 <- mean(readMeasurement("Path-StepOver580-UnOpt.csv"))
# pathOverUnOpt590 <- mean(readMeasurement("Path-StepOver590-UnOpt.csv"))
# pathOverUnOpt600 <- mean(readMeasurement("Path-StepOver600-UnOpt.csv"))
# pathOverUnOpt610 <- mean(readMeasurement("Path-StepOver610-UnOpt.csv"))
# pathOverUnOpt620 <- mean(readMeasurement("Path-StepOver620-UnOpt.csv"))
# pathOverUnOpt630 <- mean(readMeasurement("Path-StepOver630-UnOpt.csv"))
# pathOverUnOpt640 <- mean(readMeasurement("Path-StepOver640-UnOpt.csv"))
# pathOverUnOpt650 <- mean(readMeasurement("Path-StepOver650-UnOpt.csv"))
# pathOverUnOpt660 <- mean(readMeasurement("Path-StepOver660-UnOpt.csv"))
# pathOverUnOpt670 <- mean(readMeasurement("Path-StepOver670-UnOpt.csv"))
# pathOverUnOpt680 <- mean(readMeasurement("Path-StepOver680-UnOpt.csv"))
# pathOverUnOpt690 <- mean(readMeasurement("Path-StepOver690-UnOpt.csv"))
# pathOverUnOpt700 <- mean(readMeasurement("Path-StepOver700-UnOpt.csv"))
pathOverUnOptAll <- c(
  pathOverUnOpt10,
  pathOverUnOpt20,
  pathOverUnOpt30,
  pathOverUnOpt40,
  pathOverUnOpt50,
  pathOverUnOpt60,
  pathOverUnOpt70,
  pathOverUnOpt80,
  pathOverUnOpt90,
  pathOverUnOpt100,
  pathOverUnOpt110,
  pathOverUnOpt120,
  pathOverUnOpt130,
  pathOverUnOpt140,
  pathOverUnOpt150,
  pathOverUnOpt160,
  pathOverUnOpt170,
  pathOverUnOpt180,
  pathOverUnOpt190,
  pathOverUnOpt200,
  pathOverUnOpt210,
  pathOverUnOpt220,
  pathOverUnOpt230,
  pathOverUnOpt240,
  pathOverUnOpt250,
  pathOverUnOpt260,
  pathOverUnOpt270,
  pathOverUnOpt280,
  pathOverUnOpt290,
  pathOverUnOpt300,
  pathOverUnOpt310,
  pathOverUnOpt320,
  pathOverUnOpt330,
  pathOverUnOpt340,
  pathOverUnOpt350,
  pathOverUnOpt360,
  pathOverUnOpt370,
  pathOverUnOpt380,
  pathOverUnOpt390,
  pathOverUnOpt400,
  pathOverUnOpt410,
  pathOverUnOpt420,
  pathOverUnOpt430,
  pathOverUnOpt440,
  pathOverUnOpt450,
  pathOverUnOpt460,
  pathOverUnOpt470,
  pathOverUnOpt480,
  pathOverUnOpt490,
  pathOverUnOpt500
  # pathOverUnOpt510,
  # pathOverUnOpt520,
  # pathOverUnOpt530,
  # pathOverUnOpt540,
  # pathOverUnOpt550,
  # pathOverUnOpt560,
  # pathOverUnOpt570,
  # pathOverUnOpt580,
  # pathOverUnOpt590,
  # pathOverUnOpt600,
  # pathOverUnOpt610,
  # pathOverUnOpt620,
  # pathOverUnOpt630,
  # pathOverUnOpt640,
  # pathOverUnOpt650,
  # pathOverUnOpt660,
  # pathOverUnOpt670,
  # pathOverUnOpt680,
  # pathOverUnOpt690,
  # pathOverUnOpt700
  )

pathStepsIntoUnOpt10  <- mean(readSteps("Path-StepInto10-UnOpt.csv"))
pathStepsIntoUnOpt20  <- mean(readSteps("Path-StepInto20-UnOpt.csv"))
pathStepsIntoUnOpt30  <- mean(readSteps("Path-StepInto30-UnOpt.csv"))
pathStepsIntoUnOpt40  <- mean(readSteps("Path-StepInto40-UnOpt.csv"))
pathStepsIntoUnOpt50  <- mean(readSteps("Path-StepInto50-UnOpt.csv"))
pathStepsIntoUnOpt60  <- mean(readSteps("Path-StepInto60-UnOpt.csv"))
pathStepsIntoUnOpt70  <- mean(readSteps("Path-StepInto70-UnOpt.csv"))
pathStepsIntoUnOpt80  <- mean(readSteps("Path-StepInto80-UnOpt.csv"))
pathStepsIntoUnOpt90  <- mean(readSteps("Path-StepInto90-UnOpt.csv"))
pathStepsIntoUnOpt100 <- mean(readSteps("Path-StepInto100-UnOpt.csv"))
pathStepsIntoUnOptAll <- c(pathStepsIntoUnOpt10, pathStepsIntoUnOpt20, pathStepsIntoUnOpt30, pathStepsIntoUnOpt40, pathStepsIntoUnOpt50, pathStepsIntoUnOpt60, pathStepsIntoUnOpt70, pathStepsIntoUnOpt80, pathStepsIntoUnOpt90, pathStepsIntoUnOpt100)

pathStepsOverUnOpt10  <- mean(readMeasurement("Path-StepOver10-UnOpt.csv"))
pathStepsOverUnOpt20  <- mean(readMeasurement("Path-StepOver20-UnOpt.csv"))
pathStepsOverUnOpt30  <- mean(readMeasurement("Path-StepOver30-UnOpt.csv"))
pathStepsOverUnOpt40  <- mean(readMeasurement("Path-StepOver40-UnOpt.csv"))
pathStepsOverUnOpt50  <- mean(readMeasurement("Path-StepOver50-UnOpt.csv"))
pathStepsOverUnOpt60  <- mean(readMeasurement("Path-StepOver60-UnOpt.csv"))
pathStepsOverUnOpt70  <- mean(readMeasurement("Path-StepOver70-UnOpt.csv"))
pathStepsOverUnOpt80  <- mean(readMeasurement("Path-StepOver80-UnOpt.csv"))
pathStepsOverUnOpt90  <- mean(readMeasurement("Path-StepOver90-UnOpt.csv"))
pathStepsOverUnOpt100 <- mean(readMeasurement("Path-StepOver100-UnOpt.csv"))
pathStepsOverUnOpt110 <- mean(readMeasurement("Path-StepOver110-UnOpt.csv"))
pathStepsOverUnOpt120 <- mean(readMeasurement("Path-StepOver120-UnOpt.csv"))
pathStepsOverUnOpt130 <- mean(readMeasurement("Path-StepOver130-UnOpt.csv"))
pathStepsOverUnOpt140 <- mean(readMeasurement("Path-StepOver140-UnOpt.csv"))
pathStepsOverUnOpt150 <- mean(readMeasurement("Path-StepOver150-UnOpt.csv"))
pathStepsOverUnOpt160 <- mean(readMeasurement("Path-StepOver160-UnOpt.csv"))
pathStepsOverUnOpt170 <- mean(readMeasurement("Path-StepOver170-UnOpt.csv"))
pathStepsOverUnOpt180 <- mean(readMeasurement("Path-StepOver180-UnOpt.csv"))
pathStepsOverUnOpt190 <- mean(readMeasurement("Path-StepOver190-UnOpt.csv"))
pathStepsOverUnOpt200 <- mean(readMeasurement("Path-StepOver200-UnOpt.csv"))
pathStepsOverUnOpt210 <- mean(readMeasurement("Path-StepOver210-UnOpt.csv"))
pathStepsOverUnOpt220 <- mean(readMeasurement("Path-StepOver220-UnOpt.csv"))
pathStepsOverUnOpt230 <- mean(readMeasurement("Path-StepOver230-UnOpt.csv"))
pathStepsOverUnOpt240 <- mean(readMeasurement("Path-StepOver240-UnOpt.csv"))
pathStepsOverUnOpt250 <- mean(readMeasurement("Path-StepOver250-UnOpt.csv"))
pathStepsOverUnOpt260 <- mean(readMeasurement("Path-StepOver260-UnOpt.csv"))
pathStepsOverUnOpt270 <- mean(readMeasurement("Path-StepOver270-UnOpt.csv"))
pathStepsOverUnOpt280 <- mean(readMeasurement("Path-StepOver280-UnOpt.csv"))
pathStepsOverUnOpt290 <- mean(readMeasurement("Path-StepOver290-UnOpt.csv"))
pathStepsOverUnOpt300 <- mean(readMeasurement("Path-StepOver300-UnOpt.csv"))
pathStepsOverUnOpt310 <- mean(readMeasurement("Path-StepOver310-UnOpt.csv"))
pathStepsOverUnOpt320 <- mean(readMeasurement("Path-StepOver320-UnOpt.csv"))
pathStepsOverUnOpt330 <- mean(readMeasurement("Path-StepOver330-UnOpt.csv"))
pathStepsOverUnOpt340 <- mean(readMeasurement("Path-StepOver340-UnOpt.csv"))
pathStepsOverUnOpt350 <- mean(readMeasurement("Path-StepOver350-UnOpt.csv"))
pathStepsOverUnOpt360 <- mean(readMeasurement("Path-StepOver360-UnOpt.csv"))
pathStepsOverUnOpt370 <- mean(readMeasurement("Path-StepOver370-UnOpt.csv"))
pathStepsOverUnOpt380 <- mean(readMeasurement("Path-StepOver380-UnOpt.csv"))
pathStepsOverUnOpt390 <- mean(readMeasurement("Path-StepOver390-UnOpt.csv"))
pathStepsOverUnOpt400 <- mean(readMeasurement("Path-StepOver400-UnOpt.csv"))
pathStepsOverUnOpt410 <- mean(readMeasurement("Path-StepOver410-UnOpt.csv"))
pathStepsOverUnOpt420 <- mean(readMeasurement("Path-StepOver420-UnOpt.csv"))
pathStepsOverUnOpt430 <- mean(readMeasurement("Path-StepOver430-UnOpt.csv"))
pathStepsOverUnOpt440 <- mean(readMeasurement("Path-StepOver440-UnOpt.csv"))
pathStepsOverUnOpt450 <- mean(readMeasurement("Path-StepOver450-UnOpt.csv"))
pathStepsOverUnOpt460 <- mean(readMeasurement("Path-StepOver460-UnOpt.csv"))
pathStepsOverUnOpt470 <- mean(readMeasurement("Path-StepOver470-UnOpt.csv"))
pathStepsOverUnOpt480 <- mean(readMeasurement("Path-StepOver480-UnOpt.csv"))
pathStepsOverUnOpt490 <- mean(readMeasurement("Path-StepOver490-UnOpt.csv"))
pathStepsOverUnOpt500 <- mean(readMeasurement("Path-StepOver500-UnOpt.csv"))
# pathStepsOverUnOpt510 <- mean(readMeasurement("Path-StepOver510-UnOpt.csv"))
# pathStepsOverUnOpt520 <- mean(readMeasurement("Path-StepOver520-UnOpt.csv"))
# pathStepsOverUnOpt530 <- mean(readMeasurement("Path-StepOver530-UnOpt.csv"))
# pathStepsOverUnOpt540 <- mean(readMeasurement("Path-StepOver540-UnOpt.csv"))
# pathStepsOverUnOpt550 <- mean(readMeasurement("Path-StepOver550-UnOpt.csv"))
# pathStepsOverUnOpt560 <- mean(readMeasurement("Path-StepOver560-UnOpt.csv"))
# pathStepsOverUnOpt570 <- mean(readMeasurement("Path-StepOver570-UnOpt.csv"))
# pathStepsOverUnOpt580 <- mean(readMeasurement("Path-StepOver580-UnOpt.csv"))
# pathStepsOverUnOpt590 <- mean(readMeasurement("Path-StepOver590-UnOpt.csv"))
# pathStepsOverUnOpt600 <- mean(readMeasurement("Path-StepOver600-UnOpt.csv"))
# pathStepsOverUnOpt610 <- mean(readMeasurement("Path-StepOver610-UnOpt.csv"))
# pathStepsOverUnOpt620 <- mean(readMeasurement("Path-StepOver620-UnOpt.csv"))
# pathStepsOverUnOpt630 <- mean(readMeasurement("Path-StepOver630-UnOpt.csv"))
# pathStepsOverUnOpt640 <- mean(readMeasurement("Path-StepOver640-UnOpt.csv"))
# pathStepsOverUnOpt650 <- mean(readMeasurement("Path-StepOver650-UnOpt.csv"))
# pathStepsOverUnOpt660 <- mean(readMeasurement("Path-StepOver660-UnOpt.csv"))
# pathStepsOverUnOpt670 <- mean(readMeasurement("Path-StepOver670-UnOpt.csv"))
# pathStepsOverUnOpt680 <- mean(readMeasurement("Path-StepOver680-UnOpt.csv"))
# pathStepsOverUnOpt690 <- mean(readMeasurement("Path-StepOver690-UnOpt.csv"))
# pathStepsOverUnOpt700 <- mean(readMeasurement("Path-StepOver700-UnOpt.csv"))
pathStepsOverUnOptAll <- c(
  pathStepsOverUnOpt10,
  pathStepsOverUnOpt20,
  pathStepsOverUnOpt30,
  pathStepsOverUnOpt40,
  pathStepsOverUnOpt50,
  pathStepsOverUnOpt60,
  pathStepsOverUnOpt70,
  pathStepsOverUnOpt80,
  pathStepsOverUnOpt90,
  pathStepsOverUnOpt100,
  pathStepsOverUnOpt110,
  pathStepsOverUnOpt120,
  pathStepsOverUnOpt130,
  pathStepsOverUnOpt140,
  pathStepsOverUnOpt150,
  pathStepsOverUnOpt160,
  pathStepsOverUnOpt170,
  pathStepsOverUnOpt180,
  pathStepsOverUnOpt190,
  pathStepsOverUnOpt200,
  pathStepsOverUnOpt210,
  pathStepsOverUnOpt220,
  pathStepsOverUnOpt230,
  pathStepsOverUnOpt240,
  pathStepsOverUnOpt250,
  pathStepsOverUnOpt260,
  pathStepsOverUnOpt270,
  pathStepsOverUnOpt280,
  pathStepsOverUnOpt290,
  pathStepsOverUnOpt300,
  pathStepsOverUnOpt310,
  pathStepsOverUnOpt320,
  pathStepsOverUnOpt330,
  pathStepsOverUnOpt340,
  pathStepsOverUnOpt350,
  pathStepsOverUnOpt360,
  pathStepsOverUnOpt370,
  pathStepsOverUnOpt380,
  pathStepsOverUnOpt390,
  pathStepsOverUnOpt400,
  pathStepsOverUnOpt410,
  pathStepsOverUnOpt420,
  pathStepsOverUnOpt430,
  pathStepsOverUnOpt440,
  pathStepsOverUnOpt450,
  pathStepsOverUnOpt460,
  pathStepsOverUnOpt470,
  pathStepsOverUnOpt480,
  pathStepsOverUnOpt490,
  pathStepsOverUnOpt500
  # pathStepsOverUnOpt510,
  # pathStepsOverUnOpt520,
  # pathStepsOverUnOpt530,
  # pathStepsOverUnOpt540,
  # pathStepsOverUnOpt550,
  # pathStepsOverUnOpt560,
  # pathStepsOverUnOpt570,
  # pathStepsOverUnOpt580,
  # pathStepsOverUnOpt590,
  # pathStepsOverUnOpt600,
  # pathStepsOverUnOpt610,
  # pathStepsOverUnOpt620,
  # pathStepsOverUnOpt630,
  # pathStepsOverUnOpt640,
  # pathStepsOverUnOpt650,
  # pathStepsOverUnOpt660,
  # pathStepsOverUnOpt670,
  # pathStepsOverUnOpt680,
  # pathStepsOverUnOpt690,
  # pathStepsOverUnOpt700
)

pdf(file = paste(graphpath, "Path-Time-UnOpt.pdf", sep="/"))
plot(pathOverUnOptAll,
     type = "o",
     col = color2,
     ylab = "Total Time in milliseconds",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 70000),
)
lines(pathIntoUnOptAll, type = "o", col = color3)
lines(pathBUAll, type = "o", col = color4)
axis(1,at=c(1:50),labels=seq(10, 500, by = 10))
legend("topright", legend=c("step-over", "step-into", "bottom-up"),
       col=c(color2, color3, color4), lty=1:1)
dev.off()

pdf(file = paste(graphpath, "Path-1-50-UnOpt.pdf", sep="/"))
plot(c(pathOverUnOpt10, pathOverUnOpt20, pathOverUnOpt30, pathOverUnOpt40, pathOverUnOpt50),
     type = "o",
     col = color2,
     ylab = "Total Time in milliseconds",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 2000),
)
lines(c(pathIntoUnOpt10, pathIntoUnOpt20, pathIntoUnOpt30, pathIntoUnOpt40, pathIntoUnOpt50), type = "o", col = color3)
axis(1,at=c(1:5),labels=seq(10, 50, by = 10))
legend("topright", legend=c("step-over", "step-into"),
       col=c(color2, color3), lty=1:1)
dev.off()

pdf(file = paste(graphpath, "Path-Steps-UnOpt.pdf", sep="/"))
plot(pathStepsOverUnOptAll,
     type = "o",
     col = color2,
     ylab = "# Steps",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 120000),
)
lines(pathStepsIntoUnOptAll, type = "o", col = color3)
axis(1,at=c(1:50),labels=seq(10, 500, by = 10))
legend("topright", legend=c("step-over", "step-into"),
       col=c(color2, color3), lty=1:1)
dev.off()

# exponential regression for path-step-into
pathintotime.data <- data.frame(time=pathIntoUnOptAll, size=seq(10, 100, by = 10))
pathintotime.reg <- lm(log(time)~size, data= pathintotime.data)
summary(pathintotime.reg)

# linear regression for path-step-over
pathovertime.data <- data.frame(time=pathOverUnOptAll, size=seq(10, 500, by = 10))
pathovertime.reg <- lm(time~size, data= pathovertime.data)
summary(pathovertime.reg)


# Path Step Into/Over Optimized (Avoid NonProducing Iteration)


pathIntoOpt10  <- mean(readMeasurement("Path-StepInto10-Opt.csv"))
pathIntoOpt20  <- mean(readMeasurement("Path-StepInto20-Opt.csv"))
pathIntoOpt30  <- mean(readMeasurement("Path-StepInto30-Opt.csv"))
pathIntoOpt40  <- mean(readMeasurement("Path-StepInto40-Opt.csv"))
pathIntoOpt50  <- mean(readMeasurement("Path-StepInto50-Opt.csv"))
pathIntoOpt60  <- mean(readMeasurement("Path-StepInto60-Opt.csv"))
pathIntoOpt70  <- mean(readMeasurement("Path-StepInto70-Opt.csv"))
pathIntoOpt80  <- mean(readMeasurement("Path-StepInto80-Opt.csv"))
pathIntoOpt90  <- mean(readMeasurement("Path-StepInto90-Opt.csv"))
pathIntoOpt100 <- mean(readMeasurement("Path-StepInto100-Opt.csv"))
pathIntoOpt110 <- mean(readMeasurement("Path-StepInto110-Opt.csv"))
pathIntoOpt120 <- mean(readMeasurement("Path-StepInto120-Opt.csv"))
pathIntoOpt130 <- mean(readMeasurement("Path-StepInto130-Opt.csv"))
pathIntoOpt140 <- mean(readMeasurement("Path-StepInto140-Opt.csv"))
pathIntoOpt150 <- mean(readMeasurement("Path-StepInto150-Opt.csv"))
pathIntoOpt160 <- mean(readMeasurement("Path-StepInto160-Opt.csv"))
pathIntoOpt170 <- mean(readMeasurement("Path-StepInto170-Opt.csv"))
pathIntoOpt180 <- mean(readMeasurement("Path-StepInto180-Opt.csv"))
pathIntoOpt190 <- mean(readMeasurement("Path-StepInto190-Opt.csv"))
pathIntoOpt200 <- mean(readMeasurement("Path-StepInto200-Opt.csv"))
pathIntoOpt210 <- mean(readMeasurement("Path-StepInto210-Opt.csv"))
pathIntoOpt220 <- mean(readMeasurement("Path-StepInto220-Opt.csv"))
pathIntoOpt230 <- mean(readMeasurement("Path-StepInto230-Opt.csv"))
pathIntoOpt240 <- mean(readMeasurement("Path-StepInto240-Opt.csv"))
pathIntoOpt250 <- mean(readMeasurement("Path-StepInto250-Opt.csv"))
pathIntoOpt260 <- mean(readMeasurement("Path-StepInto260-Opt.csv"))
pathIntoOpt270 <- mean(readMeasurement("Path-StepInto270-Opt.csv"))
pathIntoOpt280 <- mean(readMeasurement("Path-StepInto280-Opt.csv"))
pathIntoOpt290 <- mean(readMeasurement("Path-StepInto290-Opt.csv"))
pathIntoOpt300 <- mean(readMeasurement("Path-StepInto300-Opt.csv"))
pathIntoOptAll <- c(
  pathIntoOpt10,
  pathIntoOpt20,
  pathIntoOpt30,
  pathIntoOpt40,
  pathIntoOpt50,
  pathIntoOpt60,
  pathIntoOpt70,
  pathIntoOpt80,
  pathIntoOpt90,
  pathIntoOpt100,
  pathIntoOpt110,
  pathIntoOpt120,
  pathIntoOpt130,
  pathIntoOpt140,
  pathIntoOpt150,
  pathIntoOpt160,
  pathIntoOpt170,
  pathIntoOpt180,
  pathIntoOpt190,
  pathIntoOpt200,
  pathIntoOpt210,
  pathIntoOpt220,
  pathIntoOpt230,
  pathIntoOpt240,
  pathIntoOpt250,
  pathIntoOpt260,
  pathIntoOpt270,
  pathIntoOpt280,
  pathIntoOpt290,
  pathIntoOpt300
)


pathOverOpt10  <- mean(readMeasurement("Path-StepOver10-Opt.csv"))
pathOverOpt20  <- mean(readMeasurement("Path-StepOver20-Opt.csv"))
pathOverOpt30  <- mean(readMeasurement("Path-StepOver30-Opt.csv"))
pathOverOpt40  <- mean(readMeasurement("Path-StepOver40-Opt.csv"))
pathOverOpt50  <- mean(readMeasurement("Path-StepOver50-Opt.csv"))
pathOverOpt60  <- mean(readMeasurement("Path-StepOver60-Opt.csv"))
pathOverOpt70  <- mean(readMeasurement("Path-StepOver70-Opt.csv"))
pathOverOpt80  <- mean(readMeasurement("Path-StepOver80-Opt.csv"))
pathOverOpt90  <- mean(readMeasurement("Path-StepOver90-Opt.csv"))
pathOverOpt100 <- mean(readMeasurement("Path-StepOver100-Opt.csv"))
pathOverOpt110 <- mean(readMeasurement("Path-StepOver110-Opt.csv"))
pathOverOpt120 <- mean(readMeasurement("Path-StepOver120-Opt.csv"))
pathOverOpt130 <- mean(readMeasurement("Path-StepOver130-Opt.csv"))
pathOverOpt140 <- mean(readMeasurement("Path-StepOver140-Opt.csv"))
pathOverOpt150 <- mean(readMeasurement("Path-StepOver150-Opt.csv"))
pathOverOpt160 <- mean(readMeasurement("Path-StepOver160-Opt.csv"))
pathOverOpt170 <- mean(readMeasurement("Path-StepOver170-Opt.csv"))
pathOverOpt180 <- mean(readMeasurement("Path-StepOver180-Opt.csv"))
pathOverOpt190 <- mean(readMeasurement("Path-StepOver190-Opt.csv"))
pathOverOpt200 <- mean(readMeasurement("Path-StepOver200-Opt.csv"))
pathOverOpt210 <- mean(readMeasurement("Path-StepOver210-Opt.csv"))
pathOverOpt220 <- mean(readMeasurement("Path-StepOver220-Opt.csv"))
pathOverOpt230 <- mean(readMeasurement("Path-StepOver230-Opt.csv"))
pathOverOpt240 <- mean(readMeasurement("Path-StepOver240-Opt.csv"))
pathOverOpt250 <- mean(readMeasurement("Path-StepOver250-Opt.csv"))
pathOverOpt260 <- mean(readMeasurement("Path-StepOver260-Opt.csv"))
pathOverOpt270 <- mean(readMeasurement("Path-StepOver270-Opt.csv"))
pathOverOpt280 <- mean(readMeasurement("Path-StepOver280-Opt.csv"))
pathOverOpt290 <- mean(readMeasurement("Path-StepOver290-Opt.csv"))
pathOverOpt300 <- mean(readMeasurement("Path-StepOver300-Opt.csv"))
pathOverOpt310 <- mean(readMeasurement("Path-StepOver310-Opt.csv"))
pathOverOpt320 <- mean(readMeasurement("Path-StepOver320-Opt.csv"))
pathOverOpt330 <- mean(readMeasurement("Path-StepOver330-Opt.csv"))
pathOverOpt340 <- mean(readMeasurement("Path-StepOver340-Opt.csv"))
pathOverOpt350 <- mean(readMeasurement("Path-StepOver350-Opt.csv"))
pathOverOpt360 <- mean(readMeasurement("Path-StepOver360-Opt.csv"))
pathOverOpt370 <- mean(readMeasurement("Path-StepOver370-Opt.csv"))
pathOverOpt380 <- mean(readMeasurement("Path-StepOver380-Opt.csv"))
pathOverOpt390 <- mean(readMeasurement("Path-StepOver390-Opt.csv"))
pathOverOpt400 <- mean(readMeasurement("Path-StepOver400-Opt.csv"))
pathOverOpt410 <- mean(readMeasurement("Path-StepOver410-Opt.csv"))
pathOverOpt420 <- mean(readMeasurement("Path-StepOver420-Opt.csv"))
pathOverOpt430 <- mean(readMeasurement("Path-StepOver430-Opt.csv"))
pathOverOpt440 <- mean(readMeasurement("Path-StepOver440-Opt.csv"))
pathOverOpt450 <- mean(readMeasurement("Path-StepOver450-Opt.csv"))
pathOverOpt460 <- mean(readMeasurement("Path-StepOver460-Opt.csv"))
pathOverOpt470 <- mean(readMeasurement("Path-StepOver470-Opt.csv"))
pathOverOpt480 <- mean(readMeasurement("Path-StepOver480-Opt.csv"))
pathOverOpt490 <- mean(readMeasurement("Path-StepOver490-Opt.csv"))
pathOverOpt500 <- mean(readMeasurement("Path-StepOver500-Opt.csv"))
# pathOverUnOpt510 <- mean(readMeasurement("Path-StepOver510-UnOpt.csv"))
# pathOverUnOpt520 <- mean(readMeasurement("Path-StepOver520-UnOpt.csv"))
# pathOverUnOpt530 <- mean(readMeasurement("Path-StepOver530-UnOpt.csv"))
# pathOverUnOpt540 <- mean(readMeasurement("Path-StepOver540-UnOpt.csv"))
# pathOverUnOpt550 <- mean(readMeasurement("Path-StepOver550-UnOpt.csv"))
# pathOverUnOpt560 <- mean(readMeasurement("Path-StepOver560-UnOpt.csv"))
# pathOverUnOpt570 <- mean(readMeasurement("Path-StepOver570-UnOpt.csv"))
# pathOverUnOpt580 <- mean(readMeasurement("Path-StepOver580-UnOpt.csv"))
# pathOverUnOpt590 <- mean(readMeasurement("Path-StepOver590-UnOpt.csv"))
# pathOverUnOpt600 <- mean(readMeasurement("Path-StepOver600-UnOpt.csv"))
# pathOverUnOpt610 <- mean(readMeasurement("Path-StepOver610-UnOpt.csv"))
# pathOverUnOpt620 <- mean(readMeasurement("Path-StepOver620-UnOpt.csv"))
# pathOverUnOpt630 <- mean(readMeasurement("Path-StepOver630-UnOpt.csv"))
# pathOverUnOpt640 <- mean(readMeasurement("Path-StepOver640-UnOpt.csv"))
# pathOverUnOpt650 <- mean(readMeasurement("Path-StepOver650-UnOpt.csv"))
# pathOverUnOpt660 <- mean(readMeasurement("Path-StepOver660-UnOpt.csv"))
# pathOverUnOpt670 <- mean(readMeasurement("Path-StepOver670-UnOpt.csv"))
# pathOverUnOpt680 <- mean(readMeasurement("Path-StepOver680-UnOpt.csv"))
# pathOverUnOpt690 <- mean(readMeasurement("Path-StepOver690-UnOpt.csv"))
# pathOverUnOpt700 <- mean(readMeasurement("Path-StepOver700-UnOpt.csv"))
pathOverOptAll <- c(
  pathOverOpt10,
  pathOverOpt20,
  pathOverOpt30,
  pathOverOpt40,
  pathOverOpt50,
  pathOverOpt60,
  pathOverOpt70,
  pathOverOpt80,
  pathOverOpt90,
  pathOverOpt100,
  pathOverOpt110,
  pathOverOpt120,
  pathOverOpt130,
  pathOverOpt140,
  pathOverOpt150,
  pathOverOpt160,
  pathOverOpt170,
  pathOverOpt180,
  pathOverOpt190,
  pathOverOpt200,
  pathOverOpt210,
  pathOverOpt220,
  pathOverOpt230,
  pathOverOpt240,
  pathOverOpt250,
  pathOverOpt260,
  pathOverOpt270,
  pathOverOpt280,
  pathOverOpt290,
  pathOverOpt300,
  pathOverOpt310,
  pathOverOpt320,
  pathOverOpt330,
  pathOverOpt340,
  pathOverOpt350,
  pathOverOpt360,
  pathOverOpt370,
  pathOverOpt380,
  pathOverOpt390,
  pathOverOpt400,
  pathOverOpt410,
  pathOverOpt420,
  pathOverOpt430,
  pathOverOpt440,
  pathOverOpt450,
  pathOverOpt460,
  pathOverOpt470,
  pathOverOpt480,
  pathOverOpt490,
  pathOverOpt500
  # pathOverUnOpt510,
  # pathOverUnOpt520,
  # pathOverUnOpt530,
  # pathOverUnOpt540,
  # pathOverUnOpt550,
  # pathOverUnOpt560,
  # pathOverUnOpt570,
  # pathOverUnOpt580,
  # pathOverUnOpt590,
  # pathOverUnOpt600,
  # pathOverUnOpt610,
  # pathOverUnOpt620,
  # pathOverUnOpt630,
  # pathOverUnOpt640,
  # pathOverUnOpt650,
  # pathOverUnOpt660,
  # pathOverUnOpt670,
  # pathOverUnOpt680,
  # pathOverUnOpt690,
  # pathOverUnOpt700
)

pathStepsIntoOpt10  <- mean(readSteps("Path-StepInto10-Opt.csv"))
pathStepsIntoOpt20  <- mean(readSteps("Path-StepInto20-Opt.csv"))
pathStepsIntoOpt30  <- mean(readSteps("Path-StepInto30-Opt.csv"))
pathStepsIntoOpt40  <- mean(readSteps("Path-StepInto40-Opt.csv"))
pathStepsIntoOpt50  <- mean(readSteps("Path-StepInto50-Opt.csv"))
pathStepsIntoOpt60  <- mean(readSteps("Path-StepInto60-Opt.csv"))
pathStepsIntoOpt70  <- mean(readSteps("Path-StepInto70-Opt.csv"))
pathStepsIntoOpt80  <- mean(readSteps("Path-StepInto80-Opt.csv"))
pathStepsIntoOpt90  <- mean(readSteps("Path-StepInto90-Opt.csv"))
pathStepsIntoOpt100 <- mean(readSteps("Path-StepInto100-Opt.csv"))
pathStepsIntoOptAll <- c(pathStepsIntoOpt10, pathStepsIntoOpt20, pathStepsIntoOpt30, pathStepsIntoOpt40, pathStepsIntoOpt50, pathStepsIntoOpt60, pathStepsIntoOpt70, pathStepsIntoOpt80, pathStepsIntoOpt90, pathStepsIntoOpt100)

pathStepsOverOpt10  <- mean(readMeasurement("Path-StepOver10-Opt.csv"))
pathStepsOverOpt20  <- mean(readMeasurement("Path-StepOver20-Opt.csv"))
pathStepsOverOpt30  <- mean(readMeasurement("Path-StepOver30-Opt.csv"))
pathStepsOverOpt40  <- mean(readMeasurement("Path-StepOver40-Opt.csv"))
pathStepsOverOpt50  <- mean(readMeasurement("Path-StepOver50-Opt.csv"))
pathStepsOverOpt60  <- mean(readMeasurement("Path-StepOver60-Opt.csv"))
pathStepsOverOpt70  <- mean(readMeasurement("Path-StepOver70-Opt.csv"))
pathStepsOverOpt80  <- mean(readMeasurement("Path-StepOver80-Opt.csv"))
pathStepsOverOpt90  <- mean(readMeasurement("Path-StepOver90-Opt.csv"))
pathStepsOverOpt100 <- mean(readMeasurement("Path-StepOver100-Opt.csv"))
pathStepsOverOpt110 <- mean(readMeasurement("Path-StepOver110-Opt.csv"))
pathStepsOverOpt120 <- mean(readMeasurement("Path-StepOver120-Opt.csv"))
pathStepsOverOpt130 <- mean(readMeasurement("Path-StepOver130-Opt.csv"))
pathStepsOverOpt140 <- mean(readMeasurement("Path-StepOver140-Opt.csv"))
pathStepsOverOpt150 <- mean(readMeasurement("Path-StepOver150-Opt.csv"))
pathStepsOverOpt160 <- mean(readMeasurement("Path-StepOver160-Opt.csv"))
pathStepsOverOpt170 <- mean(readMeasurement("Path-StepOver170-Opt.csv"))
pathStepsOverOpt180 <- mean(readMeasurement("Path-StepOver180-Opt.csv"))
pathStepsOverOpt190 <- mean(readMeasurement("Path-StepOver190-Opt.csv"))
pathStepsOverOpt200 <- mean(readMeasurement("Path-StepOver200-Opt.csv"))
pathStepsOverOpt210 <- mean(readMeasurement("Path-StepOver210-Opt.csv"))
pathStepsOverOpt220 <- mean(readMeasurement("Path-StepOver220-Opt.csv"))
pathStepsOverOpt230 <- mean(readMeasurement("Path-StepOver230-Opt.csv"))
pathStepsOverOpt240 <- mean(readMeasurement("Path-StepOver240-Opt.csv"))
pathStepsOverOpt250 <- mean(readMeasurement("Path-StepOver250-Opt.csv"))
pathStepsOverOpt260 <- mean(readMeasurement("Path-StepOver260-Opt.csv"))
pathStepsOverOpt270 <- mean(readMeasurement("Path-StepOver270-Opt.csv"))
pathStepsOverOpt280 <- mean(readMeasurement("Path-StepOver280-Opt.csv"))
pathStepsOverOpt290 <- mean(readMeasurement("Path-StepOver290-Opt.csv"))
pathStepsOverOpt300 <- mean(readMeasurement("Path-StepOver300-Opt.csv"))
pathStepsOverOpt310 <- mean(readMeasurement("Path-StepOver310-Opt.csv"))
pathStepsOverOpt320 <- mean(readMeasurement("Path-StepOver320-Opt.csv"))
pathStepsOverOpt330 <- mean(readMeasurement("Path-StepOver330-Opt.csv"))
pathStepsOverOpt340 <- mean(readMeasurement("Path-StepOver340-Opt.csv"))
pathStepsOverOpt350 <- mean(readMeasurement("Path-StepOver350-Opt.csv"))
pathStepsOverOpt360 <- mean(readMeasurement("Path-StepOver360-Opt.csv"))
pathStepsOverOpt370 <- mean(readMeasurement("Path-StepOver370-Opt.csv"))
pathStepsOverOpt380 <- mean(readMeasurement("Path-StepOver380-Opt.csv"))
pathStepsOverOpt390 <- mean(readMeasurement("Path-StepOver390-Opt.csv"))
pathStepsOverOpt400 <- mean(readMeasurement("Path-StepOver400-Opt.csv"))
pathStepsOverOpt410 <- mean(readMeasurement("Path-StepOver410-Opt.csv"))
pathStepsOverOpt420 <- mean(readMeasurement("Path-StepOver420-Opt.csv"))
pathStepsOverOpt430 <- mean(readMeasurement("Path-StepOver430-Opt.csv"))
pathStepsOverOpt440 <- mean(readMeasurement("Path-StepOver440-Opt.csv"))
pathStepsOverOpt450 <- mean(readMeasurement("Path-StepOver450-Opt.csv"))
pathStepsOverOpt460 <- mean(readMeasurement("Path-StepOver460-Opt.csv"))
pathStepsOverOpt470 <- mean(readMeasurement("Path-StepOver470-Opt.csv"))
pathStepsOverOpt480 <- mean(readMeasurement("Path-StepOver480-Opt.csv"))
pathStepsOverOpt490 <- mean(readMeasurement("Path-StepOver490-Opt.csv"))
pathStepsOverOpt500 <- mean(readMeasurement("Path-StepOver500-Opt.csv"))
# pathStepsOverUnOpt510 <- mean(readMeasurement("Path-StepOver510-UnOpt.csv"))
# pathStepsOverUnOpt520 <- mean(readMeasurement("Path-StepOver520-UnOpt.csv"))
# pathStepsOverUnOpt530 <- mean(readMeasurement("Path-StepOver530-UnOpt.csv"))
# pathStepsOverUnOpt540 <- mean(readMeasurement("Path-StepOver540-UnOpt.csv"))
# pathStepsOverUnOpt550 <- mean(readMeasurement("Path-StepOver550-UnOpt.csv"))
# pathStepsOverUnOpt560 <- mean(readMeasurement("Path-StepOver560-UnOpt.csv"))
# pathStepsOverUnOpt570 <- mean(readMeasurement("Path-StepOver570-UnOpt.csv"))
# pathStepsOverUnOpt580 <- mean(readMeasurement("Path-StepOver580-UnOpt.csv"))
# pathStepsOverUnOpt590 <- mean(readMeasurement("Path-StepOver590-UnOpt.csv"))
# pathStepsOverUnOpt600 <- mean(readMeasurement("Path-StepOver600-UnOpt.csv"))
# pathStepsOverUnOpt610 <- mean(readMeasurement("Path-StepOver610-UnOpt.csv"))
# pathStepsOverUnOpt620 <- mean(readMeasurement("Path-StepOver620-UnOpt.csv"))
# pathStepsOverUnOpt630 <- mean(readMeasurement("Path-StepOver630-UnOpt.csv"))
# pathStepsOverUnOpt640 <- mean(readMeasurement("Path-StepOver640-UnOpt.csv"))
# pathStepsOverUnOpt650 <- mean(readMeasurement("Path-StepOver650-UnOpt.csv"))
# pathStepsOverUnOpt660 <- mean(readMeasurement("Path-StepOver660-UnOpt.csv"))
# pathStepsOverUnOpt670 <- mean(readMeasurement("Path-StepOver670-UnOpt.csv"))
# pathStepsOverUnOpt680 <- mean(readMeasurement("Path-StepOver680-UnOpt.csv"))
# pathStepsOverUnOpt690 <- mean(readMeasurement("Path-StepOver690-UnOpt.csv"))
# pathStepsOverUnOpt700 <- mean(readMeasurement("Path-StepOver700-UnOpt.csv"))
pathStepsOverOptAll <- c(
  pathStepsOverOpt10,
  pathStepsOverOpt20,
  pathStepsOverOpt30,
  pathStepsOverOpt40,
  pathStepsOverOpt50,
  pathStepsOverOpt60,
  pathStepsOverOpt70,
  pathStepsOverOpt80,
  pathStepsOverOpt90,
  pathStepsOverOpt100,
  pathStepsOverOpt110,
  pathStepsOverOpt120,
  pathStepsOverOpt130,
  pathStepsOverOpt140,
  pathStepsOverOpt150,
  pathStepsOverOpt160,
  pathStepsOverOpt170,
  pathStepsOverOpt180,
  pathStepsOverOpt190,
  pathStepsOverOpt200,
  pathStepsOverOpt210,
  pathStepsOverOpt220,
  pathStepsOverOpt230,
  pathStepsOverOpt240,
  pathStepsOverOpt250,
  pathStepsOverOpt260,
  pathStepsOverOpt270,
  pathStepsOverOpt280,
  pathStepsOverOpt290,
  pathStepsOverOpt300,
  pathStepsOverOpt310,
  pathStepsOverOpt320,
  pathStepsOverOpt330,
  pathStepsOverOpt340,
  pathStepsOverOpt350,
  pathStepsOverOpt360,
  pathStepsOverOpt370,
  pathStepsOverOpt380,
  pathStepsOverOpt390,
  pathStepsOverOpt400,
  pathStepsOverOpt410,
  pathStepsOverOpt420,
  pathStepsOverOpt430,
  pathStepsOverOpt440,
  pathStepsOverOpt450,
  pathStepsOverOpt460,
  pathStepsOverOpt470,
  pathStepsOverOpt480,
  pathStepsOverOpt490,
  pathStepsOverOpt500
  # pathStepsOverUnOpt510,
  # pathStepsOverUnOpt520,
  # pathStepsOverUnOpt530,
  # pathStepsOverUnOpt540,
  # pathStepsOverUnOpt550,
  # pathStepsOverUnOpt560,
  # pathStepsOverUnOpt570,
  # pathStepsOverUnOpt580,
  # pathStepsOverUnOpt590,
  # pathStepsOverUnOpt600,
  # pathStepsOverUnOpt610,
  # pathStepsOverUnOpt620,
  # pathStepsOverUnOpt630,
  # pathStepsOverUnOpt640,
  # pathStepsOverUnOpt650,
  # pathStepsOverUnOpt660,
  # pathStepsOverUnOpt670,
  # pathStepsOverUnOpt680,
  # pathStepsOverUnOpt690,
  # pathStepsOverUnOpt700
)

pdf(file = paste(graphpath, "Path-Time-Opt.pdf", sep="/"))
plot(pathOverOptAll,
     type = "o",
     col = color2,
     ylab = "Total Time in milliseconds",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 10000),
)
lines(pathIntoOptAll, type = "o", col = color3)
lines(pathBUAll, type = "o", col = color4)
axis(1,at=c(1:50),labels=seq(10, 500, by = 10))
legend("topright", legend=c("step-over", "step-into", "bottom-up"),
       col=c(color2, color3, color4), lty=1:1)
dev.off()

# pdf(file = paste(graphpath, "Path-1-50-UnOpt.pdf", sep="/"))
# plot(c(pathOverUnOpt10, pathOverUnOpt20, pathOverUnOpt30, pathOverUnOpt40, pathOverUnOpt50),
#      type = "o",
#      col = color2,
#      ylab = "Total Time in milliseconds",
#      # names = seq(10, 500, by = 10),
#      # col = durationColors
#      xaxt='n',
#      ylim = c(0, 2000),
# )
# lines(c(pathIntoUnOpt10, pathIntoUnOpt20, pathIntoUnOpt30, pathIntoUnOpt40, pathIntoUnOpt50), type = "o", col = color3)
# axis(1,at=c(1:5),labels=seq(10, 50, by = 10))
# legend("topright", legend=c("step-over", "step-into"),
#        col=c(color2, color3), lty=1:1)
# dev.off()

# exponential regression for path-step-into
# pathintotime.data <- data.frame(time=pathIntoUnOptAll, size=seq(10, 100, by = 10))
# pathintotime.reg <- lm(log(time)~size, data= pathintotime.data)
# summary(pathintotime.reg)

# # linear regression for path-step-over
# pathovertime.data <- data.frame(time=pathOverUnOptAll, size=seq(10, 500, by = 10))
# pathovertime.reg <- lm(time~size, data= pathovertime.data)
# summary(pathovertime.reg)


pdf(file = paste(graphpath, "Path-Steps-Opt.pdf", sep="/"))
plot(pathStepsOverOptAll,
     type = "o",
     col = color2,
     ylab = "# Steps",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 120000),
)
lines(pathStepsIntoOptAll, type = "o", col = color3)
axis(1,at=c(1:50),labels=seq(10, 500, by = 10))
legend("topright", legend=c("step-over", "step-into"),
       col=c(color2, color3), lty=1:1)
dev.off()

