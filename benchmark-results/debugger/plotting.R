
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

methodLookupAll <-           readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_ALL.csv")
methodLookupAtomEDB <-       readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomEDB.csv")
methodLookupAtomInto <-      readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomInto.csv")
methodLookupAtomSkip <-      readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomSkip.csv")
methodLookupAtomPrimitive <- readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomPrimitive.csv")
methodLookupRuleMerge <-     readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_RuleMerge.csv")
methodLookupRuleResult <-    readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_RuleResult.csv")
methodLookupQueryUnion <-    readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryUnion.csv")
methodLookupQueryIterate <-  readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryIterate.csv")
methodLookupQueryStable <-   readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryStable.csv")
methodLookupQueryResult <-   readMeasurement("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryResult.csv")

subtypeOfAll <-              readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_ALL.csv")
subtypeOfAtomEDB <-          readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomEDB.csv")
subtypeOfAtomInto <-         readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomInto.csv")
subtypeOfAtomSkip <-         readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomSkip.csv")
subtypeOfAtomEq <-           readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomEq.csv")
subtypeOfRuleMerge <-        readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_RuleMerge.csv")
subtypeOfRuleResult <-       readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_RuleResult.csv")
subtypeOfQueryUnion <-       readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryUnion.csv")
subtypeOfQueryIterate <-     readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryIterate.csv")
subtypeOfQueryStable <-      readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryStable.csv")
subtypeOfQueryResult <-      readMeasurement("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryResult.csv")

varPointsToAll <-            readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_ALL.csv")
varPointsToAtomEDB <-        readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomEDB.csv")
varPointsToAtomInto <-       readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomInto.csv")
varPointsToAtomSkip <-       readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomSkip.csv")
varPointsToAtomEq <-         readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomEq.csv")
varPointsToAtomNeq <-        readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomNeq.csv")
varPointsToAtomPrim <-       readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_AtomPrimitive.csv")
varPointsToRuleMerge <-      readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_RuleMerge.csv")
varPointsToRuleResult <-     readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_RuleResult.csv")
varPointsToQueryUnion <-     readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryUnion.csv")
varPointsToQueryIterate <-   readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryIterate.csv")
varPointsToQueryStable <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryStable.csv")
varPointsToQueryResult <-    readMeasurement("VarPointsTo_minijavac_VarPointsTo_var;heap_PureInto_QueryResult.csv")

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

pathInto10  <- mean(readMeasurement("Path-StepInto10.csv"))
pathInto20  <- mean(readMeasurement("Path-StepInto20.csv"))
pathInto30  <- mean(readMeasurement("Path-StepInto30.csv"))
pathInto40  <- mean(readMeasurement("Path-StepInto40.csv"))
pathInto50  <- mean(readMeasurement("Path-StepInto50.csv"))
pathInto60  <- mean(readMeasurement("Path-StepInto60.csv"))
pathInto70  <- mean(readMeasurement("Path-StepInto70.csv"))
pathInto80  <- mean(readMeasurement("Path-StepInto80.csv"))
pathInto90  <- mean(readMeasurement("Path-StepInto90.csv"))
pathInto100 <- mean(readMeasurement("Path-StepInto100.csv"))
pathIntoAll <- c(pathInto10, pathInto20, pathInto30, pathInto40, pathInto50, pathInto60, pathInto70, pathInto80, pathInto90, pathInto100)

pathOver10  <- mean(readMeasurement("Path-StepOver10.csv"))
pathOver20  <- mean(readMeasurement("Path-StepOver20.csv"))
pathOver30  <- mean(readMeasurement("Path-StepOver30.csv"))
pathOver40  <- mean(readMeasurement("Path-StepOver40.csv"))
pathOver50  <- mean(readMeasurement("Path-StepOver50.csv"))
pathOver60  <- mean(readMeasurement("Path-StepOver60.csv"))
pathOver70  <- mean(readMeasurement("Path-StepOver70.csv"))
pathOver80  <- mean(readMeasurement("Path-StepOver80.csv"))
pathOver90  <- mean(readMeasurement("Path-StepOver90.csv"))
pathOver100 <- mean(readMeasurement("Path-StepOver100.csv"))
pathOver110 <- mean(readMeasurement("Path-StepOver110.csv"))
pathOver120 <- mean(readMeasurement("Path-StepOver120.csv"))
pathOver130 <- mean(readMeasurement("Path-StepOver130.csv"))
pathOver140 <- mean(readMeasurement("Path-StepOver140.csv"))
pathOver150 <- mean(readMeasurement("Path-StepOver150.csv"))
pathOver160 <- mean(readMeasurement("Path-StepOver160.csv"))
pathOver170 <- mean(readMeasurement("Path-StepOver170.csv"))
pathOver180 <- mean(readMeasurement("Path-StepOver180.csv"))
pathOver190 <- mean(readMeasurement("Path-StepOver190.csv"))
pathOver200 <- mean(readMeasurement("Path-StepOver200.csv"))
pathOver210 <- mean(readMeasurement("Path-StepOver210.csv"))
pathOver220 <- mean(readMeasurement("Path-StepOver220.csv"))
pathOver230 <- mean(readMeasurement("Path-StepOver230.csv"))
pathOver240 <- mean(readMeasurement("Path-StepOver240.csv"))
pathOver250 <- mean(readMeasurement("Path-StepOver250.csv"))
pathOver260 <- mean(readMeasurement("Path-StepOver260.csv"))
pathOver270 <- mean(readMeasurement("Path-StepOver270.csv"))
pathOver280 <- mean(readMeasurement("Path-StepOver280.csv"))
pathOver290 <- mean(readMeasurement("Path-StepOver290.csv"))
pathOver300 <- mean(readMeasurement("Path-StepOver300.csv"))
pathOver310 <- mean(readMeasurement("Path-StepOver310.csv"))
pathOver320 <- mean(readMeasurement("Path-StepOver320.csv"))
pathOver330 <- mean(readMeasurement("Path-StepOver330.csv"))
pathOver340 <- mean(readMeasurement("Path-StepOver340.csv"))
pathOver350 <- mean(readMeasurement("Path-StepOver350.csv"))
pathOver360 <- mean(readMeasurement("Path-StepOver360.csv"))
pathOver370 <- mean(readMeasurement("Path-StepOver370.csv"))
pathOver380 <- mean(readMeasurement("Path-StepOver380.csv"))
pathOver390 <- mean(readMeasurement("Path-StepOver390.csv"))
pathOver400 <- mean(readMeasurement("Path-StepOver400.csv"))
pathOver410 <- mean(readMeasurement("Path-StepOver410.csv"))
pathOver420 <- mean(readMeasurement("Path-StepOver420.csv"))
pathOver430 <- mean(readMeasurement("Path-StepOver430.csv"))
pathOver440 <- mean(readMeasurement("Path-StepOver440.csv"))
pathOver450 <- mean(readMeasurement("Path-StepOver450.csv"))
pathOver460 <- mean(readMeasurement("Path-StepOver460.csv"))
pathOver470 <- mean(readMeasurement("Path-StepOver470.csv"))
pathOver480 <- mean(readMeasurement("Path-StepOver480.csv"))
pathOver490 <- mean(readMeasurement("Path-StepOver490.csv"))
pathOver500 <- mean(readMeasurement("Path-StepOver500.csv"))
pathOver510 <- mean(readMeasurement("Path-StepOver510.csv"))
pathOver520 <- mean(readMeasurement("Path-StepOver520.csv"))
pathOver530 <- mean(readMeasurement("Path-StepOver530.csv"))
pathOver540 <- mean(readMeasurement("Path-StepOver540.csv"))
pathOver550 <- mean(readMeasurement("Path-StepOver550.csv"))
pathOver560 <- mean(readMeasurement("Path-StepOver560.csv"))
pathOver570 <- mean(readMeasurement("Path-StepOver570.csv"))
pathOver580 <- mean(readMeasurement("Path-StepOver580.csv"))
pathOver590 <- mean(readMeasurement("Path-StepOver590.csv"))
pathOver600 <- mean(readMeasurement("Path-StepOver600.csv"))
pathOver610 <- mean(readMeasurement("Path-StepOver610.csv"))
pathOver620 <- mean(readMeasurement("Path-StepOver620.csv"))
pathOver630 <- mean(readMeasurement("Path-StepOver630.csv"))
pathOver640 <- mean(readMeasurement("Path-StepOver640.csv"))
pathOver650 <- mean(readMeasurement("Path-StepOver650.csv"))
pathOver660 <- mean(readMeasurement("Path-StepOver660.csv"))
pathOver670 <- mean(readMeasurement("Path-StepOver670.csv"))
pathOver680 <- mean(readMeasurement("Path-StepOver680.csv"))
pathOver690 <- mean(readMeasurement("Path-StepOver690.csv"))
pathOver700 <- mean(readMeasurement("Path-StepOver700.csv"))
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
  pathOver500,
  pathOver510,
  pathOver520,
  pathOver530,
  pathOver540,
  pathOver550,
  pathOver560,
  pathOver570,
  pathOver580,
  pathOver590,
  pathOver600,
  pathOver610,
  pathOver620,
  pathOver630,
  pathOver640,
  pathOver650,
  pathOver660,
  pathOver670,
  pathOver680,
  pathOver690,
  pathOver700
  )

pathStepsInto10  <- mean(readSteps("Path-StepInto10.csv"))
pathStepsInto20  <- mean(readSteps("Path-StepInto20.csv"))
pathStepsInto30  <- mean(readSteps("Path-StepInto30.csv"))
pathStepsInto40  <- mean(readSteps("Path-StepInto40.csv"))
pathStepsInto50  <- mean(readSteps("Path-StepInto50.csv"))
pathStepsInto60  <- mean(readSteps("Path-StepInto60.csv"))
pathStepsInto70  <- mean(readSteps("Path-StepInto70.csv"))
pathStepsInto80  <- mean(readSteps("Path-StepInto80.csv"))
pathStepsInto90  <- mean(readSteps("Path-StepInto90.csv"))
pathStepsInto100 <- mean(readSteps("Path-StepInto100.csv"))
pathStepsIntoAll <- c(pathStepsInto10, pathStepsInto20, pathStepsInto30, pathStepsInto40, pathStepsInto50, pathStepsInto60, pathStepsInto70, pathStepsInto80, pathStepsInto90, pathStepsInto100)

pathStepsOver10  <- mean(readMeasurement("Path-StepOver10.csv"))
pathStepsOver20  <- mean(readMeasurement("Path-StepOver20.csv"))
pathStepsOver30  <- mean(readMeasurement("Path-StepOver30.csv"))
pathStepsOver40  <- mean(readMeasurement("Path-StepOver40.csv"))
pathStepsOver50  <- mean(readMeasurement("Path-StepOver50.csv"))
pathStepsOver60  <- mean(readMeasurement("Path-StepOver60.csv"))
pathStepsOver70  <- mean(readMeasurement("Path-StepOver70.csv"))
pathStepsOver80  <- mean(readMeasurement("Path-StepOver80.csv"))
pathStepsOver90  <- mean(readMeasurement("Path-StepOver90.csv"))
pathStepsOver100 <- mean(readMeasurement("Path-StepOver100.csv"))
pathStepsOver110 <- mean(readMeasurement("Path-StepOver110.csv"))
pathStepsOver120 <- mean(readMeasurement("Path-StepOver120.csv"))
pathStepsOver130 <- mean(readMeasurement("Path-StepOver130.csv"))
pathStepsOver140 <- mean(readMeasurement("Path-StepOver140.csv"))
pathStepsOver150 <- mean(readMeasurement("Path-StepOver150.csv"))
pathStepsOver160 <- mean(readMeasurement("Path-StepOver160.csv"))
pathStepsOver170 <- mean(readMeasurement("Path-StepOver170.csv"))
pathStepsOver180 <- mean(readMeasurement("Path-StepOver180.csv"))
pathStepsOver190 <- mean(readMeasurement("Path-StepOver190.csv"))
pathStepsOver200 <- mean(readMeasurement("Path-StepOver200.csv"))
pathStepsOver210 <- mean(readMeasurement("Path-StepOver210.csv"))
pathStepsOver220 <- mean(readMeasurement("Path-StepOver220.csv"))
pathStepsOver230 <- mean(readMeasurement("Path-StepOver230.csv"))
pathStepsOver240 <- mean(readMeasurement("Path-StepOver240.csv"))
pathStepsOver250 <- mean(readMeasurement("Path-StepOver250.csv"))
pathStepsOver260 <- mean(readMeasurement("Path-StepOver260.csv"))
pathStepsOver270 <- mean(readMeasurement("Path-StepOver270.csv"))
pathStepsOver280 <- mean(readMeasurement("Path-StepOver280.csv"))
pathStepsOver290 <- mean(readMeasurement("Path-StepOver290.csv"))
pathStepsOver300 <- mean(readMeasurement("Path-StepOver300.csv"))
pathStepsOver310 <- mean(readMeasurement("Path-StepOver310.csv"))
pathStepsOver320 <- mean(readMeasurement("Path-StepOver320.csv"))
pathStepsOver330 <- mean(readMeasurement("Path-StepOver330.csv"))
pathStepsOver340 <- mean(readMeasurement("Path-StepOver340.csv"))
pathStepsOver350 <- mean(readMeasurement("Path-StepOver350.csv"))
pathStepsOver360 <- mean(readMeasurement("Path-StepOver360.csv"))
pathStepsOver370 <- mean(readMeasurement("Path-StepOver370.csv"))
pathStepsOver380 <- mean(readMeasurement("Path-StepOver380.csv"))
pathStepsOver390 <- mean(readMeasurement("Path-StepOver390.csv"))
pathStepsOver400 <- mean(readMeasurement("Path-StepOver400.csv"))
pathStepsOver410 <- mean(readMeasurement("Path-StepOver410.csv"))
pathStepsOver420 <- mean(readMeasurement("Path-StepOver420.csv"))
pathStepsOver430 <- mean(readMeasurement("Path-StepOver430.csv"))
pathStepsOver440 <- mean(readMeasurement("Path-StepOver440.csv"))
pathStepsOver450 <- mean(readMeasurement("Path-StepOver450.csv"))
pathStepsOver460 <- mean(readMeasurement("Path-StepOver460.csv"))
pathStepsOver470 <- mean(readMeasurement("Path-StepOver470.csv"))
pathStepsOver480 <- mean(readMeasurement("Path-StepOver480.csv"))
pathStepsOver490 <- mean(readMeasurement("Path-StepOver490.csv"))
pathStepsOver500 <- mean(readMeasurement("Path-StepOver500.csv"))
pathStepsOver510 <- mean(readMeasurement("Path-StepOver510.csv"))
pathStepsOver520 <- mean(readMeasurement("Path-StepOver520.csv"))
pathStepsOver530 <- mean(readMeasurement("Path-StepOver530.csv"))
pathStepsOver540 <- mean(readMeasurement("Path-StepOver540.csv"))
pathStepsOver550 <- mean(readMeasurement("Path-StepOver550.csv"))
pathStepsOver560 <- mean(readMeasurement("Path-StepOver560.csv"))
pathStepsOver570 <- mean(readMeasurement("Path-StepOver570.csv"))
pathStepsOver580 <- mean(readMeasurement("Path-StepOver580.csv"))
pathStepsOver590 <- mean(readMeasurement("Path-StepOver590.csv"))
pathStepsOver600 <- mean(readMeasurement("Path-StepOver600.csv"))
pathStepsOver610 <- mean(readMeasurement("Path-StepOver610.csv"))
pathStepsOver620 <- mean(readMeasurement("Path-StepOver620.csv"))
pathStepsOver630 <- mean(readMeasurement("Path-StepOver630.csv"))
pathStepsOver640 <- mean(readMeasurement("Path-StepOver640.csv"))
pathStepsOver650 <- mean(readMeasurement("Path-StepOver650.csv"))
pathStepsOver660 <- mean(readMeasurement("Path-StepOver660.csv"))
pathStepsOver670 <- mean(readMeasurement("Path-StepOver670.csv"))
pathStepsOver680 <- mean(readMeasurement("Path-StepOver680.csv"))
pathStepsOver690 <- mean(readMeasurement("Path-StepOver690.csv"))
pathStepsOver700 <- mean(readMeasurement("Path-StepOver700.csv"))
pathStepsOverAll <- c(
  pathStepsOver10,
  pathStepsOver20,
  pathStepsOver30,
  pathStepsOver40,
  pathStepsOver50,
  pathStepsOver60,
  pathStepsOver70,
  pathStepsOver80,
  pathStepsOver90,
  pathStepsOver100,
  pathStepsOver110,
  pathStepsOver120,
  pathStepsOver130,
  pathStepsOver140,
  pathStepsOver150,
  pathStepsOver160,
  pathStepsOver170,
  pathStepsOver180,
  pathStepsOver190,
  pathStepsOver200,
  pathStepsOver210,
  pathStepsOver220,
  pathStepsOver230,
  pathStepsOver240,
  pathStepsOver250,
  pathStepsOver260,
  pathStepsOver270,
  pathStepsOver280,
  pathStepsOver290,
  pathStepsOver300,
  pathStepsOver310,
  pathStepsOver320,
  pathStepsOver330,
  pathStepsOver340,
  pathStepsOver350,
  pathStepsOver360,
  pathStepsOver370,
  pathStepsOver380,
  pathStepsOver390,
  pathStepsOver400,
  pathStepsOver410,
  pathStepsOver420,
  pathStepsOver430,
  pathStepsOver440,
  pathStepsOver450,
  pathStepsOver460,
  pathStepsOver470,
  pathStepsOver480,
  pathStepsOver490,
  pathStepsOver500,
  pathStepsOver510,
  pathStepsOver520,
  pathStepsOver530,
  pathStepsOver540,
  pathStepsOver550,
  pathStepsOver560,
  pathStepsOver570,
  pathStepsOver580,
  pathStepsOver590,
  pathStepsOver600,
  pathStepsOver610,
  pathStepsOver620,
  pathStepsOver630,
  pathStepsOver640,
  pathStepsOver650,
  pathStepsOver660,
  pathStepsOver670,
  pathStepsOver680,
  pathStepsOver690,
  pathStepsOver700
)

pdf(file = paste(graphpath, "Path-Time.pdf", sep="/"))
plot(pathOverAll,
     type = "o",
     col = durationColors[1],
     ylab = "Total Time in milliseconds",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 70000),
)
lines(pathIntoAll, type = "o", col = durationColors[2])
axis(1,at=c(1:70),labels=seq(10, 700, by = 10))
legend("topright", legend=c("step-over", "step-into"),
       col=c(durationColors[1], durationColors[2]), lty=1:1)
dev.off()
# exponential regression for path-step-into
pathintotime.data <- data.frame(time=pathIntoAll, size=seq(10, 100, by = 10))
pathintotime.reg <- lm(log(time)~size, data= pathintotime.data)
summary(pathintotime.reg)

# linear regression for path-step-over
pathovertime.data <- data.frame(time=pathOverAll, size=seq(10, 700, by = 10))
pathovertime.reg <- lm(time~size, data= pathovertime.data)
summary(pathovertime.reg)


pdf(file = paste(graphpath, "Path-Steps.pdf", sep="/"))
plot(pathStepsOverAll,
     type = "o",
     col = durationColors[1],
     ylab = "# Steps",
     # names = seq(10, 500, by = 10),
     # col = durationColors
     xaxt='n',
     ylim = c(0, 120000),
)
lines(pathStepsIntoAll, type = "o", col = durationColors[2])
axis(1,at=c(1:70),labels=seq(10, 700, by = 10))
legend("topright", legend=c("step-over", "step-into"),
       col=c(durationColors[1], durationColors[2]), lty=1:1)
dev.off()

