
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

methodLookupAll <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_ALL.csv")
methodLookupAtomEDB <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomEDB.csv")
methodLookupAtomInto <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomInto.csv")
methodLookupAtomSkip <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomSkip.csv")
methodLookupAtomPrimitive <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_AtomPrimitive.csv")
methodLookupRuleMerge <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_RuleMerge.csv")
methodLookupRuleResult <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_RuleResult.csv")
methodLookupQueryUnion <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryUnion.csv")
methodLookupQueryIterate <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryIterate.csv")
methodLookupQueryStable <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryStable.csv")
methodLookupQueryResult <- preprocessCSV("VarPointsTo_minijavac_basic_MethodLookup_simplename_PureInto_QueryResult.csv")

subtypeOfAll <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_ALL.csv")
subtypeOfAllAtomEDB <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomEDB.csv")
subtypeOfAllAtomInto <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomInto.csv")
subtypeOfAllAtomSkip <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomSkip.csv")
subtypeOfAllAtomEq <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_AtomEq.csv")
subtypeOfAllRuleMerge <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_RuleMerge.csv")
subtypeOfAllRuleResult <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_RuleResult.csv")
subtypeOfAllQueryUnion <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryUnion.csv")
subtypeOfAllQueryIterate <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryIterate.csv")
subtypeOfAllQueryStable <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryStable.csv")
subtypeOfAllQueryResult <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_QueryResult.csv")

# subtypeOfRes <- preprocessCSV("VarPointsTo_minijavac_basic_SubtypeOf_subtype_PureInto_StepInto.csv")

typeCol <- rgb(127/256, 205/256, 187/256)
constantCol <- rgb(44/256, 127/256, 184/256)
taintCol <- rgb(237/256, 248/256, 177/256)

durationColors <- c(rgb(127/256, 205/256, 187/256), rgb(44/256, 127/256, 184/256), rgb(237/256, 248/256, 177/256))
colors <- c(rgb(127/256, 205/256, 187/256), rgb(44/256, 127/256, 184/256), rgb(44/256, 127/256, 184/256), rgb(237/256, 248/256, 177/256))
options(scipen=999)

pdf(file = paste(path, "MethodLookup-IntoV2.pdf", sep="/"))
methodIntoPlot <- boxplot(methodLookupRuleMerge, methodLookupRuleResult, methodLookupAtomEDB, methodLookupAtomPrimitive, methodLookupAtomInto, methodLookupAtomSkip, methodLookupQueryUnion, methodLookupQueryStable, methodLookupQueryIterate, methodLookupQueryResult, methodLookupAll,
        # main = "Multiple boxplots for comparision",
        xlab = "Time per step in milliseconds",
        names = c("R-Merge", "R-Result", "A-EDB", "A-Prim", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
        # las = 2,
        log = "y",
        # ylim = c(0.001, 350),
        outline = FALSE,
        col = durationColors
)
# TODO rotate x axis 45 degrees
dev.off()

pdf(file = paste(path, "SubtypeOf-IntoV2.pdf", sep="/"))
methodIntoPlot <- boxplot(subtypeOfAllRuleMerge, subtypeOfAllRuleResult, subtypeOfAllAtomEDB, subtypeOfAllAtomEq, subtypeOfAllAtomInto, subtypeOfAllAtomSkip, subtypeOfAllQueryUnion, subtypeOfAllQueryStable, subtypeOfAllQueryIterate, subtypeOfAllQueryResult, subtypeOfAll,
                          # main = "Multiple boxplots for comparision",
                          xlab = "Time per step in milliseconds",
                          names = c("R-Merge", "R-Result", "A-EDB", "A-Eq", "A-Into", "A-Skip", "Q-Union", "Q-Stable", "Q-Iterate", "Q-Result", "All"),
                          # las = 2,
                          log = "y",
                          # ylim = c(0.001, 350),
                          outline = FALSE,
                          col = durationColors
)
# TODO rotate x axis 45 degrees
dev.off()

# step over performance
# pdf(file = paste(path, "VarPointsTo_minijavac_basic_MethodLookup_simplename-StepOver.pdf", sep="/"))
# boxplot(methodLookupStepMs,
#         # main = "Multiple boxplots for comparision",
#         xlab = "Time per step in milliseconds",
#         names = c("MethodLookup"),
#         outline = FALSE
#         # las = 2,
#         #ylim = c(0, 1024)
#         #col = durationColors
# )
# # means <- c(mean(methodLookupMemory))
# # points(means, pch = 'x', col = "red" )
# # text(means, labels = paste(round(means), "s"), col = "red", pos = 4, offset = 2.5)
# dev.off()


#
# typeDuration <- typeCsv$duration / 1000
# constantDuration <- constantCsv$duration / 1000
# taintDuration <- taintCsv$duration / 1000
# binaryenDuration <- binaryenCsv$duration / 1000
#
# pdf(file = paste(path, "wasmbench-mgc-duration.pdf", sep="/"))
# boxplot(typeDuration, constantDuration, taintDuration,
#         # main = "Multiple boxplots for comparision",
#         ylab = "Running times in seconds",
#         names = c("type analysis", "constant analysis", "taint analysis"),
#         # las = 2,
#         ylim = c(0, 45),
#         col = durationColors
# )
# means <- c(mean(typeDuration), mean(constantDuration), mean(taintDuration))
# points(means, pch = 'x', col = "red" )
# text(means, labels = paste(round(means), "s"), col = "red", pos = 4, offset = 2.5)
# dev.off()
#
#
#
# typeDead <- typeCsv$deadInstructionPercent
# typeDeadMedian <- median(typeDead)
# constantDead <- constantCsv$deadInstructionPercent
# constantDeadMedian <- median(constantDead)
# constantConstant <- constantCsv$constantInstructionPercent
# constantConstantMedian <- median(constantConstant)
# constantElim <- constantCsv$eliminatablePercent
# constantElimMedian <- median(constantElim)
# constantElimMean <- mean(constantElim)
# safeMem <- 100 - taintCsv$taintedAccessesPercent
# safeMemMedian <- median(safeMem)
# binaryenDead <- binaryenCsv$deadInstructionPercent
# binaryenDeadMedian <- median(binaryenDead)
# binaryenDeadMean <- mean(binaryenDead)
#
# constantDurationMean <- mean(constantDuration)
# constantDurationMedian <- median(constantDuration)
#
# binaryenDurationMean <- mean(binaryenDuration)
# binaryenDurationMedian <- median(binaryenDuration)
#
# pdf(file = paste(path, "wasmbench-mgc-results.pdf", sep="/"))
# b <- boxplot(typeDead, constantDead, constantConstant, safeMem,
#         # main = "Multiple boxplots for comparision",
#         names = c("dead code\n(type values)", "dead code\n(constant values)", "constant\ninstructions", "safe memory\ninstructions"),
#         # las = 2,
#         # ylim = c(0, 45000),
#         ylab = "Percentage (%) of instructions",
# #        pars=list(outcol=c(typeCol, typeCol, constantCol, taintCol)),
#              # xlab = "Analysis",
#         col = c(typeCol, typeCol, constantCol, taintCol)
# )
# means <- c(mean(typeDead), mean(constantDead), mean(constantConstant), mean(safeMem))
# points(means, pch = 'x', col = "red" )
# text(means, labels = paste(round(means), '%'), col = "red", pos = 4, offset = 2.5)
# dev.off()
#
# typeSuccessRuns <- length(typeCsv$duration)
# typeSuccessRuns10s <- length(typeDuration[typeDuration <= 10]) / typeSuccessRuns
# typeErrorMsgs <- typeErrorsCsv$exceptionMsg
# typeTimeouts <- length(typeErrorMsgs[typeErrorMsgs=="java.lang.InterruptedException"])
# typeInvalidImport <- length(typeErrorMsgs[grepl("No module with name", typeErrorMsgs)])
# typeInvalidMemory <- length(typeErrorMsgs[grepl("swam.validation.ValidationException: memory size may not exceed 1024 pages", typeErrorMsgs)])
# typeInvalidHostFunction <- length(typeErrorMsgs[grepl("host", typeErrorMsgs)])
# typeParseError <- length(typeErrorMsgs[grepl("WasmParseError", typeErrorMsgs)])
# typeOtherErrors <- length(typeErrorMsgs) - typeTimeouts - typeInvalidImport - typeInvalidMemory - typeInvalidHostFunction - typeParseError
#
# constantSuccessRuns <- length(constantCsv$duration)
# constantSuccessRuns10s <- length(constantDuration[constantDuration <= 10]) / constantSuccessRuns
# constantErrorMsgs <- constantErrorsCsv$exceptionMsg
# constantTimeouts <- length(constantErrorMsgs[constantErrorMsgs=="java.lang.InterruptedException"])
# constantInvalidImport <- length(constantErrorMsgs[grepl("No module with name", constantErrorMsgs)])
# constantInvalidMemory <- length(constantErrorMsgs[grepl("swam.validation.ValidationException: memory size may not exceed 1024 pages", constantErrorMsgs)])
# constantInvalidHostFunction <- length(constantErrorMsgs[grepl("host", constantErrorMsgs)])
# constantParseError <- length(constantErrorMsgs[grepl("WasmParseError", constantErrorMsgs)])
# constantOtherErrors <- length(constantErrorMsgs) - constantTimeouts - constantInvalidImport - constantInvalidMemory - constantInvalidHostFunction - constantParseError
#
# taintSuccessRuns <- length(taintCsv$duration)
# taintSuccessRuns10s <- length(taintDuration[taintDuration <= 10]) / taintSuccessRuns
# taintErrorMsgs <- taintErrorsCsv$exceptionMsg
# taintTimeouts <- length(taintErrorMsgs[constantErrorMsgs=="java.lang.InterruptedException"])
# taintInvalidImport <- length(taintErrorMsgs[grepl("No module with name", constantErrorMsgs)])
# taintInvalidMemory <- length(taintErrorMsgs[grepl("swam.validation.ValidationException: memory size may not exceed 1024 pages", constantErrorMsgs)])
# taintInvalidHostFunction <- length(taintErrorMsgs[grepl("host", constantErrorMsgs)])
# taintParseError <- length(taintErrorMsgs[grepl("WasmParseError", constantErrorMsgs)])
# taintOtherErrors <- length(taintErrorMsgs) - constantTimeouts - constantInvalidImport - constantInvalidMemory - constantInvalidHostFunction - constantParseError
#
#
# memsafeInstPercent <- 100 - taintCsv$taintedAccessesPercent
# memsafeBinaries <- length(which(memsafeInstPercent == 100)) / length(memsafeInstPercent)
# memsafeInstMean <- mean(memsafeInstPercent)
#
#
#
#
# pdf(file = paste(path, "wasmbench-binaryen-compare.pdf", sep="/"))
# b <- boxplot(constantElim, binaryenDead,
#              # main = "Multiple boxplots for comparision",
#              names = c("eliminated by us", "eliminated by binaryen"),
#              # las = 2,
#              ylim = c(0, 100),
#              ylab = "Percentage (%) of instructions",
#              #        pars=list(outcol=c(typeCol, typeCol, constantCol, taintCol)),
#              # xlab = "Analysis",
#              col = c(constantCol, taintCol)
# )
# means <- c(mean(constantElim), mean(binaryenDead))
# points(means, pch = 'x', col = "red" )
# text(means, labels = paste(round(means), '%'), col = "red", pos = 4, offset = 2.5)
# dev.off()
#
#
# # pdf(file = paste(path, "wasmbench-mgc-memsafe.pdf", sep="/"))
# # hist(memsafeYesNo)
# # dev.off()
