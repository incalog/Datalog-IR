wd <- getwd()

scale <- "mutations"
subdir <- "ScaleMutations"
maxTime <- 3
maxMem <- 500

#scale <- "nodes"
#subdir <- "ScaleObjects"
#maxTime <- 25
#maxMem <- 450

dataPath <- paste("benchmark/objectoriented/counter", subdir, sep="/")

datapath <- paste(wd, dataPath, sep="/")
graphpath <- paste(wd, paste(dataPath, "graphs", sep="/"), sep="/")

color1 <- rgb(255/256, 255/256, 204/256)
color2 <- rgb(161/256, 218/256, 180/256)
color3 <- rgb(65/256, 182/256, 196/256)
color4 <- rgb(34/256, 94/256, 168/256)

read <- function(fileName) {
  csv <- read.csv2(paste(datapath, fileName, sep="/"), sep = ",", dec = ".")
  return(csv)
}




readMeasurement <- function(fileName) {
  csv <- read(fileName)
  vals <- csv$measurement
  valsInMs <- msToS(nsToMs(vals))
  return(valsInMs)
}



readSteps <- function(fileName) {
  csv <- read(fileName)
  vals <- csv$numSteps
  return(vals)
}



readTime <- function(fileName) {
  csv <- read(fileName)
  return(msToS(nsToMs(csv)))
}


readMemory <- function(fileName) {
  csv <- read(fileName)
  return(bytesToMb(csv))
}


bytesToMb <- function(bytes) {
  bytes / (1024 * 1024)
}


nsToMs <- function(ns) {
  ns / 1000000
}

msToS <- function(ms) {
  ms / 1000
}


mutNumericTime <- colMeans(readTime("NumericCounter_Datalog_time.csv"))
mutStructuralTime <- colMeans(readTime("StructuralCounter_Datalog_time.csv"))
mutEfficientStructuralTime <- colMeans(readTime("EfficientStructuralCounter_Datalog_time.csv"))

pdf(file = paste(graphpath, "Mutation_time.pdf", sep="/"))
plot(data.matrix(mutNumericTime),
     main = "(A) Measuring execution time of different mutation counter",
     type = "o",
     col = color2,
     ylab = "Running time (s)",
     xlab = paste("Number of", scale),
     xaxt = "n",
     ylim = c(0, maxTime), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(mutStructuralTime), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(mutEfficientStructuralTime), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:10), labels = seq(100, 1010, by = 100))
legend("topleft", legend=c("Numeric", "Structural", "Eclipse"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()

# Memory

mutNumericMem <- colMeans(readMemory("NumericCounter_Datalog_mem.csv"))
mutStructuralMem <- colMeans(readMemory("StructuralCounter_Datalog_mem.csv"))
mutEfficientStructuralMem <- colMeans(readMemory("EfficientStructuralCounter_Datalog_mem.csv"))

pdf(file = paste(graphpath, "Mutation_mem.pdf", sep="/"))
plot(data.matrix(mutNumericMem),
     main = "(A) Measuring memory consumption of different mutation counter",
     type = "o",
     col = color2,
     ylab = "Memory in (Mb)",
     xlab = paste("Number of", scale),
     xaxt = "n",
     ylim = c(0, maxMem), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(mutStructuralMem), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(mutEfficientStructuralMem), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:10), labels = seq(100, 1010, by = 100))
legend("topleft", legend=c("Numeric", "Structural", "Eclipse"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()