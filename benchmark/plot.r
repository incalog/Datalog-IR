wd <- getwd()
# TODO change paths
datapath <- paste(wd, "benchmark/objectoriented", sep="/")
graphpath <- paste(wd, "benchmark/objectoriented/graphs", sep="/")

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



### Path Datalog Time
# TODO check if names are correct
pathDatalogLeft <- colMeans(readTime("Path_Datalog_left_recursive.csv"))
pathDatalogRight <- colMeans(readTime("Path_Datalog_right_recursive.csv"))
pathInterpLeft <- colMeans(readTime("Path_Interpreter_left_recursive.csv"))
pathInterpRight <- colMeans(readTime("Path_Interpreter_right_recursive.csv"))
pathIrLeft <- colMeans(readTime("Path_IR_left_recursive.csv"))
pathIrRight <- colMeans(readTime("Path_IR_right_recursive.csv"))

pdf(file = paste(graphpath, "Path-left-recrusive.pdf", sep="/"))
plot(data.matrix(pathDatalogLeft),
     main = "(A) Measuring execution time of left-recursive path example",
     type = "o",
     col = color2,
     ylab = "Running time (ms)",
     xlab = "X",
     xaxt = "n",
     ylim = c(0, 50000), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(pathInterpLeft), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(pathIrLeft), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:7), labels = seq(10, 140, by = 20))
legend("topright", legend=c("OODL-datalog", "OODL-Interpreter", "Datalog"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()



pdf(file = paste(graphpath, "Path-right-recrusive.pdf", sep="/"))
plot(data.matrix(pathDatalogRight),
     main = "(A) Measuring execution time of right-recursive path example",
     type = "o",
     col = color2,
     ylab = "Running time (ms)",
     xlab = "X",
     xaxt = "n",
     ylim = c(0, 10000), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(pathInterpRight), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(pathIrRight), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:7), labels = seq(10, 140, by = 20))
legend("topright", legend=c("OODL-datalog", "OODL-Interpreter", "Datalog"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()



pathDatalogHeapLeft <- colMeans(readTime("Path_Datalog_left_recursive_dummy.csv"))
#pathDatalogHeapRight <- colMeans(readTime("Path_Datalog_right_recursive_dummy.csv"))
pathInterpHeapLeft <- colMeans(readTime("Path_Interpreter_left_recursive_dummy.csv"))
#pathInterpHeapRight <- colMeans(readTime("Path_Interpreter_right_recursive_dummy.csv"))


pdf(file = paste(graphpath, "Path-left-recrusive-heap.pdf", sep="/"))
plot(data.matrix(pathDatalogHeapLeft),
     main = "(A) Measuring execution time of left-recursive path example",
     type = "o",
     col = color2,
     ylab = "Running time (ms)",
     xlab = "Heap size",
     xaxt = "n",
     ylim = c(0, 25000), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(pathInterpHeapLeft), type = "o", col = color3, lwd = 1.5)
axis(1, at = c(1:10), labels = seq(10, 50000, by = 5000))
legend("topright", legend=c("datalog", "interpreter"),
       col=c(color2, color3), lty=1:1, lwd = 3)
dev.off()


#pdf(file = paste(graphpath, "Path-right-recrusive-heap.pdf", sep="/"))
#plot(data.matrix(pathDatalogHeapRight),
#     main = "(A) Measuring execution time of left-recursive path example",
#     type = "o",
#     col = color2,
#     ylab = "Running time (ms)",
#     xlab = "Heap size",
#     xaxt = "n",
#     ylim = c(0, 25000), # TODO change regarding upper bound of measurevalues
#     lwd = 1.5
#)
#lines(data.matrix(pathInterpHeapRight), type = "o", col = color3, lwd = 1.5)
#axis(1, at = c(1:10), labels = seq(10, 50000, by = 5000))
#legend("topright", legend=c("datalog", "interpreter"),
#       col=c(color2, color3), lty=1:1, lwd = 3)
#dev.off()



#pathDatalogCycleLeft <- colMeans(readTime("Path_Datalog_left_recursive_cycles.csv"))
pathDatalogCycleRight <- colMeans(readTime("Path_Datalog_right_recursive_cycles.csv"))
#pathInterpHeapLeft <- colMeans(readTime("Path_Interpreter_left_recursive_cycles.csv"))
pathInterpCycleRight <- colMeans(readTime("Path_Interpreter_right_recursive_cycles.csv"))


pdf(file = paste(graphpath, "Path-right-recrusive-cycles.pdf", sep="/"))
plot(data.matrix(pathDatalogCycleRight),
     main = "(A) Measuring execution time of right-recursive path example",
     type = "o",
     col = color2,
     ylab = "Running time (ms)",
     xlab = "Number of cycles",
     xaxt = "n",
     ylim = c(0, 80000), # TODO change regarding upper bound of measurevalues
     lwd = 1.5,
)
lines(data.matrix(pathInterpCycleRight), type = "o", col = color3, lwd = 1.5)
axis(1, at = c(1,2,3,5,7.5,15), labels = c(10, 20, 30, 50, 75, 150))
  #seq(10, 21, by = 2))
legend("topright", legend=c("datalog", "interpreter"),
       col=c(color2, color3), lty=1:1, lwd = 3)
dev.off()