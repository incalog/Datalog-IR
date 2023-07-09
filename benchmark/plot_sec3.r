wd <- getwd()
# TODO change paths
datapath <- paste(wd, "benchmark/objectoriented/sec3", sep="/")
graphpath <- paste(wd, "benchmark/objectoriented/graphs/sec3", sep="/")

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



pathNodeLeft <- colMeans(readTime("Path_Interpreter_left_recursive_node.csv"))
pathNodeRight <- colMeans(readTime("Path_Interpreter_right_recursive_node.csv"))
pathInterpLeft <- colMeans(readTime("Path_Interpreter_left_recursive.csv"))
pathInterpRight <- colMeans(readTime("Path_Interpreter_right_recursive.csv"))
pathIrLeft <- colMeans(readTime("Path_IR_left_recursive.csv"))
pathIrRight <- colMeans(readTime("Path_IR_right_recursive.csv"))

pdf(file = paste(graphpath, "Path-left-recrusive.pdf", sep="/"))
plot(data.matrix(pathIrLeft),
     main = "(A) Measuring execution time of left-recursive path example",
     type = "o",
     col = color2,
     ylab = "Running time (ms)",
     xlab = "Number of nodes",
     xaxt = "n",
     ylim = c(0, 70000), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(pathInterpLeft), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(pathNodeLeft), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:6), labels = seq(10, 120, by = 20))
legend("topleft", legend=c("Datalog", "Interpreter", "Interpreter with Nodes"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()

pdf(file = paste(graphpath, "Path-right-recrusive.pdf", sep="/"))
plot(data.matrix(pathIrRight),
     main = "(A) Measuring execution time of right-recursive path example",
     type = "o",
     col = color2,
     ylab = "Running time (ms)",
     xlab = "Number of nodes",
     xaxt = "n",
     ylim = c(0, 70000), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(pathInterpRight), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(pathNodeRight), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:6), labels = seq(10, 120, by = 20))
legend("topleft", legend=c("Datalog", "Interpreter", "Interpreter with Nodes"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()