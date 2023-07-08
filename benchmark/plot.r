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





##### EDB

#pathDatalogEDBLeft <- colMeans(readTime("Path_Datalog_left_edb_recursive.csv"))
pathDatalogEDBRight <- colMeans(readTime("Path_Datalog_right_edb_recursive.csv"))
#pathInterpEDBLeft <- colMeans(readTime("Path_Interpreter_left_edb_recursive.csv"))
#pathInterpEDBRight <- colMeans(readTime("Path_Interpreter_right_edb_recursive.csv"))
#pathIrEDBLeft <- colMeans(readTime("Path_IR_left_edb_recursive.csv"))
pathIrEDBRight <- colMeans(readTime("Path_IR_right_edb_recursive.csv"))

pdf(file = paste(graphpath, "Path-right-edb-recrusive.pdf", sep="/"))
plot(data.matrix(pathDatalogEDBRight),
     main = "(A) Measuring execution time of right-recursive path example",
     type = "o",
     col = color2,
     ylab = "Running time (ms)",
     xlab = "X",
     xaxt = "n",
     ylim = c(0, 15000), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
#lines(data.matrix(pathInterpEDBRight), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(pathIrEDBRight), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:7), labels = seq(10, 140, by = 20))
legend("topright", legend=c("OODL-datalog", "OODL-Interpreter", "Datalog"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()