wd <- getwd()
# TODO change paths
datapath <- paste(wd, "benchmark/objectoriented/cfg", sep="/")
graphpath <- paste(wd, "benchmark/objectoriented/graphs/cfg", sep="/")

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



nsToMs <- function(ns) {
  ns / 1000000
}

msToS <- function(ms) {
  ms / 1000
}


cfgInterp <- colMeans(readTime("CFG_Interpreter.csv"))
cfgDatalog <- colMeans(readTime("CFG_Datalog.csv"))

pdf(file = paste(graphpath, "ControlFlowGraph.pdf", sep="/"))
plot(data.matrix(cfgDatalog),
     main = "(A) Measuring execution time of CFG example",
     type = "o",
     col = color2,
     ylab = "Running time (s)",
     xlab = "Number of nodes",
     xaxt = "n",
     ylim = c(0, 23), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(cfgInterp), type = "o", col = color3, lwd = 1.5)
axis(1, at = c(1:6), labels = seq(10, 511, by = 100))
legend("topleft", legend=c("Datalog", "Interpreter"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()