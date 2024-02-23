wd <- getwd()
# TODO change paths
datapath <- paste(wd, "benchmark/mono/asg", sep="/")
graphpath <- paste(wd, "benchmark/mono", sep="/")

color1 <- rgb(255/256, 255/256, 204/256)
color2 <- rgb(161/256, 218/256, 180/256)
color3 <- rgb(65/256, 182/256, 196/256)
color4 <- rgb(34/256, 94/256, 168/256)

read <- function(fileName) {
  csv <- read.csv(paste(datapath, fileName, sep="/"), sep = ",", dec = ".")
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


asgDL <- colMeans(readTime("ASG_DL.csv"))
asgMono <- colMeans(readTime("ASG_Mono.csv"))
asgMonoOpt <- colMeans(readTime("ASG_Mono_opt.csv"))

pdf(file = paste(graphpath, "AbstractSyntaxGraph.pdf", sep="/"))
plot(data.matrix(asgDL),
     main = "(A) Measuring execution time of ASG example",
     type = "o",
     col = color2,
     ylab = "Running time (s)",
     xlab = "Number of nodes",
     xaxt = "n",
     ylim = c(0, 280), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
lines(data.matrix(asgMono), type = "o", col = color3, lwd = 1.5)
lines(data.matrix(asgMonoOpt), type = "o", col = color4, lwd = 1.5)
axis(1, at = c(1:11), labels = seq(10, 110, by = 10))
legend("topleft", legend=c("Datalog", "Mono", "Mono Opt"),
       col=c(color2, color3, color4), lty=1:1, lwd = 3)
dev.off()