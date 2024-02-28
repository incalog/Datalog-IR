wd <- getwd()
# TODO change paths
datapath <- paste(wd, "benchmark/mono/interval", sep="/")
graphpath <- paste(wd, "benchmark/mono", sep="/")

color1 <- rgb(255/256, 255/256, 204/256)
color2 <- rgb(161/256, 218/256, 180/256)
color3 <- rgb(65/256, 182/256, 196/256)
color4 <- rgb(34/256, 94/256, 168/256)

color5 <- rgb(102/256, 194/256, 165/256)
color6 <- rgb(252/256, 141/256, 98/256)
color7 <- rgb(141/256, 160/256, 203/256)

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


asgDL <- colMeans(readTime("Bak/Interval_DL.csv"))
#asgMono <- colMeans(readTime("Interval_Mono.csv"))
asgMonoOpt <- colMeans(readTime("Bak/Interval_Mono_opt.csv"))

pdf(file = paste(graphpath, "Interval.pdf", sep="/"))
plot(data.matrix(asgDL),
     main = "(A) Measuring execution time of Interval analysis",
     type = "o",
     col = color5,
     ylab = "Running time (s)",
     xlab = "Number of cfg edges",
     xaxt = "n",
     ylim = c(0, 85), # TODO change regarding upper bound of measurevalues
     lwd = 1.5
)
#lines(data.matrix(asgMono), type = "o", col = color6, lwd = 1.5)
lines(data.matrix(asgMonoOpt), type = "o", col = color7, lwd = 1.5)
#axis(1, at = c(1:9), labels = seq(133, 650, by = 60))
axis(1, at = c(1:7), labels = seq(100000, 710000, by = 100000))
#legend("topleft", legend=c("Datalog", "Mono", "Mono Opt"),
legend("topleft", legend=c("Datalog", "Mono Opt"),
       col=c(color5, color7), lty=1:1, lwd = 3)
dev.off()