setwd("C:/Users/Tay/Downloads/NUS/ST/ST2137/Tutorials/Tutorial 4")

######################################### Q1d ##################################################

#import the wip data set
wip <- read.table("wip.txt", header = T)
attach(wip)

# extract times for plant 1 and plant 2
plant1Time <- wip[plant == 1,]
plant2Time <- wip[plant == 2,]

#get descriptive statistics (mean, median, first quartile, third quartile, minimum, maximum, 
#range, interquartile range, variance and standard deviation) for both plants

summary_stats <- function(x){
  cat("Descriptive statistics:\n\n")
  show(summary(x))
  cat("Range:", range(x), "\n")
  cat("Interquartile range:", IQR(x), "\n")
  cat("Variance:", var(x), "\n")
  cat("Standard deviation:", sd(x), "\n")
}

summary_stats(plant1Time$time)
summary_stats(plant2Time$time)

###################################### plot boxplot #########################################

# input is usually variable ~ category
boxplot(time~plant)

#################################### plot histogram #########################################

#to specify 2 graphs in one column in one page
par(mfrow = c(1,1))

#histogram for the time of plant 1
hist(plant1Time$time, include.lowest = T, freq = T, col = "grey", 
     main = paste("Histogram of processing time for plant 1"), 
     xlab = "Processing Time", ylab = "Frequency")

#histogram for the time of plant 2
hist(plant2Time$time, include.lowest = T, freq = T, col = "grey", 
     main = paste("Histogram of processing time for plant 2"), 
     xlab = "Processing Time", ylab = "Frequency")

