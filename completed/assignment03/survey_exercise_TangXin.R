setwd('~/Dsc520')
library(ggplot2)
library(readr)
theUrl <- "http://content.bellevue.edu/cst/dsc/520/id/resources/acs-14-1yr-s0201.csv"
data <- read.csv(file = theUrl, header =TRUE, sep =',')
#data <- read_delim(file = theUrl, delim = ',')
str(data)
nrow(data)
ncol(data)
head(data)
#curve(dnorm(x, mean = mean(data$HSDegree), sd =sd(data$HSDegree)) 
ggplot(data, aes(HSDegree)) + geom_histogram(aes(y= ..density..)) + ggtitle("HSdree holder precentage") + xlab("Percentage in population") +ylab("Density") +
  stat_function(fun = dnorm, args = list(mean = mean(data$HSDegree), sd = sd(data$HSDegree)), color = "red")
#ggplot(data, aes(HSDegree)) + geom_histogram(bins = 10) + ggtitle("HSdree holder precentage") + xlab("HS drgree count") +ylab("frequency") +
#stat_function(fun = dnorm, args = list(mean = mean(data$HSDegree), sd = sd(data$HSDegree)), color = "red")
ggplot(data, aes(HSDegree)) + geom_density()
stat.desc(data$HSDegree, basic = TRUE, desc =TRUE, norm = TRUE, p = 0.95)
ggplot(data, aes(HSDegree)) + geom_density() + stat_function(fun = dnorm, args = list(mean = mean(data$HSDegree), sd = sd(data$HSDegree)), color = "red")

ggplot(data, aes(HSDegree)) + geom_histogram(aes(y= ..density..)) +
stat_function(fun = dnorm, args = list(mean = mean(data$HSDegree), sd = sd(data$HSDegree)), color = "red")