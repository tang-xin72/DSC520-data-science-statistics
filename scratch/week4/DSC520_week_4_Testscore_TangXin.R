setwd('~/Dsc520')
library(ggplot2)
library(readr)
theUrl <- "http://content.bellevue.edu/cst/dsc/520/id/resources/scores.csv"
data <- read.csv(file = theUrl, header =TRUE, sep =',')
head(data)
str(data)

regular_data <- subset(data, Section=='Regular')

sports_data <- subset(data, Section=='Sports') 
#head(sports_data)
#plot(regular_data$Count, regular_data$Score)
par (mfrow = c(2, 1))
plot(Count ~ Score, data =regular_data,
     xlim = c(200, 400),
     main = "Regular Section: Student achieved each score level",
     xlab = "Course Grade",
     ylab = "Count of students",
     col = 'red')

plot(Count ~ Score, data =sports_data,
     xlim = c(200, 400),
     main = "Sports Section: Student achieved each score level",
     xlab = "Course Grade",
     ylab = "Count of students",
     col = 'blue')

par_mfrow = c(1, 1)