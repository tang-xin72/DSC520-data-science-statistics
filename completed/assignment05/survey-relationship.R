setwd('~/Dsc520')
library(ggplot2)
library(readr)
library(dplyr)
library(ppcor)
theUrl <- "http://content.bellevue.edu/cst/dsc/520/id/resources/student-survey.csv"
data <- read.csv(file = theUrl, header =TRUE, sep =',')
dim(data)
#View(data)
as.data.frame(sapply(data,class))

#calculate the covariance of the Survey variables
cov(data)


#calculate the correlation among variable in raw data set
# between time reading and Time TV
Cor_read_TV <- cor(data$TimeReading, data$TimeTV, use = "everything", method = c("pearson", "kendall", "spearman"))
Cor_read_TV
cor.test(data$TimeReading, data$TimeTV, method = c("pearson"), conf.level = 0.99)

#calculate coefficient of determination 
CD_read_TV <- Cor_read_TV^2
CD_read_TV

# between time reading and Happiness
cor_read_happy <- cor(data$TimeReading, data$Happiness, use = "everything", method = c("pearson", "kendall", "spearman"))
cor_read_happy
cor.test(data$TimeReading, data$Happiness, method = c("pearson"), conf.level = 0.99)

#calculate coefficient of determination
CD_read_happy <- cor_read_happy^2
CD_read_happy

# between time on TV and Happiness
cor_TV_happy <- cor(data$TimeTV, data$Happiness, use = "everything", method = c("pearson", "kendall", "spearman"))
cor_TV_happy
cor.test(data$TimeTV, data$Happiness, method = c("pearson"), conf.level = 0.99)

#calculate coefficient of determination
CD_TV_happy <- cor_TV_happy^2
CD_TV_happy

#on all variables
cor(data[, c('TimeReading','TimeTV','Happiness', 'Gender')])

#now split data based on gender.
#checking same correlation on male group
data_male <- data %>% filter(Gender == 1)


cor_TV_happy_male <- cor(data_male$TimeTV, data_male$Happiness, use = "everything", method = c("pearson", "kendall", "spearman"))
cor_TV_happy_male
CD_TV_Happy_male <- cor_TV_happy_male^2

#checking same correlation on female group
data_female <- data %>% filter(Gender == 0)

cor_TV_happy_female <- cor(data_female$TimeTV, data_female$Happiness, use = "everything", method = c("pearson", "kendall", "spearman"))
cor_TV_happy_female

CD_TV_Happy_female <- cor_TV_happy_female^2

#partial correlation
pcor(data[, c('Gender','TimeTV','Happiness')])

