setwd('~/Dsc520')
library(readr)
#library(readxl)
#library(writexl)
library(dplyr)
#library(utils)
#library(purrr)
library(ggplot2)
library(lm.beta)
library(tidyverse)
library(coefplot)
#library(car)
#library(zoom)
#library(gapminder)
library(scales)

# the surgery data
surgery9 <- read.csv('./data/SurgeryData.csv')

#View(surgery9)


Fatal_Risk <- glm(Risk1Yr ~ AGE + DGN + PRE4 + PRE5 + PRE6 + PRE7 + PRE8 + PRE9 +PRE10 + PRE11 +
                    PRE14 + PRE17 + PRE19 +PRE25 + PRE30 + PRE32, data = surgery9, family = binomial(link = 'logit'))

summary(Fatal_Risk)

coefplot(Fatal_Risk)

#model2
Risk2 <- glm(Risk1Yr ~ AGE + PRE4 + PRE5 + PRE7 + PRE9 + PRE25, data = surgery9, family = binomial(link = 'logit'))
coefplot(Risk2)

#model3
Risk3 <- glm(Risk1Yr ~ PRE14 + PRE7 + PRE9 + PRE25, data = surgery9, family = binomial(link = 'logit'))
coefplot(Risk3)
summary(Risk3)

#prediction result
predict_risk <- predict(Risk3, newdata = surgery9, type='response')

matrix = table(actual = surgery9$Risk1Yr, predict = predict_risk>0.5)
matrix
accuracy1 = (matrix[1,1]/sum(matrix))
accuracy1


# the binary dataset
binary9 <- read.csv('http://content.bellevue.edu/cst/dsc/520/id/resources/binary-classifier-data.csv')

View(binary9)

predict1 <- glm(label ~ x + y, data = binary9, family = binomial(link = 'logit'))
coefplot(predict1)
summary(predict1)

#predict result
predict_binary <- predict(predict1, newdata = binary9, type='response')

matrix2 = table(actual =binary9$label, predict = predict_binary>0.5)
matrix2

accuracy2 = (matrix2[1,1]+matrix2[2,2])/sum(matrix2)
accuracy2
           