setwd('~/Dsc520')
library(readr)
library(readxl)
#library(writexl)
library(dplyr)
#library(utils)
library(ggplot2)
library(lm.beta)
library(tidyverse)
library(scales)

cancel_data <- read_excel('./scratch/project/flight-cancelled.xlsx', sheet=3)
cancel_data$Year <- as.factor(cancel_data$Year)
cancel_data$Month_factor <- as.factor(cancel_data$Month_factor)
cancel_data$cancel_rate <- cancel_data$cancel_rate*100
#View(cancel_data)


#plot by month
ggplot(cancel_data, mapping = aes(x = Month_factor, y = cancel_rate, color = Airline)) + geom_point() +
  ggtitle('monthly cancel rata') + xlab('month in order') + ylab('cancel rate in precentage')

monthly_rate <- cancel_data %>% group_by(Month_factor) %>% summarize(Avg_rate = mean(cancel_rate))
monthly_rate

#plot by Airline
ggplot(cancel_data, mapping = aes(x = Airline, y = cancel_rate, color = Month_factor)) + geom_point() + ggtitle('Cancel by Airline') +
theme(axis.text.x = element_text(angle = 90))


#remove canceloutliner, month Jan'2022, Feb'2022 and Dec'2022
cancel_new <- cancel_data %>% filter(Month_factor != c(1,2,12))

#calculate best and worst airline
Avg_by_Airline <- cancel_new %>% group_by(Airline) %>% summarize(avg = mean(cancel_rate))
Avg_by_Airline$avg <- scales::percent(Avg_by_Airline$avg)
