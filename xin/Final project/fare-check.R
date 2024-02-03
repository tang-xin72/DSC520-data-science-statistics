setwd('~/Dsc520')
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(lm.beta)

fare_data <- read_excel('./scratch/project/price/price_clean.xlsx')

View(fare_data)

#plot
ggplot(fare_data, mapping = aes(x = Quarter, y = Adjusted_Average, color = Airport_Code)) + geom_point() +
  ggtitle('Quarterly flight price per airport') + ylab('Average Ticket Price')

#average
Average_price  <-  fare_data %>% group_by(Airport_Code) %>% summarize(annual_average=mean(Adjusted_Average))
Average_price$annual_average <- round(Average_price$annual_average, digits=2)
Average_price

