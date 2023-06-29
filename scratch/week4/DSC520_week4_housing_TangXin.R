setwd('~/Dsc520')
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(scales)
source_Url <- "http://content.bellevue.edu/cst/dsc/520/id/resources/10-week-housing-data/week-6-housing.xlsx"
download.file(url=source_Url, destfile = 'data/exercisedata.xlsx', method='curl')
housing <- read_excel('data/exercisedata.xlsx')
#head(housing)
#str(housing)
Sale_price = select(housing, `Sale Price`)
Total_Sale_Price <- apply(Sale_price, 2, sum)

Zipcode_SalePrice_mean <- aggregate(`Sale Price` ~ zip5, data = housing, mean)
Zipcode_SalePrice_mean

housing_by_zip <- housing %>% group_by(zip5)
View(housing_by_zip)
Count_by_Sales <- summarize(housing_by_zip, Count_of_Sales = n())
View(Count_by_Sales)
Housing_by_zip_bedroom <- group_by(housing, zip5, bedrooms)
View(Housing_by_zip_bedroom)
Count_by_sales_room <- summarize(Housing_by_zip_bedroom, count = n())
View(Count_by_sales_room)
Count_by_sales_room_avg <-summarize(Housing_by_zip_bedroom,
                                    avg_price = mean(`Sale Price`)) 
View(Count_by_sales_room_avg)

options(scipen = 999)
ggplot(housing, aes(x=`Sale Price`)) +geom_histogram() +facet_wrap(~bedrooms)+
ggtitle("Sales Historgram per beedroom") + xlab("Bedrooms") +ylab("Sale Price")
ggplot(housing, aes(x=bedrooms, y=`Sale Price`)) + geom_point() + scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))
ggplot(housing, aes(x=`Sale Price`)) +geom_histogram() + scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))