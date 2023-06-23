setwd('~/Dsc520')
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
source_Url <- "http://content.bellevue.edu/cst/dsc/520/id/resources/10-week-housing-data/week-6-housing.xlsx"
download.file(url=source_Url, destfile = 'data/exercisedata.xlsx', method='curl')
housing <- read_excel('data/exercisedata.xlsx')
#head(housing)
#str(housing)
Sale_price = select(housing, `Sale Price`)
Total_Sale_Price <- apply(Sale_price, 2, sum)

Zipcode_SalePrice_mean <- aggregate(`Sale Price` ~ zip5, data = housing, mean)
#Zipcode_SalePrice_mean

housing_by_zip <- group_by(housing, zip5)
Count_by_Sales <- summarize(housing_by_zip, Count_of_Sales = n())
Housing_by_zip_bedroom <- group_by(housing, zip5, bedrooms)
Count_by_sales_room <- summarize(Housing_by_zip_bedroom, count = n())
Count_by_sales_room_avg <-summarize(Housing_by_zip_bedroom,
                                    avg_price = mean(`Sale Price`)) 

ggplot(housing, aes(x=`Sale Price`)) +geom_histogram() +facet_wrap(~bedrooms)+
ggtitle("Sales Historgram per beedroom") + xlab("Bedrooms") +ylab("Sale Price")
ggplot(housing, aes(x=bedrooms, y=`Sale Price`)) + geom_point() 
ggplot(housing, aes(x=`Sale Price`)) +geom_histogram()