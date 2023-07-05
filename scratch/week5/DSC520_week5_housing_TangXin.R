#setwd('~/Dsc520')
library(readr)
library(readxl)
library(dplyr)
library(utils)
library(purrr)
library(tidyverse)

source_Url <- "http://content.bellevue.edu/cst/dsc/520/id/resources/10-week-housing-data/week-6-housing.xlsx"
download.file(url=source_Url, destfile = 'data/exercisedata.xlsx', method='curl')
housing <- read_excel('data/exercisedata.xlsx')
#head(housing)
#str(housing)
#View(housing)

#dplyr package
#using summarise()
Total_sales <- housing %>% summarise(total_sales = sum(`Sale Price`))
Total_sales

#using groupby()
Total_sales_zip <- housing %>% group_by(zip5) %>% summarise(Total_sales =sum(`Sale Price`))
#View(Total_sales_zip)

#using select
Housing_sales_zip_bedroom_sqft <- housing %>% 
  select(`Sale Price`,zip5, bedrooms, square_feet_total_living)
#View(Housing_sales_zip_bedroom_sqft)

#using mutate()
new_housing_byK <- Housing_sales_zip_bedroom_sqft %>% 
  mutate(price_in_K = `Sale Price` / 1000)
#View(new_housing_byK)

#using filter()
housing_byK_expensive <- new_housing_byK %>% filter(price_in_K > 300)
#View(housing_byK_expensive)

#using arrange()
housing_sorted_98052 <- Housing_sales_zip_bedroom_sqft %>% 
  filter(zip5 == 98052) %>% 
  filter(bedrooms!= 0) %>% 
  arrange(desc(`Sale Price`))
View(housing_sorted_98052)

housing_sorted_98053 <- Housing_sales_zip_bedroom_sqft %>% 
  filter(zip5 == 98053) %>% 
  filter(bedrooms!= 0) %>% 
  arrange(desc(`Sale Price`))
View(housing_sorted_98053)

#Purrr package function
#using keep()
Top_sale <- Total_sales_zip %>% 
  map(sample, 3) %>% 
  keep(function(x) mean(x) > 1000000)
Top_sale


Top_sale2 <- Total_sales_zip %>% 
  map(sample, 3) %>% 
  discard(function(x) mean(x) < 1000000)
Top_sale2  

#create subset 
Bighouse_98053_4bed <- Housing_sales_zip_bedroom_sqft %>% 
  filter(zip5 == 98053) %>% 
  filter(bedrooms == 4 & square_feet_total_living > 7000) %>% 
  arrange(desc(`Sale Price`))
#View(Bighouse_98053_4bed)

Bighouse_98053_3bed <- Housing_sales_zip_bedroom_sqft %>% 
  filter(zip5 == 98053) %>% 
  filter(bedrooms == 3 & square_feet_total_living > 7000) %>% 
  arrange(desc(`Sale Price`))
#View(Bighouse_98053_3bed)

#using cbind(0)
Sale_price_98053_4bed <- Bighouse_98053_4bed[1]
Sqft_98053_4bed <- Bighouse_98053_4bed[4]
Bighouse_98053 <- cbind(Sale_price_98053_4bed, Sqft_98053_4bed)
#View(Bighouse_98053)

#using rbind()
Copy_98053 <- Bighouse_98053
#View(Copy_98053)
duplicate <- rbind(Bighouse_98053, Copy_98053)
#View(duplicate)

#split strings and concatenate it back
sentence <- "Four score and seven years ago our fathers brought forth on this continent"

c <- unlist(strsplit(sentence," "))
print(c)

new_string = paste(c, collapse = ' ')
print(new_string, quote = FALSE)
