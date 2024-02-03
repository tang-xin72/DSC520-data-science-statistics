setwd('~/Dsc520')
library(readr)
library(readxl)
library(writexl)
library(dplyr)
library(utils)
library(purrr)
library(ggplot2)
library(lm.beta)
library(tidyverse)
library(coefplot)
library(car)
library(zoom)
library(gapminder)
library(scales)

source_Url <- "http://content.bellevue.edu/cst/dsc/520/id/resources/10-week-housing-data/week-6-housing.xlsx"
download.file(url=source_Url, destfile = 'data/datawk8.xlsx', method='curl')
datawk8 <- read_excel('data/datawk8.xlsx')

#data transformation
housing <- datawk8 %>% select(`Sale Price`,zip5, postalctyn, bedrooms, square_feet_total_living, year_built, sq_ft_lot)

#visualize data, looks like zip 98059 only have one sale, so I plan to remove it
ggplot(data = housing) + geom_point(mapping = aes(x = zip5, y = `Sale Price`, alpha = bedrooms))

#remove sale in zip = 98059

housing <- housing %>% filter(zip5 != 98059)

#count sales in each zipcode
Sales_count <- housing %>% summarize(`Sale Pice` =n())
sales_count_by_zip <- housing %>% group_by(zip5) %>% summarize(`Sale Price` =n())

#ceate datset at zip=98053, where most sales happened

#sales_in_98053 <- housing %>% filter(zip5 == 98053)

#simple linear regression, first create dataset, then do regression
simple_data <- housing %>% select(`Sale Price`, sq_ft_lot)
simple_lm <- lm(formula = `Sale Price` ~ sq_ft_lot, data = simple_data)
summary(simple_lm)

#prediction
sale_predict_df <- data.frame(sale = predict(simple_lm, newdata = simple_data), sq_ft_lot = simple_data$sq_ft_lot)
#View(sale_predict_df)
#write_xlsx(sale_predict_df,'data/predict.xlsx')

#calculate R and R^2
mean_sale <- mean(simple_data$`Sale Price`)
## Corrected Sum of Squares Total
sst <- sum((mean_sale - simple_data$`Sale Price`)^2)
## Corrected Sum of Squares for Model
ssm <- sum((mean_sale - sale_predict_df$sale)^2)

## Residuals
residuals <- simple_data$`Sale Price` - sale_predict_df$sale
## Sum of Squares for Error
sse <- sum(residuals^2)
## R Squared R^2 = SSM\SST
r_squared <- ssm/sst

# n and p
n <- nrow(simple_data)
p <- 2

adjusted_r_squared <- 1 - (1 - r_squared)*(n - 1) / (n - p)

#
#end of simple regression
#

##begin of multiple regression
multiple_data <- housing %>% select(`Sale Price`, zip5, square_feet_total_living, bedrooms, year_built)
multi <- multiple_data %>% mutate(price_per_ft = `Sale Price` / square_feet_total_living)
colnames(multi)[1] <- "SP"
colnames(multi)[3] <- "live_size"

#visual the data from price per foot angle, look like zip=98074 may also an outliner
ggplot(multi, aes(x = price_per_ft, fill = zip5)) +
  geom_histogram(binwidth = 10) + labs(x = 'price per foot') +
  facet_wrap(~zip5)

#visualize the data from price per foot and living area size. 
ggplot(multi, aes(x=log(live_size), y=(price_per_ft)))+geom_point()
ggplot(multi, aes(x=log(live_size), y=log(price_per_ft)))+geom_point()

multi1 <- lm(SP ~ scale(live_size) + zip5 + bedrooms + year_built, data = multi)
summary(multi1)

#make prediction
m_predict1_df <- data.frame(sale = predict(multi1, newdata = multi), zip = multi$zip5, live_size = multi$live_size,
                           bed = multi$bedrooms, year = multi$year_built)

View(m_predict1_df)

#calculate R^2 and adjusted R^2
mean_sale2 <- mean(multi$SP)

sst <- sum((mean_sale2 - multi$SP)^2)

ssm <- sum((mean_sale2 - m_predict1_df$sale)^2)

residuals <- multi$SP - m_predict1_df$sale

sse <- sum(residuals^2)

r_squared <- ssm/sst

## Number of observations
n <- nrow(multi)
## Number of regression paramaters
p <- 5

adjusted_r_squared <- 1 - (1 - r_squared)*(n - 1) / (n - p)

#find standardized bata
lm.beta(multi1) 

#find confidence interval
coefficients(multi1)
coefplot(multi1)
#zm()

#outliner and store it in a variable
options(scipen = 999)
ggplot(multi) +
  aes(x = SP) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

outliner <- multi %>% filter(SP > 200000)
influencer <- multi %>% filter(SP > 500000 & SP < 1000000)


#calculate standardized residual and plot it. then store outline (>2) in a variable and find out sum of large residuals
multi$SD_residual <- rstandard(multi1)
plot(fitted(multi1), multi$SD_residual)
#multi$out = ifelse(abs(standard_res_multi1)> 2, 1, 0)
multi$out = ifelse(abs(multi$SD_residual)> 2, 1, 0)
plot(multi$SD_residual, col = multi$out+1, pch=16,ylim=c(-5,10))
ggplot(data = multi, aes(x = multi$SD_residual)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

#large_SD_multi <- multi$SD_residual
sum_large_SD_multi <- multi %>% filter(out == 1) %>% summarize(sum(SD_residual))

#residual from simple regression model
simple_data$SD_res <- rstandard(simple_lm)
plot(fitted(simple_lm), simple_data$SD_res)
simple_data$out = ifelse(abs(simple_data$SD_res)> 2, 1, 0)
plot(simple_data$SD_res, col = simple_data$out+1, pch=16,ylim=c(-4,10))
ggplot(data = simple_data, aes(x = simple_data$SD_res)) +
  geom_histogram(fill = 'yellow', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

#large_SD_simple <- simple_data$SD_res
sum_large_SD_simple <- simple_data %>% filter(out == 1) %>% summarize(sum(SD_res))
View(sum_large_SD_simple)

#calculate cooks distance
cooks.distance(simple_lm)
cooks.distance(simple_lm)[which.max(cooks.distance(simple_lm))]

cooks.distance(multi1)
cooks.distance(multi1)[which.max(cooks.distance(multi1))]

#calculate leverage
hats_simple <- as.data.frame(hatvalues(simple_lm))
which(hatvalues(simple_lm)>0.05)


hats_multi <- as.data.frame(hatvalues(multi1))
which(hatvalues(multi1)>0.014)

#calculate Covariance ratio
cov_simple <- cov(housing$`Sale Price`, housing$sq_ft_lot)

multi_new <- multi %>% select(1:5)
Cov_multi <- cov(multi_new)

#calculate assumption of independence
cor(multi_new)

#calculate assumption of no multicollinearity
vif(multi1)

