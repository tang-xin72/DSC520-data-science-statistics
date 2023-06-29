x <- list(a = 1:5, b = 5:1)
View(x)
library(dplyr)
library(purrr)

rep(10, 10)  %>% 
  map(sample, 5)  %>% 
  keep(function(x) mean(x) > 6)

rep(1:4, 2)
rep(1:4, each = 2) 
rep(1:4, c(2,1,2,1))

A <- c(10,20,30,5,4)
B <- A %>% map(sample, 2) %>% keep(function(x) mean(x)>6)
B