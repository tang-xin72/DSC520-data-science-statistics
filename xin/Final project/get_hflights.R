install.packages("hflights")
install.packages("writexl")
library(readr)
library(readxl)
library(dplyr)
library("hflights")
#getwd()
#library("writexl")
#write_xlsx(hflights,"hflights.xlsx")

View(hflights)

new_flight <- read_excel('scratch/project/houston-flight-jan-2023.xlsx')
View(new_flight)

Houston_Dep <- new_flight %>% filter(Origin == 'IAH' | Origin == 'HOU')
View(Houston_Dep)

Hou_CA <- Houston_Dep %>% filter(DestState == 'CA')
View(Hou_CA)

Houston_Avl <- new_flight %>% filter(Dest == 'HOU' | Dest == 'HOU')
View(Houston_Avl)

CA_Hou <- Houston_Avl %>% filter(OriginState == 'CA')
View(CA_Hou)