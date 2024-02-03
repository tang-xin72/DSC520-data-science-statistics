install.packages("hflights")
install.packages("writexl")
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
setwd("~/dsc520")

#this is an analysis of flight between Austin, TX and silicon valley California 
#read data

AUS_flight <- read_excel('scratch/project/aus-flight.xlsx')
View(AUS_flight)

# get departure dataset from Austin

AUS_Dep <- AUS_flight %>% filter(Origin == 'AUS')
dim(AUS_Dep)
View(AUS_Dep)

# get Arrival dataset from Austin
AUS_Avl <- new_flight %>% filter(Dest == 'AUS')
View(AUS_Avl)

#calculate count of delay and delay percentage by departure and arrival

# number of departure delay
delay_by_dest <- group_by(AUS_Dep, Dest) %>% filter(ArrDelay > 0) %>% summarize(delay_count=n())
View(delay_by_dest)

# number of departure flight count
flight_by_dest <- group_by(AUS_Dep, Dest) %>% summarize(flight_count=n())
View(flight_by_dest)

# merge flight count and delay count into same data frame
df <- merge(x = delay_by_dest, y = flight_by_dest, by = "Dest")
View(df)

# calculate the percentage of delay and format result to only 2 decimal digits
df_by_dest = mutate(df, delay_percent =delay_count / flight_count * 100)
print(sapply(df_by_dest, class))

df_by_dest$delay_percent<-round(df_by_dest$delay_percent,2)
View(df_by_dest)

#plot the flight delay by destination airport
ggplot(delay_by_dest, aes(x=Dest, y=delay_count)) + geom_bar(stat = "identity")


# calculate the arrival delay count and delay percentage

delay_by_origin <- group_by(AUS_Avl, Origin) %>% filter(ArrDelay > 0)%>% summarize(delay_count=n())

flight_by_origin <- group_by(AUS_Avl, Origin) %>% summarize(flight_count=n())

df <- merge(x = delay_by_origin, y = flight_by_origin, by = "Origin")
df_by_origin = mutate(df, delay_percent =delay_count / flight_count * 100)

df_by_origin$delay_percent<-round(df_by_origin$delay_percent,2)
View(df_by_origin)

#From study above, OAK airport always have most delay no matter as destination or origin. so the study below will remove exclude Oakland airport.
# The other 2 airport SFO and SJC have comparable flight count and delay performance, so focus these 2 airports.
#create dataset without OAK 

AUS_CA <- AUS_Dep %>% filter(Dest != 'OAK')
CA_AUS <- AUS_Avl %>% filter(Origin != 'OAK')

#Calculate True delay of flight (Arrival delay plus Departure delay)
AUS_CA <- mutate(AUS_CA, true_delay = ArrDelay + DepDelay)
CA_AUS <- mutate(CA_AUS, true_delay = ArrDelay + DepDelay)
ggplot(data = CA_AUS) +
  geom_point(mapping= aes(x = DayOfWeek, y = true_delay, color = Operating_Airline))

ggplot(data = CA_AUS, mapping= aes(x = Operating_Airline, y = true_delay)) +
  geom_boxplot()


#Below study will focus on airlines. the Oakland airport will be excluded.

# Which airline had worst delay when fly to Austin from Bay area? exclude early arrival, 
# it turns out airline OO (skywest) has least delay and rest are about same.
options(dplyr.summarise.inform = FALSE)

Avl_inbound_delay_carrier <- CA_AUS %>%
  group_by(Operating_Airline, Origin) %>% 
  filter(true_delay > 0) %>% 
  summarize( avg_delay = mean (true_delay))

# Which airline had worst delay when fly out of Austin to Bay area? exclude early arrival, 
# it turns out airline UA has worst delay and rest are about same.            
  
Outbound_delay_carrier <- AUS_CA %>%
  group_by(Operating_Airline, Dest) %>% 
  filter(true_delay > 0) %>% 
  summarize( avg_delay = mean (true_delay))

#which delay factor caused worst inbound delay
Inbound_delay_factor <- CA_AUS %>%
  group_by(Origin) %>% 
  filter(true_delay > 0) %>% 
  summarize( carrier_delay = mean (CarrierDelay, na.rm = TRUE), weather_delay = mean(WeatherDelay, na.rm=TRUE), NAS_delay = mean(NASDelay, na.rm=TRUE), 
             Security_delay = mean(SecurityDelay, na.rm=TRUE), late_arriv_Delay = mean(LateAircraftDelay, na.rm =TRUE))

#which delay factor caused worst outbound delay
Outbound_delay_factor <- AUS_CA %>%
  group_by(Dest) %>% 
  filter(true_delay > 0) %>% 
  summarize( carrier_delay = mean (CarrierDelay, na.rm = TRUE), weather_delay = mean(WeatherDelay, na.rm=TRUE), NAS_delay = mean(NASDelay, na.rm=TRUE), 
             Security_delay = mean(SecurityDelay, na.rm=TRUE), late_arriv_Delay = mean(LateAircraftDelay, na.rm =TRUE))

#Create histogram of delay, inbound and outbound each, check if there are normal distribution
#get data set
outbound_delay <- AUS_CA %>% group_by(Dest) %>% select(FlightDate, Dest, true_delay) 
ggplot(outbound_delay, aes(x=true_delay)) + 
  geom_histogram(binwidth = 10) +
  xlim(-50, 500) +
  geom_density(alpha=.2, fill="#FF6666")
  
inbound_delay <- CA_AUS %>% group_by(Origin) %>% select(FlightDate, Dest, true_delay) 
ggplot(outbound_delay, aes(x = true_delay)) + 
  geom_histogram(binwidth = 10) +
  xlim(-50, 500) +
  geom_density(alpha=.2)

#select one airport SFO and check again, seems same result, not normal distributed.
SFO_inbound_delay <- CA_AUS %>% filter(Origin == 'SFO') %>% select(FlightDate, Origin, ActualElapsedTime, true_delay, CarrierDelay, WeatherDelay, NASDelay, LateAircraftDelay) 
ggplot(SFO_inbound_delay, aes(x = true_delay)) + 
  geom_histogram(binwidth = 10) +
  xlim(-50, 500) +
  geom_density(alpha=.2)

#test correlationn and covariance, the square error is zero so returned nothing
SFO_inbound_delay_clean <- SFO_inbound_delay%>% drop_na()
cor(SFO_inbound_delay[, c('ActualElapsedTime','CarrierDelay', 'NASDelay', 'LateAircraftDelay')], use="complete.obs")
cor(SFO_inbound_delay_clean$ActualElapsedTime, SFO_inbound_delay_clean$NASDelay, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(SFO_inbound_delay_clean$ActualElapsedTime, SFO_inbound_delay_clean$CarrierDelay, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(SFO_inbound_delay_clean$ActualElapsedTime, SFO_inbound_delay_clean$LateAircraftDelay, use = "everything", method = c("pearson", "kendall", "spearman"))


