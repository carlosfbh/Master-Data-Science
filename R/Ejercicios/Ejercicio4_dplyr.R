### Exercises using dplyr. Data Frame used ---> flights/2008.csv

library(dplyr)

flights <- readr::read_csv('data/flights/2008.csv')

##########################################################################
# 1. Exercise: 
# Mutate the data frame so that it includes a new variable that contains the average speed, 
#  avg_speed traveled by the plane for each flight (in mph). 
# Hint: Average speed can be calculated as distance divided by number of hours of travel,
#  and note that AirTime is given in minutes
##########################################################################

foo <- flights %>% 
  mutate(avg_speed = Distance * 60 / AirTime) %>% 
  select(Distance, AirTime, avg_speed)
rm(foo)


##########################################################################
# 2. Exercise: 
# How many weekend flights to JFK airport flew a distance of more than 1000 miles 
#  but had a total taxiing time below 15 minutes?
# 1) Select the flights that had JFK as their destination and assign the result to jfk
# 2) Combine the Year, Month and DayofMonth variables to create a Date column
# 3) Result
# 4) Delete jfk object to free resources 
##########################################################################

jfk <- flights %>% 
  filter(Dest == "JFK") %>% 
  mutate(Date = as.Date(paste(Year, Month, DayofMonth, sep = "-"))) %>% 
  filter(Distance > 1000, (TaxiIn + TaxiOut) < 15, DayOfWeek %in% c(6,7))
rm(jfk)


##########################################################################
# 3. Exercise: 
# Print the maximum taxiing difference of taxi with summarise() 
# Filter flights to keep all American Airline flights: aa
# Print out a summary of aa with the following variables:
#  n_flights: the total number of flights,
#  n_canc: the total number of cancelled flights,
#  p_canc: the percentage of cancelled flights,
#  avg_delay: the average arrival delay of flights whose delay is not NA. 
##########################################################################

flights %>% 
  filter(!is.na(TaxiIn), !is.na(TaxiOut)) %>% 
  summarise(max_taxi = max(abs(TaxiIn - TaxiOut)))

aa <- flights %>% 
  filter(UniqueCarrier == "AA")

aa %>% 
  summarise(
    n_flights = n(),
    n_canc = sum(Cancelled),
    p_canc = mean(Cancelled),
    avg_delay = mean(ArrDelay, na.rm = TRUE)
  )


##########################################################################
# 4. Exercise: 
# Print out a summary of aa with the following variables:
# n_security: the total number of cancelled flights by security reasons,
# CancellationCode: reason for cancellation (A = carrier, B = weather, C = NAS, 
# D = security) 
##########################################################################

aa %>% 
  summarise(
    n_carrier = sum(CancellationCode == "A", na.rm = TRUE),
    n_weather = sum(CancellationCode == "B", na.rm = TRUE),
    n_NAS = sum(CancellationCode == "C", na.rm = TRUE),
    n_security = sum(CancellationCode == "D", na.rm = TRUE)
  )
rm(aa)

##########################################################################
# 5. Exercise: 
# Use summarise() to create a summary of flight with a single variable, n, 
# that counts the number of overnight flights. These flights have an arrival 
# time that is earlier than their departure time. Only include flights that have 
# no NA values for both DepTime and ArrTime in your count.
##########################################################################

flights %>% 
  filter(ArrTime < DepTime, !is.na(ArrTime), !is.na(DepTime)) %>% 
  select(DepTime, AirTime, ArrTime) %>% 
  mutate(
    hours = as.integer(AirTime / 60),
    minutes = AirTime - 60 * hours,
    DepTime_h = as.integer(DepTime / 100),
    DepTime_m = DepTime - DepTime_h * 100,
    ArrTime_m = DepTime_m + minutes,
    ArrTime_h = DepTime_h + hours + as.integer(ArrTime_m / 60)
    ) %>% 
  filter(ArrTime_h > 23) %>% 
  summarise(n = n())


##########################################################################
# 6. Exercise: 
# In a similar fashion, keep flights that are delayed (ArrDelay > 0 and not NA). 
# Next, create a by-carrier summary with a single variable: avg, the average delay 
# of the delayed flights. Again add a new variable rank to the summary according to 
# avg. Finally, arrange by this rank variable
##########################################################################

flights %>% 
  filter(!is.na(ArrDelay)) %>% 
  group_by(UniqueCarrier) %>% 
  summarise(avg = mean(ArrDelay > 0)) %>% 
  mutate(rank = rank(avg)) %>% 
  arrange(rank)


##########################################################################
# 7. Exercise: 
# Do the same as in the Exercise 6, but now we want to know the average delay 
# per carrier and per time: morning (00:00 to 12:00) and afternoon (12:00 - 23:59)
##########################################################################

a <- flights %>% 
  filter(!is.na(DepDelay)) %>% 
  mutate(blocks = cut(DepTime, breaks = 2)) %>% 
  group_by(blocks, UniqueCarrier) %>% 
  summarise(avg = mean(DepDelay > 0)) %>% 
  mutate(rank = rank(avg)) %>% 
  arrange(rank)

rm(a)

##########################################################################
# 8. Exercise: 
# How many airplanes only flew to one destination from JFK? 
# The result contains only a single column named nplanes and a single row.
##########################################################################

a <- flights %>% 
  filter(Origin == "JFK") %>% 
  group_by(TailNum) %>% 
  summarise(ndest = n_distinct(Dest)) %>% 
  summarise(nplanes = sum(ndest == 1))


##########################################################################
# 9. Find the most visited destination for each carrier
# Your solution should contain four columns:
# UniqueCarrier and Dest, n, how often a carrier visited a particular destination,
# rank, how each destination ranks per carrier. rank should be 1 for every row, 
# as you want to find the most visited destination for each carrier.
##########################################################################

a <- flights %>% 
  group_by(UniqueCarrier, Dest) %>% 
  summarise(total = n()) %>% 
  mutate(rank = rank(desc(total))) %>% 
  filter(rank == 1)
