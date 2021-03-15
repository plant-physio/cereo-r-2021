# Explore the bike data characteristics
# 
#
#Explore the bike data to look at the relationship between
#temperature and number of riders
#
# Step 1: Load the bike data and look at the metadata
#
#
install.packages("lifecycle")
library(lifecycle)
install.packages("broom",dependencies = T)
library(broom)
install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(ggplot2)
library(magrittr)

bike <- read.csv("data/daily_bike_data.csv", header = T)

head(bike)
str(bike)
sort(names(bike))

summary(bike$dteday)

# Time trend of ridership
ggplot(data = bike, ) +
  geom_line(aes(x = as.Date(dteday), y = cnt))


ggplot(data = bike)+
  geom_point(aes(x = atemp, y = cnt))


### Data cleaning ----
# dplyr verbs for data transformation
# select
# mutate
# filter
# transmute
# %>% 

bike %>% dplyr::select(dteday, season, weathersit, temp, cnt)


#One way of selecting spring records and just a few cols

spring_bike <- bike %>% dplyr::filter(season == "spring")



#Exercise to get winter data

winter_bike <- bike %>% dplyr::filter(season == "winter")%>%
  dplyr::select(atemp, cnt)


###Mutate and Transmute with Factors and Dates
summary(bike$weathersit)
unique(bike$weathersit)


#Can reference the data documentation
# 

bike2 <- bike %>% 
  dplyr::mutate(
    weather_fac = factor(weathersit, 
                         levels = c(1,2,3,4),
                         labels = c("Clear", "Cloudy", "Rainy", "Heavy Rain")
  ))
  

bike2 %>% dplyr::select(dteday, weathersit, weather_fac)


### Converting to and from dates
bike_dates <- bike %>% transmute(
  instant,
  date = dteday,
  date_num = as.numeric(dteday),
  date_char = as.character(dteday)
)


###Additional Filtering and Selecting
bike %>% select(dteday, cnt)
bike %>% select(dteday, cnt, temp) %>% select(-temp)

keep_vars <- c("dteday", "cnt", "temp")
keep_vars <- paste0("temp", 1:12)

bike %>% select(all_of(keep_vars))

###Filtering
bike %>% dplyr::filter(season =="spring")
#'!='

bike %>% 
  dplyr::filter(season != "spring") %>%
  dplyr::select(season) %>% 
  dinstinct()


bike %>% 
  dplyr::filter(
    season == "summer" | season == "winter")
  

seasons <- c("summer", "winter")


bike %>% filter(season %in% seasons)