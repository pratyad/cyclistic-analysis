install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("chron")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(chron)
# import 12  csv files from May 2020 till April 2021

May2020 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_2.csv")
June2020 <-read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_3.csv")
July2020<-read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_4.csv")
August2020 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_5.csv")
Sept2020 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_6.csv")
Oct2020 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_7.csv")
Nov2020 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_8.csv")
Dec2020 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_9.csv")
Jan2021 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_10.csv")
Feb2021 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_11.csv")
Mar2021 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_12.csv")
Apr2021 <- read.csv("C:\\Users\\Pratibha\\Desktop\\Data Analyst Portfolio\\Bicycle Data\\Trip_data_13.csv")

# By checking structure of all the files it is clear that start_station_id and end_station_id coloumns were
#integer datatype for 2020 except for december month and Char data type for 2021
# We will have to change the data type of four files from 2021 to integer

Dec2020 <- Dec2020 %>% mutate(start_station_id = as.integer(start_station_id), 
                              end_station_id = as.integer(end_station_id))

Jan2021 <- Jan2021 %>% mutate(start_station_id = as.integer(start_station_id), 
                              end_station_id = as.integer(end_station_id))
Feb2021 <- Feb2021 %>% mutate(start_station_id = as.integer(start_station_id), 
                              end_station_id = as.integer(end_station_id))
Mar2021 <- Mar2021 %>% mutate(start_station_id = as.integer(start_station_id), 
                              end_station_id = as.integer(end_station_id))
Apr2021 <- Apr2021 %>% mutate(start_station_id = as.integer(start_station_id), 
                              end_station_id = as.integer(end_station_id))


cycle_trips<-bind_rows(May2020,June2020,July2020,August2020,Sept2020,Oct2020,
                       Nov2020,Dec2020,Jan2021, Feb2021, Mar2021,Apr2021)



#Remove NA from the data set cycle_trips

na_vecs <- which(!complete.cases(cycle_trips))
cycle_trips <- cycle_trips[-na_vecs]

# Remove duplicates
# find duplicate entries. It can be done using ride_id

cycle_trips %>% group_by(ride_id) %>%
  filter(length(ride_id)>1)%>%
  summarise(num_duplicates = length(ride_id))

head(cycle_trips)

# there are 789 duplicate values
# Remove the duplicates 

cycle_trips_final <- cycle_trips[!duplicated(cycle_trips$ride_id),]

head(cycle_trips_final)

str(cycle_trips_final)

summary(cycle_trips_final)

nrow(cycle_trips_final)

# Data Wrangling

head(cycle_trips_final)

# separating started_time column so that day, month, year and time can be in separate column

cycle_trips_final$date <- as.Date(cycle_trips_final$started_at,format = "%m/%d/%y") #default format does not work here
cycle_trips_final$month <- format(as.Date(cycle_trips_final$date),"%m")
cycle_trips_final$day <- format(as.Date(cycle_trips_final$date),"%d")
cycle_trips_final$year <- format(as.Date(cycle_trips_final$date),"%Y")
cycle_trips_final$day_of_week <- format(as.Date(cycle_trips_final$date),"%A")


#Converting char columns started_at/ended_at to date from char

cycle_trips_final$started_at <- mdy_hm(cycle_trips_final$started_at,tz=Sys.timezone())
cycle_trips_final$ended_at <- mdy_hm(cycle_trips_final$ended_at,tz=Sys.timezone())



#Adding calculated ride_length column (2)

cycle_trips_final$ride_length <- difftime(cycle_trips_final$ended_at,cycle_trips_final$started_at)



# Creating a column with month name corresponding to the month column 

cycle_trips_final$month_name <- months(cycle_trips_final$started_at)

head(cycle_trips_final)

# filtering out entries from the dataframe when bikes were taken out of docks or ride_length was negative:

cycle_trips_final <- cycle_trips_final[!(cycle_trips_final$start_station_name == "HQ QR" | cycle_trips_final$ride_length<0),]




# Analyze

# membertype_day is a tibble that will group member type and days of the week and give
# average ride length and number of rides 

membertype_day<- cycle_trips_final %>% group_by(member_casual, day_of_week) %>%
  summarise(number_rides = n(), average_ridelength = round(mean(ride_length), 2)) %>%
  arrange(member_casual, day_of_week)

view(membertype_day)


#plotting a column chart for Number_of_rides Vs member_casual

membertype_day %>% ggplot(aes(day_of_week, number_rides, fill = member_casual)) +
  geom_col(position = "dodge" , width = .5 , color = "black") +
  labs(x = "day_of_week", y = "number of rides")+
  scale_y_continuous(breaks =  c(100000,200000,300000,400000),
                                 labels = c("100k","200k","300k","400k"))+
  theme_grey() +
  ggtitle(" Number of rides based on user type on days of week")

#plotting a column chart for average_ridelength Vs day_of_week for both member and casual type users:  


membertype_day %>% ggplot(aes(day_of_week ,average_ridelength, fill = member_casual)) +
  geom_col(position = "dodge" , width = .5 , color = "black") +
  labs(x = "Weekdays", y = "Average Ride Length") +
  scale_y_continuous(breaks =  c(100000,200000,300000,400000),
                     labels = c("100k","200k","300k","400k"))+
  theme_grey() +
  ggtitle("  ")
  

# Mean of number of rides for both member and casual

averageridelength_usertype <- membertype_day %>% group_by(member_casual) %>% 
  summarise (mean_duration = mean(average_ridelength))

averagenumrides_usertype <- membertype_day %>% group_by(member_casual) %>% 
  summarise(mean_numofrides = mean(number_rides))

averagenumrides_usertype

# graphical representation of Mean of number of rides for both member and casual

averageridelength_usertype %>% ggplot( aes(x = member_casual ,y = mean_duration,  fill = member_casual)) +
  geom_col(color = "black",position = "dodge",
           show.legend = FALSE , width = .5) +
  geom_text(aes(label = round(mean_duration),2),
            position = position_stack(vjust = 0.5)) 

averagenumrides_usertype %>% ggplot( aes(x = member_casual ,y = mean_numofrides,  fill = member_casual)) +
  geom_col(color = "black",position = "dodge",
           show.legend = FALSE , width = .5)+
  scale_y_continuous(breaks =  c(100000,200000,300000,400000),
                     labels = c("100k","200k","300k","400k")) +
  geom_text(aes(label = round(mean_numofrides, 2)),
            position = position_stack(vjust = 0.5))


#Cycle Usage Based On Bike Type And Member Type

ride_type<- cycle_trips_final %>% group_by( member_casual, rideable_type,)%>% summarise(num_ride = n())


ride_type %>% ggplot(aes(x= rideable_type, y = num_ride, fill = member_casual, color = member_casual)) + 
  geom_bar(stat='identity', position = "dodge") + ggtitle("Cycle Usage Based On Bike Type And Member Type", 
                                                          subtitle = "From May 2020 - April 2021")+ theme_grey()+
  scale_y_continuous(breaks =  c(100000,500000,900000,1300000,1700000),labels = c("100k","500k","900k","1300k", "1700k"))
                                                                                                                      




# Average ride time based on month and usertype

ride_month_usertype <- cycle_trips_final %>% group_by(month_name, member_casual) %>%
  summarise(mean_ride_length= mean(ride_length)) 

ride_month_usertype

# visualization based on month and usertype

ride_month_usertype %>% ggplot(aes(x= month_name, y = mean_ride_length, fill = member_casual, color = member_casual)) + 
  geom_bar(stat='identity', position = "dodge") + ggtitle("Average Usage Time Based On Month And Member Type", 
                                                          subtitle = "From May 2020 - April 2021")+ theme_grey()+
  scale_y_continuous(breaks =  c(100000,200000,300000,400000,500000),labels = c("100k","200k","300k","400k", "500k"))



