library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(ggplot2) #helps visualize data

library(readr)


# DATA 2021 ..................................................


Divvy_Trips_2021_01 <- read_csv("...//202101-divvy-tripdata.csv")
View(Divvy_Trips_2021_01)


Divvy_Trips_2021_02 <- read_csv("...//202102-divvy-tripdata.csv")
View(Divvy_Trips_2021_02)


Divvy_Trips_2021_03 <- read_csv("...//202103-divvy-tripdata.csv")
View(Divvy_Trips_2021_03)


Divvy_Trips_2021_04 <- read_csv("...//202104-divvy-tripdata.csv")
View(Divvy_Trips_2021_04)

#.....................................................................
Divvy_2021_Q1_data<- read_csv("...//Divvy_2021_Q1_data.csv")
View(Divvy_2021_Q1_data)


# Removing the unnecessary columns :-  ............................................


Divvy_Trips_2021_01<- Divvy_Trips_2021_01 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


Divvy_Trips_2021_02<- Divvy_Trips_2021_02 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


Divvy_Trips_2021_03<- Divvy_Trips_2021_03 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


Divvy_Trips_2021_04 <- Divvy_Trips_2021_04 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


# changing station_id columns to double format ..........

Divvy_Trips_2021_01$start_station_id <- as.double(Divvy_Trips_2021_01$start_station_id)
Divvy_Trips_2021_01$end_station_id <- as.double(Divvy_Trips_2021_01$end_station_id)

Divvy_Trips_2021_02$start_station_id <- as.double(Divvy_Trips_2021_02$start_station_id)
Divvy_Trips_2021_02$end_station_id <- as.double(Divvy_Trips_2021_02$end_station_id)

Divvy_Trips_2021_03$start_station_id <- as.double(Divvy_Trips_2021_03$start_station_id)
Divvy_Trips_2021_03$end_station_id <- as.double(Divvy_Trips_2021_03$end_station_id)

Divvy_Trips_2021_04$start_station_id <- as.double(Divvy_Trips_2021_04$start_station_id)
Divvy_Trips_2021_04$end_station_id <- as.double(Divvy_Trips_2021_04$end_station_id)




# changing the format of started_at and ended_at columns ................

Divvy_Trips_2021_01$started_at <- dmy_hm(Divvy_Trips_2021_01$started_at)
Divvy_Trips_2021_01$ended_at <- dmy_hm(Divvy_Trips_2021_01$ended_at)
View(Divvy_Trips_2021_01)


# Creating the ride_length column 
Divvy_Trips_2021_01$ride_length <- difftime(Divvy_Trips_2021_01$ended_at,Divvy_Trips_2021_01$started_at)
Divvy_Trips_2021_02$ride_length <- difftime(Divvy_Trips_2021_02$ended_at,Divvy_Trips_2021_02$started_at)
Divvy_Trips_2021_03$ride_length <- difftime(Divvy_Trips_2021_03$ended_at,Divvy_Trips_2021_03$started_at)
Divvy_Trips_2021_04$ride_length <- difftime(Divvy_Trips_2021_04$ended_at,Divvy_Trips_2021_04$started_at)

# reordering the columns ........................

Divvy_Trips_2021_01 <- Divvy_Trips_2021_01[,c(1,2,3,4,10,5:9)]
Divvy_Trips_2021_02 <- Divvy_Trips_2021_02[,c(1,2,3,4,10,5:9)]
Divvy_Trips_2021_03 <- Divvy_Trips_2021_03[,c(1,2,3,4,10,5:9)]
Divvy_Trips_2021_04 <- Divvy_Trips_2021_04[,c(1,2,3,4,10,5:9)]



# adding date,day,month,year columns ..................................................

Divvy_Trips_2021_01$date <- format(as.Date(Divvy_Trips_2021_01$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_Trips_2021_01$month <- format(as.Date(Divvy_Trips_2021_01$date), "%m")
Divvy_Trips_2021_01$day <- format(as.Date(Divvy_Trips_2021_01$date), "%d")
Divvy_Trips_2021_01$year <- format(as.Date(Divvy_Trips_2021_01$date), "%Y")
Divvy_Trips_2021_01$day_of_week <- format(as.Date(Divvy_Trips_2021_01$date), "%A")


Divvy_Trips_2021_02$date <- format(as.Date(Divvy_Trips_2021_02$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_Trips_2021_02$month <- format(as.Date(Divvy_Trips_2021_02$date), "%m")
Divvy_Trips_2021_02$day <- format(as.Date(Divvy_Trips_2021_02$date), "%d")
Divvy_Trips_2021_02$year <- format(as.Date(Divvy_Trips_2021_02$date), "%Y")
Divvy_Trips_2021_02$day_of_week <- format(as.Date(Divvy_Trips_2021_02$date), "%A")



Divvy_Trips_2021_03$date <- format(as.Date(Divvy_Trips_2021_03$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_Trips_2021_03$month <- format(as.Date(Divvy_Trips_2021_03$date), "%m")
Divvy_Trips_2021_03$day <- format(as.Date(Divvy_Trips_2021_03$date), "%d")
Divvy_Trips_2021_03$year <- format(as.Date(Divvy_Trips_2021_03$date), "%Y")
Divvy_Trips_2021_03$day_of_week <- format(as.Date(Divvy_Trips_2021_03$date), "%A")



Divvy_Trips_2021_04$date <- format(as.Date(Divvy_Trips_2021_04$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_Trips_2021_04$month <- format(as.Date(Divvy_Trips_2021_04$date), "%m")
Divvy_Trips_2021_04$day <- format(as.Date(Divvy_Trips_2021_04$date), "%d")
Divvy_Trips_2021_04$year <- format(as.Date(Divvy_Trips_2021_04$date), "%Y")
Divvy_Trips_2021_04$day_of_week <- format(as.Date(Divvy_Trips_2021_04$date), "%A")



# merging all month files into one quarter file of 2021........................................
Divvy_2021_Q1_data <- bind_rows(Divvy_Trips_2021_01 , Divvy_Trips_2021_02,Divvy_Trips_2021_03,Divvy_Trips_2021_04)
View(Divvy_2021_Q1_data)




write.csv(Divvy_Trips_2021_01,"...//Divvy_Trips_2021_m1.csv", row.names = FALSE)
write.csv(Divvy_Trips_2021_02,"...//Divvy_Trips_2021_m2.csv", row.names = FALSE)
write.csv(Divvy_Trips_2021_03,"...//Divvy_Trips_2021_m3.csv", row.names = FALSE)
write.csv(Divvy_Trips_2021_04,"...//Divvy_Trips_2021_m4.csv", row.names = FALSE)


write.csv(Divvy_2021_Q1_data,"...//Divvy_2021_Q1_data.csv", row.names = FALSE)




# Let's visualize the number of rides by rider type
Divvy_2021_Q1_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(mapping=aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")



# Let's create a visualization for average duration
Divvy_2021_Q1_data%>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")






# Comparing general bike type preference between members and casual riders
all_trips_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')



Divvy_2021_Q1_data$day_of_week <- ordered(Divvy_2021_Q1_data$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Comparing number of docked_bike rides between members and casual riders for each day of week
docked_bike_data <- Divvy_2021_Q1_data%>%
  filter(rideable_type == 'docked_bike') %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)

View(docked_bike_data)



# Docked_bike rides of casual riders for each day of week
docked_bike_casual <- Divvy_2021_Q1_data %>% 
  filter(rideable_type == 'docked_bike', member_casual == 'casual') %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop')

View(docked_bike_casual)


custom_theme = theme(plot.title=element_text(size=20),
                     axis.text.x=element_text(size=10), 
                     axis.text.y=element_text(size=12),
                     axis.title.x=element_text(size=18), 
                     axis.title.y=element_text(size=18),
                     strip.text.x=element_text(size=16), 
                     strip.text.y=element_text(size=16),
                     legend.title=element_text(size=18), 
                     legend.text=element_text(size=16))



# Average Number of Rides by Month: Casual Riders

Divvy_2021_Q1_data%>% 
  group_by(month, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(member_casual == 'casual') %>%
  drop_na() %>%
  ggplot(aes(x = month, y = number_of_rides,fill=member_casual)) + 
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45)) + 
  labs(x = "Month", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Month: Casual Riders") +
  custom_theme





# average number of rides by hour (casual riders)
str(all_trips_v2)
all_trips_v2$started_at_hour <- as.POSIXct(all_trips_v2$started_at, "%Y-%m-%d %H:%M:%S")      
str(all_trips_v2)


options(repr.plot.width = 12, repr.plot.height = 8)

Divvy_2021_Q1_data %>%
  filter(member_casual == 'casual') %>%
  mutate(hour_of_day = hour(round_date(started_at, 'hour'))) %>% 
  group_by(hour_of_day, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(-number_of_rides) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity',color="black") + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  labs(x = "Time of the Day (h)", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Hour: Casual Riders") +
  custom_theme





# top 10 stations (members)

options(repr.plot.width = 10, repr.plot.height = 6)

ggplot(data = top_10_station_member) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "thistle") +
  labs(title = "Top 10 Used Stations by Members", y = "Number of Rides", x = "") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() + my_theme




# top 10 stations (casual riders)

ggplot(data = top_10_station_casual) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "lightsalmon") +
  labs(title = "Top 10 Used Stations by Casual Riders", x = "", y = "Number of Rides") + 
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() + my_theme


# top 10 start stations (members)

options(repr.plot.width = 10, repr.plot.height = 6)

Divvy_2021_Q1_data %>% 
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "casual") %>% 
  arrange(-number_of_rides) %>% 
  head(n=10) %>%
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_col(position = 'dodge',fill="darkblue" , color= "lightyellow" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Top 10 Start Stations for Members', x = '', y = "Number of Rides") +
  coord_flip() +
  theme_minimal() +
  my_theme







# top 10 start stations (casual riders)

options(repr.plot.width = 10, repr.plot.height = 6)

Divvy_2021_Q1_data%>% 
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=10) %>%
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_col(position = 'dodge', fill ="darkorange1",color="black") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Top 10 Start Stations for Casual Riders', x = '', y = "Number of Rides") +
  coord_flip() +
  theme_minimal() +
  custom_theme



# usage of different bikes by rider type

options(repr.plot.width = 12, repr.plot.height = 8)

Divvy_2021_Q1_data %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("casual"="gold",  "member"="navyblue"))+
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~rideable_type) +
  labs(fill = "Member/Casual", x = "", y = "Number of Rides", 
       title = "Usage of Different Bikes: Members vs. Casual Riders") + custom_theme



#  usage of different bikes by rider type (separated)


options(repr.plot.width = 14, repr.plot.height = 10)

Divvy_2021_Q1_data%>% 
  group_by(month, member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("casual"="darkred","member"="turquoise4" ))+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~rideable_type) +
  labs(x = "Month", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Month") +
  theme(axis.text.x = element_text(angle = 90)) + custom_theme



#  number of rides between members and casual riders by day of week across the year¶

options(repr.plot.width = 26, repr.plot.height = 10)

Divvy_2021_Q1_data %>% 
  mutate(weekday=wday(started_at, label = TRUE))%>%
  group_by(month, weekday, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("casual"= "tomato","member"="turquoise4"))+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~month) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Bike Usage between Members and Casual Riders by Day of Week across the Year", fill = 'Member/Casual') +
  theme(axis.text.x = element_text(angle = 90)) +
  custom_theme



View(Divvy_2021_Q1_data)

