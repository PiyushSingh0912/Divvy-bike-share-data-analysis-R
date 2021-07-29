library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(ggplot2) #helps visualize data



# DATA 2018 .............................................................

library(readr)
Divvy_Trips_2018_Q1 <- read_csv("...//Divvy_Trips_2018_Q1.csv")
View(Divvy_Trips_2018_Q1)

Divvy_Trips_2018_Q2 <- read_csv("...//Divvy_Trips_2018_Q2.csv")
View(Divvy_Trips_2018_Q2)

Divvy_Trips_2018_Q3 <- read_csv("...//Divvy_Trips_2018_Q3.csv")
View(Divvy_Trips_2018_Q3)

Divvy_Trips_2018_Q4 <- read_csv("...//Divvy_Trips_2018_Q4.csv")
View(Divvy_Trips_2018_Q4)



# renaming the columns ...............................................................
Divvy_Trips_2018_Q1 <- rename(Divvy_Trips_2018_Q1
                              ,ride_id = '01 - Rental Details Rental ID'
                              ,rideable_type = '01 - Rental Details Bike ID'
                              ,ride_length = '01 - Rental Details Duration In Seconds Uncapped'
                              ,started_at = '01 - Rental Details Local Start Time'
                              ,ended_at = '01 - Rental Details Local End Time'
                              ,start_station_name = '03 - Rental Start Station Name'
                              ,start_station_id = '03 - Rental Start Station ID' 
                              ,end_station_name = '02 - Rental End Station Name'
                              ,end_station_id = '02 - Rental End Station ID'
                              ,member_casual = 'User Type')



Divvy_Trips_2018_Q2 <- rename(Divvy_Trips_2018_Q2
                              ,ride_id = 'trip_id'
                              ,rideable_type = 'bikeid'
                              ,ride_length = 'tripduration'
                              ,started_at = 'start_time'
                              ,ended_at = 'end_time'
                              ,start_station_name = 'from_station_name'
                              ,start_station_id = 'from_station_id' 
                              ,end_station_name = 'to_station_name'
                              ,end_station_id = 'to_station_id'
                              ,member_casual = 'usertype')



Divvy_Trips_2018_Q3 <- rename(Divvy_Trips_2018_Q3
                              ,ride_id = '0'
                              ,rideable_type = 'bikeid'
                              ,ride_length = 'tripduration'
                              ,started_at = 'start_time'
                              ,ended_at = 'end_time'
                              ,start_station_name = 'from_station_name'
                              ,start_station_id = 'from_station_id' 
                              ,end_station_name = 'to_station_name'
                              ,end_station_id = 'to_station_id'
                              ,member_casual = 'usertype')


Divvy_Trips_2018_Q4 <- rename(Divvy_Trips_2018_Q4
                              ,ride_id = 'trip_id'
                              ,rideable_type = 'bikeid'
                              ,ride_length = 'tripduration'
                              ,started_at = 'start_time'
                              ,ended_at = 'end_time'
                              ,start_station_name = 'from_station_name'
                              ,start_station_id = 'from_station_id' 
                              ,end_station_name = 'to_station_name'
                              ,end_station_id = 'to_station_id'
                              ,member_casual = 'usertype')



# replacing entries of subscriber & customer with member & casual ...........................
Divvy_Trips_2018_Q1 <- Divvy_Trips_2018_Q1 %>%
  mutate(member_casual = recode(member_casual,"Subscriber" = "Member" , "Customer" = "Casual"))

Divvy_Trips_2018_Q2 <- Divvy_Trips_2018_Q2 %>%
  mutate(member_casual = recode(member_casual,"Subscriber" = "Member" , "Customer" = "Casual"))

Divvy_Trips_2018_Q3 <- Divvy_Trips_2018_Q3 %>%
  mutate(member_casual = recode(member_casual,"Subscriber" = "Member" , "Customer" = "Casual"))

Divvy_Trips_2018_Q4 <- Divvy_Trips_2018_Q4 %>%
  mutate(member_casual = recode(member_casual,"Subscriber" = "Member" , "Customer" = "Casual"))



# Removing the unnecessary columns : - 

Divvy_Trips_2018_Q1 <- Divvy_Trips_2018_Q1 %>%
  select(-c("Member Gender", "05 - Member Details Member Birthday Year"))

Divvy_Trips_2018_Q2 <- Divvy_Trips_2018_Q2 %>%
  select(-c("gender", "birthyear"))

Divvy_Trips_2018_Q3 <- Divvy_Trips_2018_Q3 %>%
  select(-c("gender", "birthyear"))

Divvy_Trips_2018_Q4 <- Divvy_Trips_2018_Q4%>%
  select(-c("gender", "birthyear"))




# changing the started_at and ended_at columns to datetime format ..........

Divvy_Trips_2018_Q1$started_at <- dmy_hm(Divvy_Trips_2018_Q1$started_at)
Divvy_Trips_2018_Q1$ended_at <- dmy_hm(Divvy_Trips_2018_Q1$ended_at)
View(Divvy_Trips_2018_Q1)


Divvy_Trips_2018_Q2$started_at <- dmy_hm(Divvy_Trips_2018_Q2$started_at)
Divvy_Trips_2018_Q2$ended_at <- dmy_hm(Divvy_Trips_2018_Q2$ended_at)
View(Divvy_Trips_2018_Q2)


Divvy_Trips_2018_Q3$started_at <- dmy_hm(Divvy_Trips_2018_Q3$started_at)
Divvy_Trips_2018_Q3$ended_at <- dmy_hm(Divvy_Trips_2018_Q3$ended_at)
View(Divvy_Trips_2018_Q3)


Divvy_Trips_2018_Q4$started_at <- dmy_hm(Divvy_Trips_2018_Q4$started_at)
Divvy_Trips_2018_Q4$ended_at <- dmy_hm(Divvy_Trips_2018_Q4$ended_at)
View(Divvy_Trips_2018_Q4)




Divvy_Trips_2018_Q1 <- mutate(Divvy_Trips_2018_Q1, ride_id = as.character(ride_id)
                              ,rideable_type = as.character(rideable_type))

Divvy_Trips_2018_Q2 <- mutate(Divvy_Trips_2018_Q2, ride_id = as.character(ride_id)
                              ,rideable_type = as.character(rideable_type))

Divvy_Trips_2018_Q3 <- mutate(Divvy_Trips_2018_Q3, ride_id = as.character(ride_id)
                              ,rideable_type = as.character(rideable_type))

Divvy_Trips_2018_Q4 <- mutate(Divvy_Trips_2018_Q4, ride_id = as.character(ride_id)
                              ,rideable_type = as.character(rideable_type))


Divvy_2018_data <- mutate(Divvy_2018_data, ride_id = as.character(ride_id)
                          ,rideable_type = as.character(rideable_type))


# adding date , day , month year columns .....................

Divvy_Trips_2018_Q1$date <- format(as.Date(Divvy_Trips_2018_Q1$started_at, "%Y-%m-%d"), "%d-%m-%Y") 
Divvy_Trips_2018_Q1$month <- format(as.Date(Divvy_Trips_2018_Q1$date,"%d-%m-%Y"), "%m")
Divvy_Trips_2018_Q1$day <- format(as.Date(Divvy_Trips_2018_Q1$date,"%d-%m-%Y"), "%d")
Divvy_Trips_2018_Q1$year <- format(as.Date(Divvy_Trips_2018_Q1$date,"%d-%m-%Y"), "%Y")
Divvy_Trips_2018_Q1$day_of_week <- format(as.Date(Divvy_Trips_2018_Q1$date,"%d-%m-%Y"), "%A")


Divvy_Trips_2018_Q2$date <- format(as.Date(Divvy_Trips_2018_Q2$started_at, "%Y-%m-%d"), "%d-%m-%Y") 
Divvy_Trips_2018_Q2$month <- format(as.Date(Divvy_Trips_2018_Q2$date,"%d-%m-%Y"), "%m")
Divvy_Trips_2018_Q2$day <- format(as.Date(Divvy_Trips_2018_Q2$date,"%d-%m-%Y"), "%d")
Divvy_Trips_2018_Q2$year <- format(as.Date(Divvy_Trips_2018_Q2$date,"%d-%m-%Y"), "%Y")
Divvy_Trips_2018_Q2$day_of_week <- format(as.Date(Divvy_Trips_2018_Q2$date,"%d-%m-%Y"), "%A")

Divvy_Trips_2018_Q3$date <- format(as.Date(Divvy_Trips_2018_Q3$started_at, "%Y-%m-%d"), "%d-%m-%Y") 
Divvy_Trips_2018_Q3$month <- format(as.Date(Divvy_Trips_2018_Q3$date,"%d-%m-%Y"), "%m")
Divvy_Trips_2018_Q3$day <- format(as.Date(Divvy_Trips_2018_Q3$date,"%d-%m-%Y"), "%d")
Divvy_Trips_2018_Q3$year <- format(as.Date(Divvy_Trips_2018_Q3$date,"%d-%m-%Y"), "%Y")
Divvy_Trips_2018_Q3$day_of_week <- format(as.Date(Divvy_Trips_2018_Q3$date,"%d-%m-%Y"), "%A")

Divvy_Trips_2018_Q4$date <- format(as.Date(Divvy_Trips_2018_Q4$started_at, "%Y-%m-%d"), "%d-%m-%Y") 
Divvy_Trips_2018_Q4$month <- format(as.Date(Divvy_Trips_2018_Q4$date,"%d-%m-%Y"), "%m")
Divvy_Trips_2018_Q4$day <- format(as.Date(Divvy_Trips_2018_Q4$date,"%d-%m-%Y"), "%d")
Divvy_Trips_2018_Q4$year <- format(as.Date(Divvy_Trips_2018_Q4$date,"%d-%m-%Y"), "%Y")
Divvy_Trips_2018_Q4$day_of_week <- format(as.Date(Divvy_Trips_2018_Q4$date,"%d-%m-%Y"), "%A")





library(dplyr)
# Merging all quarter files of 2018 into one year file ......................

Divvy_2018_data <- bind_rows(Divvy_Trips_2018_Q1 , Divvy_Trips_2018_Q2,Divvy_Trips_2018_Q3 , Divvy_Trips_2018_Q4)

View(Divvy_2018_data)

Divvy_2018_data <- read_csv("...//Divvy_2018_data.csv")


write.csv(Divvy_Trips_2018_Q1,"...//Divvy_Tripdata_2018_Q1.csv", row.names = FALSE)
write.csv(Divvy_Trips_2018_Q2,"...//Divvy_Tripdata_2018_Q2.csv", row.names = FALSE)
write.csv(Divvy_Trips_2018_Q3,"...//Divvy_Tripdata_2018_Q3.csv", row.names = FALSE)
write.csv(Divvy_Trips_2018_Q4,"...//Divvy_Tripdata_2018_Q4.csv", row.names = FALSE)


write.csv(Divvy_2018_data,"...//Divvy_2018_data.csv", row.names = FALSE)



#............Descriptive Analysis.............................................................



# Let's visualize the number of rides by rider type
Divvy_2018_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(mapping=aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")



# Let's create a visualization for average duration
Divvy_2018_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")



Divvy_2018_data %>%
  group_by(member_casual, day_of_week) %>%
  arrange(member_casual, day_of_week)%>%
  ggplot(aes(x = ride_id, y = ride_length , linetype = member_casual)) +
  geom_smooth()




# Compare members and casual users
aggregate(Divvy_2018_data$ride_length ~ Divvy_2018_data$member_casual, FUN = mean)
aggregate(Divvy_2018_data$ride_length ~ Divvy_2018_data$member_casual, FUN = median)
aggregate(Divvy_2018_data$ride_length ~ Divvy_2018_data$member_casual, FUN = max)
aggregate(Divvy_2018_data$ride_length ~ Divvy_2018_data$member_casual, FUN = min)



# See the average ride time by each day for members vs casual users
aggregate(Divvy_2018_data$ride_length ~ Divvy_2018_data$member_casual + Divvy_2018_data$day_of_week,
          FUN = mean)


# Notice that the days of the week are out of order. Let's fix that.
Divvy_2018_data$day_of_week <- ordered(Divvy_2018_data$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# Now, let's run the average ride time by each day for members vs casual users
aggregate(Divvy_2018_data$ride_length ~ Divvy_2018_data$member_casual + Divvy_2018_data$day_of_week,
          FUN = mean)



# analyze ridership data by type and weekday
Divvy_2018_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
  group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n() ,average_duration = mean(ride_length)) %>% # calculates the average duration
  arrange(member_casual, weekday) # sorts


# Let's visualize the number of rides by rider type
Divvy_2018_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
Divvy_2018_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")




# Top 5 start stations for casual riders
Top_5_start_stations <- Divvy_2018_data %>%
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "Member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=5)

View(Top_5_start_stations)


# Top 10 start stations for casual riders
Top_10_start_stations <- Divvy_2018_data %>%
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "Member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=10)



View(Top_10_start_stations)


Top_10_stations_casual <- Top_10_start_stations




# Number of rides between members and casual riders for each day of week
riders_Member_Vs_Casual <- Divvy_2018_data %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)


View(riders_Member_Vs_Casual)




# Put months in order
Divvy_2018_data$month <- factor(Divvy_2018_data$month , levels= month.abb)

# See the average ride length by month for members vs. casual riders
aggregate(Divvy_2018_data$ride_length ~ Divvy_2018_data$member_casual + Divvy_2018_data$month, FUN = mean)



# Number of riders for member and casual for each month :
Divvy_2018_data %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(month)




# Comparing general bike type preference between members and casual riders
all_trips_v2 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')





# Comparing number of docked_bike rides between members and casual riders for each day of week
all_trips_v2 %>% 
  filter(rideable_type == 'docked_bike') %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)



# Docked_bike rides of casual riders for each day of week
all_trips_v2 %>% 
  filter(rideable_type == 'docked_bike', member_casual == 'casual') %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop')



# creating a custom theme for the plots :-
custom_theme = theme(plot.title=element_text(size=20),
                 axis.text.x=element_text(size=10), 
                 axis.text.y=element_text(size=12),
                 axis.title.x=element_text(size=18), 
                 axis.title.y=element_text(size=18),
                 strip.text.x=element_text(size=16), 
                 strip.text.y=element_text(size=16),
                 legend.title=element_text(size=18), 
                 legend.text=element_text(size=16))



options(repr.plot.width = 16, repr.plot.height = 8)

Divvy_2018_data %>%
  filter(member_casual == 'Casual') %>%
  group_by(hour_of_day = hour(round_date(started_at_hour, 'hour'))) %>% 
  group_by(hour_of_day, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(-number_of_rides) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill= member_casual)) +
  geom_bar(position = 'dodge', stat = 'identity', fill = "violetred3") + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  labs(x = "Time of the Day (h)", y = "Number of Rides",fill="Member/Casual",
       title = "Average Number of Rides by Hour: Casual Riders") + 
  my_theme


scale_fill_manual("legend", values = c("A" = "black", "B" = "orange", "C" = "blue"))


# top 10 stations (members)

options(repr.plot.width = 10, repr.plot.height = 6)

ggplot(data = top_10_station_member) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "thistle") +
  labs(title = "Top 10 Used Stations by Members", y = "Number of Rides", x = "") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() + my_theme




# top 10 stations (casual riders)

ggplot(data = Top_10_stations_casual) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "orangered4") +
  labs(title = "Top 10 Used Stations by Casual Riders", x = "", y = "Number of Rides") + 
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() + my_theme


# top 10 start stations (casual riders)

options(repr.plot.width = 10, repr.plot.height = 6)

Divvy_2018_data %>% 
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=10) %>%
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_col(position = 'dodge', fill = "orange" ) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Top 10 Start Stations for Casual Riders', x = '', y = "Number of Rides") +
  coord_flip() +
  theme_minimal() +
  my_theme


#  number of rides between members and casual riders by day of week across the year¶

options(repr.plot.width = 26, repr.plot.height = 10)

Divvy_2018_data%>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(month, weekday, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~month) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Bike Usage between Members and Casual Riders by Day of Week across the Year", fill = 'Member/Casual') +
  theme(axis.text.x = element_text(angle = 90)) +
  my_theme






