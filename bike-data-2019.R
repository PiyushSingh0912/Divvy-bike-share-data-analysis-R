library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(ggplot2) #helps visualize data


#.................................................................................................



library(readr)


#DATA 2019 .......................................................

Divvy_trip_2019_Q1<- read_csv(".../Divvy_trip_2019_Q1.csv")
Divvy_trip_2019_Q2<- read_csv(".../Divvy_trip_2019_Q2.csv")
Divvy_trip_2019_Q3<- read_csv(".../Divvy_trip_2019_Q3.csv")
Divvy_trip_2019_Q4<- read_csv(".../Divvy_trip_2019_Q4.csv")

# changing the ride_id and rideable_type to character

Divvy_trip_2019_Q1 <- mutate(Divvy_trip_2019_Q1, ride_id = as.character(ride_id)
                             ,rideable_type = as.character(rideable_type))

Divvy_trip_2019_Q2 <- mutate(Divvy_trip_2019_Q2, ride_id = as.character(ride_id)
                             ,rideable_type = as.character(rideable_type))

Divvy_trip_2019_Q3 <- mutate(Divvy_trip_2019_Q3, ride_id = as.character(ride_id)
                             ,rideable_type = as.character(rideable_type))

Divvy_trip_2019_Q4 <- mutate(Divvy_trip_2019_Q4, ride_id = as.character(ride_id)
                             ,rideable_type = as.character(rideable_type))



Divvy_2019_data <- mutate(Divvy_2019_data, ride_id = as.character(ride_id)
                          ,rideable_type = as.character(rideable_type))



View(Divvy_trip_2019_Q1)
View(Divvy_trip_2019_Q2)
View(Divvy_trip_2019_Q3)
View(Divvy_trip_2019_Q4)
# removing member gender and birth year columns from 2019 Q2 ............

Divvy_trip_2019_Q2 <- Divvy_trip_2019_Q2 %>%
  select(-c("Member Gender", "05 - Member Details Member Birthday Year"))



# creating date,day of week , month columns :- 

Divvy_trip_2019_Q1$date <- format(as.Date(Divvy_trip_2019_Q1$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_trip_2019_Q1$month <- format(as.Date(Divvy_trip_2019_Q1$date), "%m")
Divvy_trip_2019_Q1$day <- format(as.Date(Divvy_trip_2019_Q1$date), "%d")
Divvy_trip_2019_Q1$year <- format(as.Date(Divvy_trip_2019_Q1$date), "%Y")
Divvy_trip_2019_Q1$day_of_week <- format(as.Date(Divvy_trip_2019_Q1$date), "%A")



Divvy_trip_2019_Q2$date <- format(as.Date(Divvy_trip_2019_Q2$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_trip_2019_Q2$month <- format(as.Date(Divvy_trip_2019_Q2$date), "%m")
Divvy_trip_2019_Q2$day <- format(as.Date(Divvy_trip_2019_Q2$date), "%d")
Divvy_trip_2019_Q2$year <- format(as.Date(Divvy_trip_2019_Q2$date), "%Y")
Divvy_trip_2019_Q2$day_of_week <- format(as.Date(Divvy_trip_2019_Q2$date), "%A")


Divvy_trip_2019_Q3$date <- format(as.Date(Divvy_trip_2019_Q3$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_trip_2019_Q3$month <- format(as.Date(Divvy_trip_2019_Q3$date), "%m")
Divvy_trip_2019_Q3$day <- format(as.Date(Divvy_trip_2019_Q3$date), "%d")
Divvy_trip_2019_Q3$year <- format(as.Date(Divvy_trip_2019_Q3$date), "%Y")
Divvy_trip_2019_Q3$day_of_week <- format(as.Date(Divvy_trip_2019_Q3$date), "%A")


Divvy_trip_2019_Q4$date <- format(as.Date(Divvy_trip_2019_Q4$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_trip_2019_Q4$month <- format(as.Date(Divvy_trip_2019_Q4$date), "%m")
Divvy_trip_2019_Q4$day <- format(as.Date(Divvy_trip_2019_Q4$date), "%d")
Divvy_trip_2019_Q4$year <- format(as.Date(Divvy_trip_2019_Q4$date), "%Y")
Divvy_trip_2019_Q4$day_of_week <- format(as.Date(Divvy_trip_2019_Q4$date), "%A")



# changing the names of rows subscriber & customer : - ................

Divvy_trip_2019_Q1 <- Divvy_trip_2019_Q1 %>%
  mutate(member_casual = recode(member_casual,"Subscriber" = "Member" , "Customer" = "Casual"))

Divvy_trip_2019_Q2 <- Divvy_trip_2019_Q2 %>%
  mutate(member_casual = recode(member_casual,"Subscriber" = "Member" , "Customer" = "Casual"))

Divvy_trip_2019_Q3 <- Divvy_trip_2019_Q3 %>%
  mutate(member_casual = recode(member_casual,"Subscriber" = "Member" , "Customer" = "Casual"))

Divvy_trip_2019_Q4 <- Divvy_trip_2019_Q4 %>%
  mutate(member_casual = recode(member_casual,"Subscriber" = "Member" , "Customer" = "Casual"))




write.csv(Divvy_trip_2019_Q1,"...//Divvy_trip_2019_Q1.csv", row.names = FALSE)
write.csv(Divvy_trip_2019_Q2,"...//Divvy_trip_2019_Q2.csv", row.names = FALSE)
write.csv(Divvy_trip_2019_Q3,"...//Divvy_trip_2019_Q3.csv", row.names = FALSE)
write.csv(Divvy_trip_2019_Q4,"...//Divvy_trip_2019_Q4.csv", row.names = FALSE)



# Merging all quarter files of 2019 into one year file :- 

Divvy_2019_data <- bind_rows(Divvy_trip_2019_Q1 , Divvy_trip_2019_Q2,Divvy_trip_2019_Q3,Divvy_trip_2019_Q4)
View(Divvy_2019_data)


Divvy_trip_2019_Q4 <- mutate(Divvy_trip_2019_Q4, month= as.character(month))

write.csv(Divvy_2019_data,"...//Divvy_2019_data.csv", row.names = FALSE)

Divvy_2019_data <- read_csv(".../Divvy_2019_data.csv")


# creating a custom theme for the plots :-

custom_theme = theme(plot.title=element_text(size=20),
                     axis.text.x=element_text(size=2), 
                     axis.text.y=element_text(size=12),
                     axis.title.x=element_text(size=18), 
                     axis.title.y=element_text(size=18),
                     strip.text.x=element_text(size=16), 
                     strip.text.y=element_text(size=16),
                     legend.title=element_text(size=18), 
                     legend.text=element_text(size=16))


# Let's visualize the number of rides by rider type
Divvy_2019_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, weekday) %>%
  ggplot(mapping=aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

,average_duration = mean(ride_length)

# Let's create a visualization for average duration
Divvy_2019_data%>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


Divvy_2019_data%>%
  group_by(member_casual,day_of_week) %>%
  summarise(number_of_rides = n(), average_duration=mean(ride_length)) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_point()


# Top 5 start stations for casual riders
Top_5_start_stations <- Divvy_2019_data %>%
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "Member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=5)

View(Top_5_start_stations)


# Top 10 start stations for casual riders
Top_10_start_stations <- Divvy_2019_data %>%
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "Member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=10)



View(Top_10_stations_casual_2019)


Top_10_stations_casual_2019 <- Top_10_start_stations



# visualization of top 10 station 2019 for casual riders :

Top_10_stations_casual_2019%>%
  ggplot(aes(x=start_station_name,y=number_of_rides,fill=member_casual)) +
  geom_col(position = "dodge")+
  coord_flip()+
  scale_fill_manual(values=c("Casual"= "brown"))




# Number of rides between members and casual riders for each day of week
riders_Member_Vs_Casual <- Divvy_2019_data %>% 
  mutate(weekday=wday(started_at,label=TRUE))%>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(weekday)


View(riders_Member_Vs_Casual)

# visualization of rides between meber &  casual riders for each day by week 

riders_Member_Vs_Casual%>%
  ggplot(aes(x=weekday, y=number_of_rides,fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("Casual"="red4","Member"="powderblue"))




# Number of riders for member and casual for each month :
monthly_data_member_casual_2019 <- Divvy_2019_data %>%
  mutate(month_name = month(started_at, label = TRUE))%>%
  group_by(member_casual, month_name) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(month_name)


View(monthly_data_member_casual_2019)

# visualization for monthly rides of members vs casuals
monthly_data_member_casual_2019%>%
  ggplot(aes(x=month_name,y=number_of_rides,fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("Casual"="brown3","Member"="turquoise2" ))+
  custom_theme




# average number of rides by hour (casual riders)

options(repr.plot.width = 16, repr.plot.height = 8)

Divvy_2019_data %>%
  filter(member_casual == 'Casual') %>%
  group_by(hour_of_day = hour(round_date(started_at, 'hour'))) %>% 
  group_by(hour_of_day, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(-number_of_rides) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill= member_casual,color="black")) +
  geom_bar(position = 'dodge', stat = 'identity', fill = "violetred3") + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  labs(x = "Time of the Day (h)", y = "Number of Rides",fill="Member/Casual",
       title = "Average Number of Rides by Hour: Casual Riders") + 
  custom_theme




#  number of rides between members and casual riders by day of week across the year¶

options(repr.plot.width = 80, repr.plot.height = 14)

Divvy_2019_data%>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(month, weekday, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual,color="black")) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~month) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Bike Usage between Members and Casual Riders by Day of Week across the Year", fill = 'Member/Casual') +
  theme(axis.text.x = element_text(angle = 90)) +
  custom_theme


