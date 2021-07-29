# DATA 2020 TRIPS ..............................................................


Divvy_Trips_2020_Q1 <- read_csv(".../Divvy_Trips_2020_Q1.csv")
View(Divvy_Trips_2020_Q1)

Divvy_tripdata_2020_04 <- read_csv(".../202004-divvy-tripdata.csv")


Divvy_tripdata_2020_05 <- read_csv(".../202005-divvy-tripdata.csv")


Divvy_tripdata_2020_06 <- read_csv(".../202006-divvy-tripdata.csv")



Divvy_tripdata_2020_07<- read_csv(".../202007-divvy-tripdata.csv")
View(Divvy_tripdata_2020_07)


Divvy_tripdata_2020_08 <- read_csv(".../202008-divvy-tripdata.csv")

Divvy_tripdata_2020_09<- read_csv(".../202009-divvy-tripdata.csv")

Divvy_tripdata_2020_10<- read_csv(".../202010-divvy-tripdata.csv")
Divvy_tripdata_2020_11<- read_csv(".../202011-divvy-tripdata.csv")
Divvy_tripdata_2020_12<- read_csv(".../202012-divvy-tripdata.csv")

Divvy_tripdata_2020_10$started_at <- dmy_hm(Divvy_tripdata_2020_10$started_at)
Divvy_tripdata_2020_10$ended_at <- dmy_hm(Divvy_tripdata_2020_10$ended_at)
View(Divvy_tripdata_2020_10)



Divvy_tripdata_2020_Q1 <- read_csv(".../Divvy_tripdata_2020_Q1.csv")
Divvy_tripdata_2020_Q2 <- read_csv(".../Divvy_tripdata_2020_Q2.csv")
Divvy_tripdata_2020_Q3 <- read_csv(".../Divvy_tripdata_2020_Q3.csv")
Divvy_tripdata_2020_Q4 <- read_csv(".../Divvy_tripdata_2020_Q4.csv")


Divvy_data_2020 <- read_csv(".../Divvy_data_2020.csv")
Divvy_data_2019 <- read_csv(".../Divvy_data_2019.csv")

Divvy_2020_data <- read_csv(".../Divvy_2020_data_v2.csv")




# Changing the started_at & ended_at columns from character to datetime

Divvy_tripdata_2020_Q1$started_at <- dmy_hm(Divvy_tripdata_2020_Q1$started_at)
Divvy_tripdata_2020_Q1$ended_at <- dmy_hm(Divvy_tripdata_2020_Q1$ended_at)
View(Divvy_tripdata_2020_Q1)

Divvy_tripdata_2020_10$started_at <- dmy_hm(Divvy_tripdata_2020_10$started_at)
Divvy_tripdata_2020_10$ended_at <- dmy_hm(Divvy_tripdata_2020_10$ended_at)
View(Divvy_tripdata_2020_Q1)
View(Divvy_tripdata_2020_Q2)
View(Divvy_tripdata_2020_Q3)
View(Divvy_tripdata_2020_Q4)





# Changing the datatype of station id to double...............
Divvy_tripdata_2020_12$start_station_id <- as.double(Divvy_tripdata_2020_12$start_station_id)
Divvy_tripdata_2020_12$end_station_id <- as.double(Divvy_tripdata_2020_12$end_station_id)


View(Divvy_tripdata_2020_Q1)
View(Divvy_tripdata_2020_Q2)
View(Divvy_tripdata_2020_Q3)




Divvy_tripdata_2020_12 <- bind_rows(Divvy_tripdata_2020_04 , Divvy_tripdata_2020_05,Divvy_tripdata_2020_06)
View(Divvy_Trips_2020_Q2)




# Creating the ride_length column 
Divvy_tripdata_2020_12$ride_length <- difftime(Divvy_tripdata_2020_12$ended_at,Divvy_tripdata_2020_12$started_at)

Divvy_tripdata_2020_11$ride_length <- difftime(Divvy_tripdata_2020_11$ended_at,Divvy_tripdata_2020_11$started_at)

Divvy_tripdata_2020_10$ride_length <- difftime(Divvy_tripdata_2020_10$ended_at,Divvy_tripdata_2020_10$started_at)

Divvy_tripdata_2020_Q1$ride_length <- difftime(Divvy_tripdata_2020_Q1$ended_at,Divvy_tripdata_2020_Q1$started_at)
Divvy_tripdata_2020_Q2$ride_length <- difftime(Divvy_tripdata_2020_Q2$ended_at,Divvy_tripdata_2020_Q2$started_at)
Divvy_tripdata_2020_Q3$ride_length <- difftime(Divvy_tripdata_2020_Q3$ended_at,Divvy_tripdata_2020_Q3$started_at)



# reordering the column ride_length
Divvy_tripdata_2020_Q1 <- Divvy_tripdata_2020_Q1[,c(1,2,3,4,10,5:9)]
Divvy_tripdata_2020_Q2 <- Divvy_tripdata_2020_Q2[,c(1,2,3,4,10,5:9)]
Divvy_tripdata_2020_Q3 <- Divvy_tripdata_2020_Q3[,c(1,2,3,4,10,5:9)]



# Merging the tables for Q2 :- 
Divvy_Trips_2020_Q2 <- bind_rows(Divvy_tripdata_2020_04 , Divvy_tripdata_2020_05,Divvy_tripdata_2020_06)
View(Divvy_Trips_2020_Q2)


# Merging the tables for Q3:- 
Divvy_Trips_2020_Q3 <- bind_rows(Divvy_tripdata_2020_07 , Divvy_tripdata_2020_08,Divvy_tripdata_2020_09)
View(Divvy_Trips_2020_Q3)



# Merging the tables for Q4:- 
Divvy_tripdata_2020_Q4 <- bind_rows(Divvy_tripdata_2020_10 , Divvy_tripdata_2020_11,Divvy_tripdata_2020_12)
View(Divvy_tripdata_2020_Q4)

Divvy_tripdata_2020_Q4 <- Divvy_Trips_2020_Q4
View(Divvy_tripdata_2020_Q4)


#Merging the quarter files of 2020 into one year file ...................
Divvy_2020_data <- bind_rows(Divvy_tripdata_2020_Q1 , Divvy_tripdata_2020_Q2,Divvy_tripdata_2020_Q3,Divvy_tripdata_2020_Q4)
View(Divvy_2020_data)

Divvy_2020_data <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_2020_data.csv")






write.csv(Divvy_tripdata_2020_Q1,"...//Divvy_tripdata_2020_Q1.csv", row.names = FALSE)
write.csv(Divvy_tripdata_2020_Q2,"...//Divvy_tripdata_2020_Q2.csv", row.names = FALSE)
write.csv(Divvy_tripdata_2020_Q3,"...//Divvy_tripdata_2020_Q3.csv", row.names = FALSE)
write.csv(Divvy_tripdata_2020_Q4,"...//Divvy_tripdata_2020_Q4.csv", row.names = FALSE)




write.csv(Divvy_data_2020,"...//Divvy_data_2020.csv", row.names = FALSE)


colnames(Divvy_Trips_2020_Q1)[3] <- "started_at"
colnames(Divvy_Trips_2020_Q1)[4] <- "ended_at"

colnames(Divvy_Trips_2020_Q2)[3] <- "started_at"
colnames(Divvy_Trips_2020_Q2)[4] <- "ended_at"

colnames(Divvy_Trips_2020_Q3)[3] <- "started_at"
colnames(Divvy_Trips_2020_Q3)[4] <- "ended_at"

colnames(Divvy_Trips_2020_Q4)[3] <- "started_at"
colnames(Divvy_Trips_2020_Q4)[4] <- "ended_at"


# Removing the lat,lng,birthyear,gender columns 

Divvy_Trips_2020_Q1<- Divvy_Trips_2020_Q1 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_04<- Divvy_tripdata_2020_04 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_05<- Divvy_tripdata_2020_05 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_06<- Divvy_tripdata_2020_06 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_07<- Divvy_tripdata_2020_07 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_08<- Divvy_tripdata_2020_08 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_09<- Divvy_tripdata_2020_09 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_10<- Divvy_tripdata_2020_10 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_11<- Divvy_tripdata_2020_11 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))

Divvy_tripdata_2020_12<- Divvy_tripdata_2020_12 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))




Divvy_tripdata_2020_Q1$started_at <- dmy_hm(Divvy_tripdata_2020_Q1$started_at)
Divvy_tripdata_2020_Q1$ended_at <- dmy_hm(Divvy_tripdata_2020_Q1$ended_at)


Divvy_2020_data$date <- format(as.Date(Divvy_2020_data$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_2020_data$month <- format(as.Date(Divvy_2020_data$date), "%m")
Divvy_2020_data$day <- format(as.Date(Divvy_2020_data$date), "%d")
Divvy_2020_data$year <- format(as.Date(Divvy_2020_data$date), "%Y")
Divvy_2020_data$day_of_week <- format(as.Date(Divvy_2020_data$date), "%A")




Divvy_2020_data <- bind_rows( Divvy_tripdata_2020_Q1 ,  Divvy_tripdata_2020_Q2, Divvy_tripdata_2020_Q3 , Divvy_tripdata_2020_Q4)

View(Divvy_2020_data)



# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and
#checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed

Divvy_2020_data_v2 <- Divvy_2020_data[!(Divvy_2020_data$start_station_name == "HQ QR" | Divvy_2020_data$ride_length<0),]

View(Divvy_2020_data_v2)


write.csv(Divvy_2020_data_v2,"...\\Divvy_2020_data_v2.csv", row.names = FALSE)

write.csv(Divvy_2020_data,"...//Divvy_2020_data.csv", row.names = FALSE)

memory.limit(size = 12000)


library(dplyr)

summary_data_2019 <- summary(Divvy_data_2019) #Statistical summary of data. Mainly for numerics
View(summary_data_2019)

summary_data_2020 <- summary(Divvy_data_2020) #Statistical summary of data. Mainly for numerics
View(summary_data_2020)



write.csv(summary_data_2020,"...//summary_data_2020.csv", row.names = FALSE)



#............Descriptive Analysis.............................................................

aggregate(Divvy_2020_data$ride_length ~ Divvy_2020_data$member_casual, FUN = mean)
aggregate(Divvy_2020_data$ride_length ~ Divvy_2020_data$member_casual, FUN = median)
aggregate(Divvy_2020_data$ride_length ~ Divvy_2020_data$member_casual, FUN = max)
aggregate(Divvy_2020_data$ride_length ~ Divvy_2020_data$member_casual, FUN = min)





colnames(all_quart_data)[13] <- "date"

colnames(all_quart_data)[5] <- "Ride_length"

#the average ride time by each day for members vs casual users
mean_ride_length_by_Day <- aggregate(Divvy_2020_data$ride_length ~ Divvy_2020_data$member_casual + Divvy_2020_data$day_of_week, FUN = mean)  

View(Divvy_2020_data)

write.csv(mean_ride_length_by_Day,"C:\\Users\\piyus\\Desktop\\bike-share PROJECT\\mean_ride_length_by_Day.csv", row.names = FALSE)


#correcting the order of days in week
Divvy_2020_data$day_of_week <- ordered(Divvy_2020_data$day_of_week, levels=c("Sunday" ,"Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# analyze ridership data by type and weekday
Divvy_2020_type_weekday <- Divvy_2020_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n() , average_duration = mean(ride_length)) %>%
  arrange(member_casual,weekday) # sorts 


write.csv(Divvy_2020_type_weekday,"...\\Divvy_2020_type_weekday.csv", row.names = FALSE)

View(Divvy_2020_type_weekday)

data_by_week <- all_quart_data %>%
  mutate(weekday = wday(Start_Time, label = TRUE)) %>% 
  group_by(User_Type, weekday) %>%
  summarise(number_of_rides = n() , average_duration = mean(Ride_length)) %>%
  arrange(User_Type,weekday) # sorts 





# Let's visualize the number of rides by rider type
Divvy_2020_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(mapping=aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")



# Let's create a visualization for average duration
Divvy_2020_data%>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

counts <- aggregate(Divvy_2020_data$ride_length ~ Divvy_2020_data$member_casual +
                      Divvy_2020_data$day_of_week, FUN = mean)
View(counts)









# Comparing general bike type preference between members and casual riders
Divvy_2020_data %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')





# Comparing number of docked_bike rides between members and casual riders for each day of week
Divvy_2020_data%>% 
  filter(rideable_type == 'docked_bike') %>% 
  mutate(weekday=wday(started_at,label=TRUE))%>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(weekday)



# Docked_bike rides of casual riders for each day of week
Divvy_2020_data %>% 
  filter(rideable_type == 'docked_bike', member_casual == 'casual') %>% 
  mutate(weekday=wday(started_at,label=TRUE)) %>%
  group_by(weekday) %>% 
  summarise(number_of_rides = n(), .groups = 'drop')



# creating a custom theme for the plots :- 

custom_theme = theme(plot.title=element_text(size=20),
                     axis.text.x=element_text(size=9), 
                     axis.text.y=element_text(size=16),
                     axis.title.x=element_text(size=18), 
                     axis.title.y=element_text(size=18),
                     strip.text.x=element_text(size=16), 
                     strip.text.y=element_text(size=16),
                     legend.title=element_text(size=18), 
                     legend.text=element_text(size=16))



Divvy_2020_data_v2 %>% 
  mutate(month_name=month(started_at,label=TRUE))%>%
  group_by(month_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(member_casual == 'casual') %>%
  drop_na() %>%
  ggplot(aes(x = month_name, y = number_of_rides, fill = member_casual)) + 
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = "Month", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Month: Casual Riders") + custom_theme





# average number of rides by hour (casual riders)

options(repr.plot.width = 12, repr.plot.height = 8)

Divvy_2020_data_v2 %>%
  filter(member_casual == 'casual') %>%
  group_by(hour_of_day = hour(round_date(started_at, 'hour'))) %>% 
  group_by(hour_of_day, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(-number_of_rides) %>% 
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual , color="brown")) +
  scale_fill_manual(values=c("casual"="turquoise4"))+
  geom_bar(position = 'dodge', stat = 'identity') + scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) +
  labs(x = "Time of the Day (h)", y = "Number of Rides", 
       fill = "Member/Casual",
       title = "Average Number of Rides by Hour: Casual Riders") + custom_theme





# top 10 stations (members)



# Top 10 start stations for casual riders
Top_10_stations_casual_2020 <- Divvy_2020_data %>%
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "Member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=10)





options(repr.plot.width = 10, repr.plot.height = 6)

ggplot(Top_10_stations_casual_2020) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "thistle") +
  labs(title = "Top 10 Used Stations by Members", y = "Number of Rides", x = "") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() + custom_theme




# top 10 stations (casual riders)

ggplot(data = top_10_station_casual) +
  geom_col(aes(x = reorder(stations, station_count), y = station_count), fill = "lightsalmon") +
  labs(title = "Top 10 Used Stations by Casual Riders", x = "", y = "Number of Rides") + 
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  theme_minimal() + custom_theme


# top 10 start stations (casual riders)

options(repr.plot.width = 10, repr.plot.height = 6)

Divvy_2020_data %>% 
  group_by(start_station_name, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  filter(start_station_name != "", member_casual != "member") %>% 
  arrange(-number_of_rides) %>% 
  head(n=10) %>%
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_col(position = 'dodge', fill = "turquoise3") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = 'Top 10 Start Stations for Casual Riders', x = '', y = "Number of Rides") +
  coord_flip() +
  theme_minimal() +
  custom_theme



# usage of different bikes by rider type

options(repr.plot.width = 12, repr.plot.height = 8)

Divvy_2020_data %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("casual"="tan1","member"="palevioletred4" )) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~rideable_type) +
  labs(fill = "Member/Casual", x = "", y = "Number of Rides", 
       title = "Usage of Different Bikes: Members vs. Casual Riders") + custom_theme



#  usage of different bikes by rider type (separated)

options(repr.plot.width = 14, repr.plot.height = 10)

Divvy_2020_data %>% 
  mutate(month_name=month(started_at,label=TRUE))%>%
  group_by(month_name, member_casual, rideable_type) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = month_name, y = number_of_rides, fill = member_casual,color="black")) +
  geom_col(position = "dodge") +
  scale_fill_manual(values=c("casual"="deeppink4","member"="turquoise"))+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~rideable_type) +
  labs(x = "Month", y = "Number of Rides", fill = "Member/Casual",
       title = "Average Number of Rides by Month") +
  theme(axis.text.x = element_text(angle = 90)) + custom_theme



#  number of rides between members and casual riders by day of week across the year¶

options(repr.plot.width = 26, repr.plot.height = 10)

Divvy_2020_data %>% 
  mutate(month_name=month(started_at,label=TRUE),weekday=wday(started_at,label=TRUE))%>%
  group_by(month_name, weekday, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop') %>% 
  drop_na() %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual,color="black")) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("casual"="rosybrown1","member"="turquoise3"))+
  scale_y_continuous(labels = scales::comma) +
  facet_grid(member_casual~month_name) +
  labs(x = "Day of Week", y = "Number of Rides", fill = "Member/Casual",
       title = "Bike Usage between Members and Casual Riders by Day of Week across the Year", fill = 'Member/Casual') +
  theme(axis.text.x = element_text(angle = 90)) +
  custom_theme



