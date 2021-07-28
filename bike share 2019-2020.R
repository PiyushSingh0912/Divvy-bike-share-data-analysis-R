library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(ggplot2) #helps visualize data


#.................................................................................................



library(readr)


#DATA 2019 .......................................................

Divvy_trip_2019_Q1<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_trip_2019_Q1.csv")
Divvy_trip_2019_Q2<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_trip_2019_Q2.csv")
Divvy_trip_2019_Q3<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_trip_2019_Q3.csv")
Divvy_trip_2019_Q4<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_trip_2019_Q4.csv")

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




write.csv(Divvy_trip_2019_Q1,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_trip_2019_Q1.csv", row.names = FALSE)
write.csv(Divvy_trip_2019_Q2,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_trip_2019_Q2.csv", row.names = FALSE)
write.csv(Divvy_trip_2019_Q3,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_trip_2019_Q3.csv", row.names = FALSE)
write.csv(Divvy_trip_2019_Q4,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_trip_2019_Q4.csv", row.names = FALSE)










# Merging all quarter files of 2019 into one year file :- 

Divvy_2019_data <- bind_rows(Divvy_trip_2019_Q1 , Divvy_trip_2019_Q2,Divvy_trip_2019_Q3,Divvy_trip_2019_Q4)
View(Divvy_2019_data)


Divvy_trip_2019_Q4 <- mutate(Divvy_trip_2019_Q4, month= as.character(month))

write.csv(Divvy_2019_data,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_2019_data.csv", row.names = FALSE)

Divvy_2019_data <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_2019_data.csv")



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











# DATA 2020 TRIPS ..............................................................


Divvy_Trips_2020_Q1 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_Trips_2020_Q1.csv")
View(Divvy_Trips_2020_Q1)

Divvy_tripdata_2020_04 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202004-divvy-tripdata.csv")


Divvy_tripdata_2020_05 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202005-divvy-tripdata.csv")


Divvy_tripdata_2020_06 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202006-divvy-tripdata.csv")



Divvy_tripdata_2020_07<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202007-divvy-tripdata.csv")
View(Divvy_tripdata_2020_07)


Divvy_tripdata_2020_08 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202008-divvy-tripdata.csv")

Divvy_tripdata_2020_09<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202009-divvy-tripdata.csv")

Divvy_tripdata_2020_10<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202010-divvy-tripdata.csv")
Divvy_tripdata_2020_11<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202011-divvy-tripdata.csv")
Divvy_tripdata_2020_12<- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/202012-divvy-tripdata.csv")

Divvy_tripdata_2020_10$started_at <- dmy_hm(Divvy_tripdata_2020_10$started_at)
Divvy_tripdata_2020_10$ended_at <- dmy_hm(Divvy_tripdata_2020_10$ended_at)
View(Divvy_tripdata_2020_10)



Divvy_tripdata_2020_Q1 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_tripdata_2020_Q1.csv")
Divvy_tripdata_2020_Q2 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_tripdata_2020_Q2.csv")
Divvy_tripdata_2020_Q3 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_tripdata_2020_Q3.csv")
Divvy_tripdata_2020_Q4 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_tripdata_2020_Q4.csv")


Divvy_data_2020 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_data_2020.csv")
Divvy_data_2019 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_data_2019.csv")

Divvy_2020_data <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_2020_data_v2.csv")




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






write.csv(Divvy_tripdata_2020_Q1,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_tripdata_2020_Q1.csv", row.names = FALSE)
write.csv(Divvy_tripdata_2020_Q2,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_tripdata_2020_Q2.csv", row.names = FALSE)
write.csv(Divvy_tripdata_2020_Q3,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_tripdata_2020_Q3.csv", row.names = FALSE)
write.csv(Divvy_tripdata_2020_Q4,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_tripdata_2020_Q4.csv", row.names = FALSE)




write.csv(Divvy_data_2020,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_data_2020.csv", row.names = FALSE)
write.csv(Divvy_data_2019,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_data_2019.csv", row.names = FALSE)




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







Divvy_Trips_2019_Q1 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_T_2019_Q1.csv")
View(Divvy_Trips_2019_Q1)

Divvy_Trips_2019_Q2 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_T_2019_Q2.csv")
View(Divvy_Trips_2019_Q2)

Divvy_Trips_2019_Q3 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_T_2019_Q3.csv")
View(Divvy_Trips_2019_Q3)

Divvy_Trips_2019_Q4 <- read_csv("C:/Users/piyus/Desktop/bike-share PROJECT/Divvy_T_2019_Q4.csv")
View(Divvy_Trips_2019_Q4)


# Changing the names of the columns for all quarters of year 2019 :-


#renaming 2019 Q1 .............
Divvy_Trips_2019_Q1 <- rename(Divvy_Trips_2019_Q1
                  ,ride_id = trip_id
                  ,rideable_type = bikeid
                  ,started_at = start_time
                  ,ended_at = end_time
                  ,start_station_name = from_station_name
                  ,start_station_id = from_station_id
                  ,end_station_name = to_station_name
                  ,end_station_id = to_station_id
                  ,member_casual = usertype)



#renaming 2019 Q2 .............
Divvy_Trips_2019_Q2<- rename(Divvy_Trips_2019_Q2
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID"
                   ,started_at = "01 - Rental Details Local Start Time"
                   ,ended_at = "01 - Rental Details Local End Time"
                   ,start_station_name = "03 - Rental Start Station Name"
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name"
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type")




#renaming 2019 Q3 .............
Divvy_Trips_2019_Q3 <- rename(Divvy_Trips_2019_Q3
                              ,ride_id = trip_id
                              ,rideable_type = bikeid
                              ,started_at = start_time
                              ,ended_at = end_time
                              ,start_station_name = from_station_name
                              ,start_station_id = from_station_id
                              ,end_station_name = to_station_name
                              ,end_station_id = to_station_id
                              ,member_casual = usertype)


#renaming 2019 Q4 .............
Divvy_Trips_2019_Q4 <- rename(Divvy_Trips_2019_Q4
                              ,ride_id = trip_id
                              ,rideable_type = bikeid
                              ,started_at = start_time
                              ,ended_at = end_time
                              ,start_station_name = from_station_name
                              ,start_station_id = from_station_id
                              ,end_station_name = to_station_name
                              ,end_station_id = to_station_id
                              ,member_casual = usertype)




Divvy_data_2019 <- mutate(Divvy_data_2019, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))


Divvy_Trips_2019_Q2 <- mutate(Divvy_Trips_2019_Q2, ride_id = as.character(ride_id)
                              ,rideable_type = as.character(rideable_type))


Divvy_Trips_2019_Q3 <- mutate(Divvy_Trips_2019_Q3, ride_id = as.character(ride_id)
                              ,rideable_type = as.character(rideable_type))


Divvy_Trips_2019_Q4 <- mutate(Divvy_Trips_2019_Q4, ride_id = as.character(ride_id),rideable_type = as.character(rideable_type))



Divvy_Trips_2019_Q1<- Divvy_Trips_2019_Q1 %>%
  select(-c(gender,birthyear))

Divvy_Trips_2019_Q2<- Divvy_Trips_2019_Q2 %>%
  select(-c(gender,birthyear))

Divvy_Trips_2019_Q3<- Divvy_Trips_2019_Q3 %>%
  select(-c(gender,birthyear))

Divvy_Trips_2019_Q4<- Divvy_Trips_2019_Q4 %>%
  select(-c(gender,birthyear))


colnames(Divvy_Trips_2019_Q2)[11] <- "gender"
colnames(Divvy_Trips_2019_Q2)[12] <- "birthyear"



colnames(Divvy_Trips_2019_Q1)[5] <- "ride_length"
colnames(Divvy_Trips_2019_Q2)[5] <- "ride_length"
colnames(Divvy_Trips_2019_Q3)[5] <- "ride_length"
colnames(Divvy_Trips_2019_Q4)[5] <- "ride_length"




View(Divvy_Trips_2019_Q1)



Divvy_Trips_2019_Q1$started_at <- dmy_hm(Divvy_Trips_2019_Q1$started_at)
Divvy_Trips_2019_Q2$started_at <- dmy_hm(Divvy_Trips_2019_Q2$started_at)
Divvy_Trips_2019_Q3$started_at <- dmy_hm(Divvy_Trips_2019_Q3$started_at)
Divvy_Trips_2019_Q4$started_at <- dmy_hm(Divvy_Trips_2019_Q4$started_at)


Divvy_Trips_2019_Q1$ended_at <- dmy_hm(Divvy_Trips_2019_Q1$ended_at)
Divvy_Trips_2019_Q2$ended_at <- dmy_hm(Divvy_Trips_2019_Q2$ended_at)
Divvy_Trips_2019_Q3$ended_at <- dmy_hm(Divvy_Trips_2019_Q3$ended_at)
Divvy_Trips_2019_Q4$ended_at <- dmy_hm(Divvy_Trips_2019_Q4$ended_at)


Divvy_tripdata_2020_Q1$started_at <- dmy_hm(Divvy_tripdata_2020_Q1$started_at)
Divvy_tripdata_2020_Q1$ended_at <- dmy_hm(Divvy_tripdata_2020_Q1$ended_at)




Divvy_Trips_2019_Q2 <- mutate(Divvy_Trips_2019_Q2,started_at=dmy_hm(started_at),ended_at=dmy_hm(ended_at))
Divvy_Trips_2019_Q3 <- mutate(Divvy_Trips_2019_Q3,started_at=dmy_hm(started_at)
                              ,ended_at=dmy_hm(ended_at))
Divvy_Trips_2019_Q4 <- mutate(Divvy_Trips_2019_Q4, started_at = dmy_hm(started_at),ended_at = dmy_hm(ended_at))
Divvy_Trips_2019_Q1 <- mutate(Divvy_Trips_2019_Q1, started_at = dmy_hm(started_at),ended_at = dmy_hm(ended_at))


# merging all 2019 data into one year data :-

Divvy_Trips_2019<- bind_rows(Divvy_Trips_2019_Q1 , Divvy_Trips_2019_Q2,Divvy_Trips_2019_Q3,Divvy_Trips_2019_Q4)
View(Divvy_data_2019)
View(Divvy_data_2020)



write.csv(Divvy_Trips_2019,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_data_2019.csv", row.names = FALSE)






Divvy_data_2019$date <- format(as.Date(Divvy_data_2019$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_data_2019$month <- format(as.Date(Divvy_data_2019$date), "%m")
Divvy_data_2019$day <- format(as.Date(Divvy_data_2019$date), "%d")
Divvy_data_2019$year <- format(as.Date(Divvy_data_2019$date), "%Y")
Divvy_data_2019$day_of_week <- format(as.Date(Divvy_data_2019$date), "%A")


Divvy_2020_data$date <- format(as.Date(Divvy_2020_data$started_at, "%Y-%m-%d"), "%d-%m-%Y" )#The default format is yyyy-mm-dd
Divvy_2020_data$month <- format(as.Date(Divvy_2020_data$date), "%m")
Divvy_2020_data$day <- format(as.Date(Divvy_2020_data$date), "%d")
Divvy_2020_data$year <- format(as.Date(Divvy_2020_data$date), "%Y")
Divvy_2020_data$day_of_week <- format(as.Date(Divvy_2020_data$date), "%A")



#.............................................................................

colnames(Divvy_Trips_2019_Q1)
colnames(Divvy_Trips_2019_Q2)
colnames(Divvy_Trips_2019_Q3)
colnames(Divvy_Trips_2019_Q4)


summary(Divvy_Trips_2019_Q1)
summary(Divvy_Trips_2019_Q2)
summary(Divvy_Trips_2019_Q3)
summary(Divvy_Trips_2019_Q4)

typeof(Divvy_Trips_2019_Q1)
colnames(Divvy_Trips_2019_Q2)
colnames(Divvy_Trips_2019_Q3)
colnames(Divvy_Trips_2019_Q4)




write.csv(Divvy_Trips_2019_Q1,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_T_2019_Q1.csv", row.names = FALSE)
write.csv(Divvy_Trips_2019_Q2,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_T_2019_Q2.csv", row.names = FALSE)
write.csv(Divvy_Trips_2019_Q3,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_T_2019_Q3.csv", row.names = FALSE)
write.csv(Divvy_Trips_2019_Q4,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_T_2019_Q4.csv", row.names = FALSE)



year_2019_data <- bind_rows(Divvy_Trips_2019_Q1 , Divvy_Trips_2019_Q2,Divvy_Trips_2019_Q3 , Divvy_Trips_2019_Q4)

View(year_2019_data)


Divvy_2020_data <- bind_rows( Divvy_tripdata_2020_Q1 ,  Divvy_tripdata_2020_Q2, Divvy_tripdata_2020_Q3 , Divvy_tripdata_2020_Q4)

View(Divvy_2020_data)



# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and
#checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed

Divvy_2020_data_v2 <- Divvy_2020_data[!(Divvy_2020_data$start_station_name == "HQ QR" | Divvy_2020_data$ride_length<0),]

View(Divvy_2020_data_v2)


write.csv(Divvy_2020_data_v2,"C:\\Users\\piyus\\Desktop\\bike-share PROJECT\\Divvy_2020_data_v2.csv", row.names = FALSE)

write.csv(Divvy_2020_data,"C:/Users/piyus/Desktop/bike-share PROJECT//Divvy_2020_data.csv", row.names = FALSE)

memory.limit(size = 12000)




library(dplyr)


all_quart_data <- bind_rows(Divvy_Trips_2018_Q1 , Divvy_Trips_2018_Q2,Divvy_Trips_2018_Q3 , Divvy_Trips_2018_Q4)

View(all_quart_data)



colnames(all_quart_data) #List of column names
nrow(all_quart_data) #How many rows are in data frame?
dim(all_quart_data) #Dimensions of the data frame?
head(all_quart_data) #See the first 6 rows of data frame. Also tail(qs_raw)
str(all_quart_data) #See list of columns and data types (numeric, character, etc)



summary_data_2019 <- summary(Divvy_data_2019) #Statistical summary of data. Mainly for numerics
View(summary_data_2019)

summary_data_2020 <- summary(Divvy_data_2020) #Statistical summary of data. Mainly for numerics
View(summary_data_2020)



write.csv(summary_data_2019,"C:/Users/piyus/Desktop/bike-share PROJECT//summary_data_2019.csv", row.names = FALSE)

write.csv(summary_data_2020,"C:/Users/piyus/Desktop/bike-share PROJECT//summary_data_2020.csv", row.names = FALSE)




all_quart_data <- all_quart_data %>%
  mutate(User_Type = recode(User_Type,"Subscriber" = "Member" , "Customer" = "Casual"))

lubridate::dmy(D)

library(lubridate)

all_quart_data$date <- format(as.Date(all_quart_data$Start_Time, "%d-%m-%y"), "%d/%m/%Y") 
all_quart_data$month <- format(as.Date(all_quart_data$date,"%d/%m/%Y"), "%m")
all_quart_data$day <- format(as.Date(all_quart_data$date,"%d/%m/%Y"), "%d")
all_quart_data$year <- format(as.Date(all_quart_data$date,"%d/%m/%Y"), "%Y")
all_quart_data$day_of_week <- format(as.Date(all_quart_data$date,"%d/%m/%Y"), "%A")

all_quart_data$date <- as.Date(all_quart_data$Start_Time)
as.Da


colnames(all_quart_data)[13] <- "date"
View(all_quart_data)
colnames(all_quart_data)[5] <- "Ride_length"





#............Descriptive Analysis.............................................................


mean(all_quart_data2$Ride_length) #straight average (total ride length / rides)
median(all_quart_data$Ride_length) #midpoint number in the ascending array of ride lengths
max(all_quart_data$Ride_length) #longest ride
min(all_quart_data$Ride_length) #shortest ride

summary(all_quart_data$Ride_length)# it does the same job as above 4 lines ...

# Compare members and casual users...............................
aggregate(Divvy_data_2019$ride_length ~ Divvy_data_2019$member_casual, FUN = mean)
aggregate(Divvy_data_2019$ride_length ~ Divvy_data_2019$member_casual, FUN = median)
aggregate(Divvy_data_2019$ride_length ~ Divvy_data_2019$member_casual, FUN = max)
aggregate(Divvy_data_2019$ride_length ~ Divvy_data_2019$member_casual, FUN = min)

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


write.csv(Divvy_2020_type_weekday,"C:\\Users\\piyus\\Desktop\\bike-share PROJECT\\Divvy_2020_type_weekday.csv", row.names = FALSE)

View(Divvy_2020_type_weekday)

data_by_week <- all_quart_data %>%
  mutate(weekday = wday(Start_Time, label = TRUE)) %>% 
  group_by(User_Type, weekday) %>%
  summarise(number_of_rides = n() , average_duration = mean(Ride_length)) %>%
  arrange(User_Type,weekday) # sorts 



View(all_quart_data)

write.csv(data_by_week,"C:/Users/piyus/Desktop/bike-share PROJECT//Bike_Data_by_week.csv", row.names = FALSE)

mean(all_quart_data$Bike_ID)




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




write.csv(all_quart_data,"C:/Users/piyus/Desktop/bike-share PROJECT//Bike_Data.csv", row.names = FALSE)






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
  theme_minimal() + my_theme


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



























