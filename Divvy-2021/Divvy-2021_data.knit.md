---
title: "Untitled"
author: "Piyush Singh"
date: "7/31/2021"
output: html_document
---


```r
library(tidyverse) #helps wrangle data
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
```

```
## v ggplot2 3.3.5     v purrr   0.3.4
## v tibble  3.1.2     v dplyr   1.0.7
## v tidyr   1.1.3     v stringr 1.4.0
## v readr   1.4.0     v forcats 0.5.1
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(lubridate) #helps wrangle date attributes
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2) #helps visualize data

library(readr)
```

## COLLECTING DATA

### Reading all the .csv files for first four months of year 2021 :



Divvy_Trips_2021_01 <- read_csv("...//202101-divvy-tripdata.csv")



Divvy_Trips_2021_02 <- read_csv("...//202102-divvy-tripdata.csv")



Divvy_Trips_2021_03 <- read_csv("...//202103-divvy-tripdata.csv")



Divvy_Trips_2021_04 <- read_csv("...//202104-divvy-tripdata.csv")




```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   ride_id = col_character(),
##   rideable_type = col_character(),
##   started_at = col_character(),
##   ended_at = col_character(),
##   start_station_name = col_character(),
##   start_station_id = col_character(),
##   end_station_name = col_character(),
##   end_station_id = col_character(),
##   start_lat = col_double(),
##   start_lng = col_double(),
##   end_lat = col_double(),
##   end_lng = col_double(),
##   member_casual = col_character()
## )
```

```
## 
## -- Column specification --------------------------------------------------------
## cols(
##   ride_id = col_character(),
##   rideable_type = col_character(),
##   started_at = col_datetime(format = ""),
##   ended_at = col_datetime(format = ""),
##   start_station_name = col_character(),
##   start_station_id = col_character(),
##   end_station_name = col_character(),
##   end_station_id = col_character(),
##   start_lat = col_double(),
##   start_lng = col_double(),
##   end_lat = col_double(),
##   end_lng = col_double(),
##   member_casual = col_character()
## )
## 
## 
## -- Column specification --------------------------------------------------------
## cols(
##   ride_id = col_character(),
##   rideable_type = col_character(),
##   started_at = col_datetime(format = ""),
##   ended_at = col_datetime(format = ""),
##   start_station_name = col_character(),
##   start_station_id = col_character(),
##   end_station_name = col_character(),
##   end_station_id = col_character(),
##   start_lat = col_double(),
##   start_lng = col_double(),
##   end_lat = col_double(),
##   end_lng = col_double(),
##   member_casual = col_character()
## )
## 
## 
## -- Column specification --------------------------------------------------------
## cols(
##   ride_id = col_character(),
##   rideable_type = col_character(),
##   started_at = col_datetime(format = ""),
##   ended_at = col_datetime(format = ""),
##   start_station_name = col_character(),
##   start_station_id = col_character(),
##   end_station_name = col_character(),
##   end_station_id = col_character(),
##   start_lat = col_double(),
##   start_lng = col_double(),
##   end_lat = col_double(),
##   end_lng = col_double(),
##   member_casual = col_character()
## )
```



## WRANGLE DATA AND COMBINE INTO A SINGLE FILE


### Removing all the unecessary columns (start_lat,start_lng, end_lat, end_lng) :


```r
Divvy_Trips_2021_01<- Divvy_Trips_2021_01 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


Divvy_Trips_2021_02<- Divvy_Trips_2021_02 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


Divvy_Trips_2021_03<- Divvy_Trips_2021_03 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))


Divvy_Trips_2021_04 <- Divvy_Trips_2021_04 %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
```

### Changing station_id columns to double format : 



```r
Divvy_Trips_2021_01$start_station_id <- as.double(Divvy_Trips_2021_01$start_station_id)
Divvy_Trips_2021_01$end_station_id <- as.double(Divvy_Trips_2021_01$end_station_id)

Divvy_Trips_2021_02$start_station_id <- as.double(Divvy_Trips_2021_02$start_station_id)
Divvy_Trips_2021_02$end_station_id <- as.double(Divvy_Trips_2021_02$end_station_id)

Divvy_Trips_2021_03$start_station_id <- as.double(Divvy_Trips_2021_03$start_station_id)
Divvy_Trips_2021_03$end_station_id <- as.double(Divvy_Trips_2021_03$end_station_id)

Divvy_Trips_2021_04$start_station_id <- as.double(Divvy_Trips_2021_04$start_station_id)
Divvy_Trips_2021_04$end_station_id <- as.double(Divvy_Trips_2021_04$end_station_id)
```


### Changing the format of started_at and ended_at columns to date_time : 

```r
Divvy_Trips_2021_01$started_at <- dmy_hm(Divvy_Trips_2021_01$started_at)
Divvy_Trips_2021_01$ended_at <- dmy_hm(Divvy_Trips_2021_01$ended_at)
View(Divvy_Trips_2021_01)
```

### Creating the ride_length column for all the month files :

```r
Divvy_Trips_2021_01$ride_length <- difftime(Divvy_Trips_2021_01$ended_at,Divvy_Trips_2021_01$started_at)
Divvy_Trips_2021_02$ride_length <- difftime(Divvy_Trips_2021_02$ended_at,Divvy_Trips_2021_02$started_at)
Divvy_Trips_2021_03$ride_length <- difftime(Divvy_Trips_2021_03$ended_at,Divvy_Trips_2021_03$started_at)
Divvy_Trips_2021_04$ride_length <- difftime(Divvy_Trips_2021_04$ended_at,Divvy_Trips_2021_04$started_at)
```


### reordering the columns :


```r
Divvy_Trips_2021_01 <- Divvy_Trips_2021_01[,c(1,2,3,4,10,5:9)]
Divvy_Trips_2021_02 <- Divvy_Trips_2021_02[,c(1,2,3,4,10,5:9)]
Divvy_Trips_2021_03 <- Divvy_Trips_2021_03[,c(1,2,3,4,10,5:9)]
Divvy_Trips_2021_04 <- Divvy_Trips_2021_04[,c(1,2,3,4,10,5:9)]
```



### adding date,day,month,year columns :


```r
Divvy_Trips_2021_01$date <- format(as.Date(Divvy_Trips_2021_01$started_at, "%Y-%m-%d"), "%d-%m-%Y" )
```

```
## Warning in as.POSIXlt.POSIXct(x, tz = tz): unknown timezone '%Y-%m-%d'
```

```r
Divvy_Trips_2021_01$month <- format(as.Date(Divvy_Trips_2021_01$date), "%m")
Divvy_Trips_2021_01$day <- format(as.Date(Divvy_Trips_2021_01$date), "%d")
Divvy_Trips_2021_01$year <- format(as.Date(Divvy_Trips_2021_01$date), "%Y")
Divvy_Trips_2021_01$day_of_week <- format(as.Date(Divvy_Trips_2021_01$date), "%A")


Divvy_Trips_2021_02$date <- format(as.Date(Divvy_Trips_2021_02$started_at, "%Y-%m-%d"), "%d-%m-%Y" )
```

```
## Warning in as.POSIXlt.POSIXct(x, tz = tz): unknown timezone '%Y-%m-%d'
```

```r
Divvy_Trips_2021_02$month <- format(as.Date(Divvy_Trips_2021_02$date), "%m")
Divvy_Trips_2021_02$day <- format(as.Date(Divvy_Trips_2021_02$date), "%d")
Divvy_Trips_2021_02$year <- format(as.Date(Divvy_Trips_2021_02$date), "%Y")
Divvy_Trips_2021_02$day_of_week <- format(as.Date(Divvy_Trips_2021_02$date), "%A")



Divvy_Trips_2021_03$date <- format(as.Date(Divvy_Trips_2021_03$started_at, "%Y-%m-%d"), "%d-%m-%Y" )
```

```
## Warning in as.POSIXlt.POSIXct(x, tz = tz): unknown timezone '%Y-%m-%d'
```

```r
Divvy_Trips_2021_03$month <- format(as.Date(Divvy_Trips_2021_03$date), "%m")
Divvy_Trips_2021_03$day <- format(as.Date(Divvy_Trips_2021_03$date), "%d")
Divvy_Trips_2021_03$year <- format(as.Date(Divvy_Trips_2021_03$date), "%Y")
Divvy_Trips_2021_03$day_of_week <- format(as.Date(Divvy_Trips_2021_03$date), "%A")



Divvy_Trips_2021_04$date <- format(as.Date(Divvy_Trips_2021_04$started_at, "%Y-%m-%d"), "%d-%m-%Y" )
```

```
## Warning in as.POSIXlt.POSIXct(x, tz = tz): unknown timezone '%Y-%m-%d'
```

```r
Divvy_Trips_2021_04$month <- format(as.Date(Divvy_Trips_2021_04$date), "%m")
Divvy_Trips_2021_04$day <- format(as.Date(Divvy_Trips_2021_04$date), "%d")
Divvy_Trips_2021_04$year <- format(as.Date(Divvy_Trips_2021_04$date), "%Y")
Divvy_Trips_2021_04$day_of_week <- format(as.Date(Divvy_Trips_2021_04$date), "%A")
```



### Merging all month files into one quarter file of 2021 :


```r
Divvy_2021_Q1_data <- bind_rows(Divvy_Trips_2021_01 , Divvy_Trips_2021_02,Divvy_Trips_2021_03,Divvy_Trips_2021_04)
View(Divvy_2021_Q1_data)
```




## CONDUCT DESCRIPTIVE ANALYSIS

### Descriptive analysis on ride_length (all figures in seconds)


```r
mean(Divvy_2021_Q1_data$ride_length) 
```

```
## Time difference of 1352.703 secs
```

```r
median(Divvy_2021_Q1_data$ride_length) 
```

```
## Time difference of 719 secs
```

```r
max(Divvy_2021_Q1_data$ride_length)
```

```
## Time difference of 2866602 secs
```

```r
min(Divvy_2021_Q1_data$ride_length) 
```

```
## Time difference of -132 secs
```


### Summary of the ride length column :

```r
summary(Divvy_2021_Q1_data$ride_length)
```

```
##   Length    Class     Mode 
##   712182 difftime  numeric
```
### Comparing  members and casual riders :


```r
aggregate(Divvy_2021_Q1_data$ride_length ~ Divvy_2021_Q1_data$member_casual, FUN = mean)
```

```
##   Divvy_2021_Q1_data$member_casual Divvy_2021_Q1_data$ride_length
## 1                           casual                 2257.9619 secs
## 2                           member                  866.4026 secs
```

```r
aggregate(Divvy_2021_Q1_data$ride_length ~ Divvy_2021_Q1_data$member_casual, FUN = median)
```

```
##   Divvy_2021_Q1_data$member_casual Divvy_2021_Q1_data$ride_length
## 1                           casual                      1058 secs
## 2                           member                       600 secs
```

```r
aggregate(Divvy_2021_Q1_data$ride_length ~ Divvy_2021_Q1_data$member_casual, FUN = max)
```

```
##   Divvy_2021_Q1_data$member_casual Divvy_2021_Q1_data$ride_length
## 1                           casual                   2866602 secs
## 2                           member                     93596 secs
```

```r
aggregate(Divvy_2021_Q1_data$ride_length ~ Divvy_2021_Q1_data$member_casual, FUN = min)
```

```
##   Divvy_2021_Q1_data$member_casual Divvy_2021_Q1_data$ride_length
## 1                           casual                        -1 secs
## 2                           member                      -132 secs
```
### Tthe average ride time by each day for members vs casual riders :


### Ordering the days of week :


```r
Divvy_2021_Q1_data$day_of_week <- ordered(Divvy_2021_Q1_data$day_of_week, levels=c("Sunday", "Monday",
"Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```
### the average ride time by each day for members vs casual riders :


```r
aggregate(Divvy_2021_Q1_data$ride_length ~ Divvy_2021_Q1_data$member_casual + Divvy_2021_Q1_data$day_of_week,
FUN = mean)
```

```
##    Divvy_2021_Q1_data$member_casual Divvy_2021_Q1_data$day_of_week
## 1                            casual                         Sunday
## 2                            member                         Sunday
## 3                            casual                         Monday
## 4                            member                         Monday
## 5                            casual                        Tuesday
## 6                            member                        Tuesday
## 7                            casual                      Wednesday
## 8                            member                      Wednesday
## 9                            casual                       Thursday
## 10                           member                       Thursday
## 11                           casual                         Friday
## 12                           member                         Friday
## 13                           casual                       Saturday
## 14                           member                       Saturday
##    Divvy_2021_Q1_data$ride_length
## 1                  2219.6374 secs
## 2                   865.6892 secs
## 3                  2365.6413 secs
## 4                   860.4289 secs
## 5                  2425.6751 secs
## 6                   879.7919 secs
## 7                  2194.4380 secs
## 8                   871.0516 secs
## 9                  2264.8108 secs
## 10                  850.9200 secs
## 11                 2100.1359 secs
## 12                  860.3463 secs
## 13                 2271.0202 secs
## 14                  876.5984 secs
```


### Analyzing ridership data by type and weekday :


```r
Divvy_2021_Q1_data %>%
mutate(weekday = wday(started_at, label = TRUE)) %>% 
group_by(member_casual, weekday) %>%
summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
arrange(member_casual, weekday)
```

```
## `summarise()` has grouped output by 'member_casual'. You can override using the `.groups` argument.
```

```
## # A tibble: 14 x 4
## # Groups:   member_casual [2]
##    member_casual weekday number_of_rides average_duration
##    <chr>         <ord>             <int> <drtn>          
##  1 casual        Sun               46795 2470.8180 secs  
##  2 casual        Mon               30693 2326.6684 secs  
##  3 casual        Tue               33729 2266.3775 secs  
##  4 casual        Wed               24091 2004.3612 secs  
##  5 casual        Thu               21228 1548.5407 secs  
##  6 casual        Fri               35040 2278.1641 secs  
##  7 casual        Sat               57306 2399.4471 secs  
##  8 member        Sun               56305  980.6670 secs  
##  9 member        Mon               65706  866.6958 secs  
## 10 member        Tue               71683  841.5144 secs  
## 11 member        Wed               65961  819.6843 secs  
## 12 member        Thu               62063  778.7334 secs  
## 13 member        Fri               72914  829.3132 secs  
## 14 member        Sat               68668  961.9068 secs
```


###   Visualizing the number of rides by rider type :

```r
Divvy_2021_Q1_data %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(mapping=aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
```

```
## `summarise()` has grouped output by 'member_casual'. You can override using the `.groups` argument.
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-16-1.png" width="672" />




### Visualization for average duration :


```r
Divvy_2021_Q1_data%>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

```
## `summarise()` has grouped output by 'member_casual'. You can override using the `.groups` argument.
```

```
## Don't know how to automatically pick scale for object of type difftime. Defaulting to continuous.
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-17-1.png" width="672" />




### Comparing general bike type preference between members and casual riders :


```r
Divvy_2021_Q1_data%>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(number_of_rides = n(), .groups = 'drop')
```

```
## # A tibble: 6 x 3
##   rideable_type member_casual number_of_rides
##   <chr>         <chr>                   <int>
## 1 classic_bike  casual                 130258
## 2 classic_bike  member                 333618
## 3 docked_bike   casual                  43747
## 4 docked_bike   member                      1
## 5 electric_bike casual                  74877
## 6 electric_bike member                 129681
```



### Comparing number of docked_bike rides between members and casual riders for each day of week

```r
Divvy_2021_Q1_data%>%
  filter(rideable_type == 'docked_bike') %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop') %>% 
  arrange(day_of_week)
```

```
## # A tibble: 8 x 3
##   member_casual day_of_week number_of_rides
##   <chr>         <ord>                 <int>
## 1 casual        Sunday                 5940
## 2 member        Sunday                    1
## 3 casual        Monday                 4176
## 4 casual        Tuesday                6749
## 5 casual        Wednesday              6212
## 6 casual        Thursday               5852
## 7 casual        Friday                 6697
## 8 casual        Saturday               8121
```



### Docked_bike rides of casual riders for each day of week :


```r
Divvy_2021_Q1_data %>% 
  filter(rideable_type == 'docked_bike', member_casual == 'casual') %>% 
  group_by(day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'drop')
```

```
## # A tibble: 7 x 2
##   day_of_week number_of_rides
##   <ord>                 <int>
## 1 Sunday                 5940
## 2 Monday                 4176
## 3 Tuesday                6749
## 4 Wednesday              6212
## 5 Thursday               5852
## 6 Friday                 6697
## 7 Saturday               8121
```

### Creating a custom theme :


```r
custom_theme = theme(plot.title=element_text(size=20),
                 axis.text.x=element_text(size=10), 
                 axis.text.y=element_text(size=12),
                 axis.title.x=element_text(size=18), 
                 axis.title.y=element_text(size=18),
                 strip.text.x=element_text(size=16), 
                 strip.text.y=element_text(size=16),
                 legend.title=element_text(size=18), 
                 legend.text=element_text(size=16))
```



### Average Number of Rides by Month: Casual Riders


```r
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
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-22-1.png" width="672" />






###  average number of rides by hour (casual riders) :


```r
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
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-23-1.png" width="672" />



### Top 10 start stations (members)


```r
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
  custom_theme
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-24-1.png" width="672" />




### Top 10 start stations (casual riders) :


```r
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
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-25-1.png" width="672" />



### Usage of different bikes by rider type :


```r
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
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-26-1.png" width="672" />



###  Usage of different bikes by rider type (separated) :



```r
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
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-27-1.png" width="672" />



###  Number of rides between members and casual riders by day of week across the year:


```r
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
```

<img src="Divvy-2021_data_files/figure-html/unnamed-chunk-28-1.png" width="672" />




























