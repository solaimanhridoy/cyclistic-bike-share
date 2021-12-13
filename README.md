---
title: "Cyclistic Bikeshare"
output: html_document:
          keep_md: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Install Required Packages
```{r}
install.packages("tidyverse")
```
```{r}
install.packages("DescTools")
```

## Prepare and Process Data

Importing library tidyverse 
```{r import library}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(lubridate)
```
```{r}
getwd()
setwd("/media/solaiman/D/home/lib10/Insync/hridoy341@gmail.com/Google Drive/Courses and Projects/Data Analysis/Course 08: Capstone Projects/Case Study 01")
```


Loading Data-sets
```{r Load datasets}
tripdata_202011 <- read_csv("202011-divvy-tripdata.csv")
tripdata_202012 <- read_csv("202012-divvy-tripdata.csv")
#tripdata_202101 <- read_csv("202101-divvy-tripdata.csv")
#tripdata_202102 <- read_csv("202102-divvy-tripdata.csv")
#tripdata_202103 <- read_csv("202103-divvy-tripdata.csv")
#tripdata_202104 <- read_csv("202104-divvy-tripdata.csv")
#tripdata_202105 <- read_csv("202105-divvy-tripdata.csv")
#tripdata_202106 <- read_csv("202106-divvy-tripdata.csv")
#tripdata_202107 <- read_csv("202107-divvy-tripdata.csv")
#tripdata_202108 <- read_csv("202108-divvy-tripdata.csv")
#tripdata_202109 <- read_csv("202109-divvy-tripdata.csv")
#tripdata_202110 <- read_csv("202110-divvy-tripdata.csv")
```

```{r what type of data they are now?}
class(tripdata_202011)
```

```{r Check Column Names}
colnames(tripdata_202011)
colnames(tripdata_202012)
#colnames(tripdata_202101)
#colnames(tripdata_202102)
#colnames(tripdata_202103)
#colnames(tripdata_202104)
#colnames(tripdata_202105)
#colnames(tripdata_202106)
#colnames(tripdata_202107)
#colnames(tripdata_202108)
#colnames(tripdata_202109)
#colnames(tripdata_202110)
```


```{r data type}
str(tripdata_202011)
```



Combining all data frames into one data frame
```{r concat datasets}
all_tripdata <- rbind(tripdata_202011,
                  tripdata_202012)
                  #tripdata_202101,
                  #tripdata_202102,
                  #tripdata_202103,
                  #tripdata_202104,
                  #tripdata_202105,
                  #tripdata_202106,
                  #tripdata_202107,
                  #tripdata_202108,
                  #tripdata_202109,
                  #tripdata_202110)
```

```{r}
class(all_tripdata)
```


```{r summary data}
glimpse(all_tripdata)
```

```{r list of column names}
colnames(all_tripdata)
```
```{r number of rows}
nrow(all_tripdata)
```
```{r dimensions of the data frame}
dim(all_tripdata)
```
```{r first 6 rows of the dataframe}
head(all_tripdata)
```
```{r view data in a new tab}
View(all_tripdata)
```


```{r the list of the columns and data types of the all_tripdata}
str(all_tripdata)
```
```{r statistical summary of data}
summary(all_tripdata)
```

```{r check the member_casual columns}
table(all_tripdata$member_casual)
```

Remove rows with missing values
```{r remove missing values}
colSums(is.na(all_tripdata))
```

```{r cleaned tripdata}
all_trips_cleaned <- all_tripdata[complete.cases(all_tripdata), ]
```

```{r cleaned tripdata}
nrow(all_trips_cleaned)
```

``` {r remove started_at geater than end_at}
# data with started_at greater than ended_at will be removed
all_trips_cleaned <- all_trips_cleaned %>%
  filter(all_trips_cleaned$started_at < all_trips_cleaned$ended_at)
```


```{r head of all_trips_cleaned}
head(all_trips_cleaned)
```

```{r create new column ride_length}
all_trips_cleaned$ride_length <- all_trips_cleaned$ended_at - all_trips_cleaned$started_at

```

```{r}
head(all_trips_cleaned$ride_length)
```

```{r ride length sec to period}
all_trips_cleaned$ride_length <- hms::hms(seconds_to_period(all_trips_cleaned$ride_length))
```

```{r}
head(all_trips_cleaned$ride_length)
```

```{r create a new clomun day_of_week}
all_trips_cleaned$day_of_week <- wday(all_trips_cleaned$started_at, label = FALSE)
```


```{r ride_length seconds to period}
all_trips_cleaned$ride_length <- hms::hms(seconds_to_period((all_trips_cleaned$ride_length)))
```

```{r mean of ride_length}
all_trips_cleaned %>% 
  summarise(a = hms::hms(seconds_to_period(mean(ride_length)))) %>% 
  rename_at("a", ~ "Mean of  ride_length") 
```

```{r max of ride_length}
all_trips_cleaned %>% 
  summarise(a = hms::hms(seconds_to_period(max(ride_length)))) %>% 
  rename_at("a", ~ "Max of  ride_length") 
```

```{r min of ride_length}
all_trips_cleaned %>% 
  summarise(a = hms::hms(seconds_to_period(min(ride_length)))) %>% 
  rename_at("a", ~ "Min of  ride_length") 
```

```{r mode of day_of_week}
library(DescTools)
Mode(all_trips_cleaned$day_of_week)
```
```{r}
library(janitor)
clean_names(all_trips_cleaned)
```
```{r}
# average ride_length for members and casual riders
all_trips_cleaned %>% 
  group_by(member_casual) %>% 
  summarise(a = hms::hms(seconds_to_period(mean(ride_length)))) %>% 
  rename_at("a", ~ "Average ride_length")
```

```{r}
# average ride_length for users by day_of_week
all_trips_cleaned %>% 
  group_by(day_of_week) %>% 
  summarise(a = hms::hms(seconds_to_period(mean(ride_length)))) %>% 
  rename_at("a", ~ "Average ride_length")
```

```{r}
# number of rides for users by day_of_week
all_trips_cleaned %>% 
  group_by(ride_id, day_of_week) %>% 
  summarise(number_of_rides=n())
```

```{r}
#  average ride time by each day for members vs casual users
aggregate(all_trips_cleaned$ride_length ~ all_trips_cleaned$member_casual + all_trips_cleaned$day_of_week, FUN = mean)
```

```{r}
# analyze ridership data by type and weekday
all_trips_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)
```

```{r visualize 1}
# visualize number of rides by rider type
all_trips_cleaned %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Number of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Users") +
  ylab("Number of Rides") +
  xlab("Day of Week")
```
```{r visualize 02}
all_trips_cleaned %>% 
mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarize(average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#CC6633","#6699CC")) +
  labs(title = "Average Duration of Rides by Days and Rider Type",
       subtitle = "Members versus Casual Users") +
  ylab("Average Duration of Rides") +
  xlab("Day of Week")
```

```{r }
# average ride_length by type and of week
counts <- aggregate(all_trips_cleaned$ride_length ~ all_trips_cleaned$member_casual + all_trips_cleaned$day_of_week, FUN=mean)
```

```{r}
write.csv(counts, file = 'avg_ride-length.csv')
```
```{r}
# dataset for visualization on Tableau
divvy_trips <- all_trips_cleaned %>% 
  select(-day_of_week)

divvy_trips$day_of_week <- wday(divvy_trips$started_at, label = TRUE)

write.csv(alltrips, file = "all_trips.csv", row.names = FALSE)
```



