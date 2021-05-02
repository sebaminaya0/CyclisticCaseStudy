#Unpack all needed datasets

library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)

#Generate a single Data Frame with all the data

for (data in list.files()){
  
  # Create the first data if no data exist yet
  if (!exists("dataset")){
    dataset <- read.csv(data, header=TRUE)
  }
  
  # if data already exist, then append it together
  if (exists("dataset")){
    tempory <-read.csv(data, header=TRUE)
    dataset <-unique(rbind(dataset, tempory))
    rm(tempory)
  }
}

#Check the Data Frame

glimpse(dataset)

View(head(dataset))

skim_without_charts(dataset)

#Figured we need to use new package geosphere

install.packages("geosphere")

library(geosphere)

#Create time difference (tdiff) and start/end same station (ss)

dataset <- dataset %>% 
  rename(user_status = member_casual) %>% 
  mutate(tdiff = difftime(started_at,ended_at,units = "mins")) %>% 
  mutate(ss = ifelse(start_station_id == end_station_id, 1, 0))

View(dataset)

#Error in columns started_at and ended_at on part of the data

dummy_pos <- dataset %>% 
  filter(tdiff > 0) %>% 
  relocate(started_at, .after = ended_at) %>% 
  rename(dummy = ended_at) %>% 
  rename(ended_at = started_at) %>% 
  rename(started_at = dummy)%>% 
  mutate(tdiff = difftime(started_at,ended_at,units = "mins"))

dummy_neg <- dataset %>% 
  filter(tdiff < 0)

dataset <- rbind(dummy_neg,dummy_pos)

dataset <- dataset %>% 
  mutate(tdiff = -as.numeric(tdiff, units = "mins"))

#Create distance traveled (distance) with distm() function

distance = c()

i <- 1

for (i in 1:nrow(dataset)) {
  
  distance[i] <- distm(c(dataset[i,9],dataset[i,10]), c(dataset[i,11], dataset[i,12]), fun = distHaversine)
  
}

dataset <- cbind(dataset,distance)

dataset <- dataset %>% 
  mutate(weekday = wday(started_at,label = TRUE))

#Check transformation result

View(dataset)

#Analysis of whole dataset

dataset %>% 
  group_by(user_status) %>% 
  summarize(mean_dist = mean(distance),
            max_dist = max(distance),
            mean_time = mean(tdiff),
            max_time = max(tdiff),
            min_time = min(tdiff))

#We take six random samples to further analysis as the computation
#is not powerful enough

#For 3346217 obs., a 95% confidence level
# and 5% margin of error we sample 385 obs. (https://www.surveymonkey.com/mp/sample-size-calculator/)

sample_1 <- sample_n(dataset, 385)

sample_1 <- sample_1 %>% 
  mutate(sample = 1)

sample_2 <- sample_n(dataset, 385)

sample_2 <- sample_2 %>% 
  mutate(sample = 2)

sample_3 <- sample_n(dataset, 385)

sample_3 <- sample_3 %>% 
  mutate(sample = 3)

sample_4 <- sample_n(dataset, 385)

sample_4 <- sample_4 %>% 
  mutate(sample = 4)

sample_5 <- sample_n(dataset,385)

sample_5 <- sample_5 %>% 
  mutate(sample = 5)

sample_6 <- sample_n(dataset,385)

sample_6 <- sample_6 %>% 
  mutate(sample = 6)

sampled_data <- rbind(sample_1,sample_2,sample_3,sample_4,sample_5,sample_6)

summary(sampled_data)

sampled_data <- distinct(sampled_data)

aggregate(sampled_data$tdiff~sampled_data$user_status, FUN = mean)
aggregate(sampled_data$tdiff~sampled_data$user_status, FUN = median)
aggregate(sampled_data$tdiff~sampled_data$user_status, FUN = max)
aggregate(sampled_data$tdiff~sampled_data$user_status, FUN = min)

aggregate(sampled_data$tdiff~sampled_data$user_status + sampled_data$weekday, FUN = mean)

sampled_data %>% 
  group_by(user_status, weekday) %>% 
  summarise(total_rides = n(),
            average_duration = mean(tdiff),
            average_distance = mean(distance)) %>% 
  arrange(user_status,weekday) 

sampled_data %>% 
  group_by(user_status, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(tdiff),
            average_distance = mean(distance)) %>% 
  arrange(user_status,weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = user_status)) +
  geom_col(position = "dodge")

sampled_data %>% 
  group_by(user_status, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(tdiff),
            average_distance = mean(distance)) %>% 
  arrange(user_status,weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = user_status)) +
  geom_col(position = "dodge")

sampled_data %>% 
  group_by(user_status, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(tdiff),
            average_distance = mean(distance)) %>% 
  arrange(user_status,weekday) %>% 
  ggplot(aes(x = weekday, y = average_distance, fill = user_status)) +
  geom_col(position = "dodge")

sampled_data %>% 
  group_by(ss, rideable_type) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(tdiff),
            average_distance = mean(distance)) %>% 
  arrange(ss,rideable_type) %>% 
  ggplot(aes(x = ss, y = average_distance, fill = rideable_type)) +
  geom_col(position = "dodge")

ggplot(data = sampled_data) +
  geom_col(mapping = 
             aes(x = ss,
                 y = tdiff,
                 fill = user_status), 
           position = "dodge") +
  facet_wrap(~rideable_type)

niche <- dataset %>% 
  filter(ss == 1) %>% 
  filter(rideable_type == "docked_bike")

View(niche)
