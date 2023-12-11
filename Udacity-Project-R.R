## Bike Share Data Analysis Project ##
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(RColorBrewer)
library(tidyverse)


getwd()
setwd("c:/Users/hpatel7/OneDrive - JNJ/Desktop/Data Sciences Training/Udacity/Programing for Data Scienc in R/Project-R")
list.files()
ny <- read.csv("new-york-city.csv")
wash <- read.csv("washington.csv")
chi <- read.csv("chicago.csv")

head(ny)
names(ny)
summary(ny)

head(wash)
names(wash)
summary(wash)

head(chi)
names(chi)
summary(chi)


###### Question 1 #####
##**What is the most common month?**##

# Extracting End.Time from all three files and combine in a new Data Frame.

city <- c("ny", "chi", "wash")  # creating a city variable for use in loops.
trip_end_date <- data.frame() # Create an empty data frame to store the combined column

for (x in city) {
  df<-get(x) # Retrieve the data frame using the variable value
  end_time_col <- select(df, End.Time)  # Select the End.Time column
  trip_end_date <- bind_rows(trip_end_date, end_time_col)  # Combine the column with the existing data frame
}
dim(trip_end_date)
head(trip_end_date)

# Separate Dates and Time and cover to Date and Time format.
trip_end_date$End.Time <- as.POSIXct(trip_end_date$End.Time, format = "%Y-%m-%d %H:%M:%S") #converting text to POSIXct
trip_end_date$End_Date <- as.Date(trip_end_date$End.Time) #Adding date column
trip_end_date$End_Time <- format(trip_end_date$End.Time, "%H:%M:%S") #Adding time column


#### Building a frequency chart by month #####
trip_end_date$End_Month <- as.numeric(month(trip_end_date$End_Date)) # add month column
str(trip_end_date)  #check
head(trip_end_date) #check

ggplot(trip_end_date, aes(x = End_Month)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..,), vjust = -0.5) +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", y = "Count") +
  ggtitle("Trip Count by Month")


#' Checking Data as only 7 months show up in the chart ####
#Checking # of years the data covers.
tmp <- trip_end_date
dim(tmp)
head(tmp)

ggplot(tmp, aes(x = year(End_Date))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..,), vjust = -0.5) +
  scale_x_continuous(breaks = seq(2014,2018,1))+
  labs(x = "year", y = "Count") +
  ggtitle("Chart by year")

#Checking data in 2016.
tmp <- subset(trip_end_date, year(End_Date) == "2016")
dim(tmp)
head(tmp)


#Checking for trips ending past June.
tmp <- trip_end_date
tmp <- subset(trip_end_date, month(End_Date) == "7")
dim(tmp)
head(tmp)

ggplot(tmp, aes(x = week(End_Date))) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..,), vjust = -0.5) +
  scale_x_continuous(breaks = seq(24,28,1))+
  labs(x = "Week", y = "Count") +
  ggtitle("Chart by Week month >=July")


########' Summary of Question 1 Results: ########
#' 
#' June has the most number of trips.
#' 
#' Data only has trips conducted in 2017 upto July.
#' July data is incomplete, only first week has complete data with only 3 data points in second week.
#' July data be ignored for practical purposes as there only a few data points

################################


###### Question 2 #####
##**How does the average travel time compare across the cities? **##
##*
##* Extracting Trip.Duration from all three files, adding city name and combine in a new Data Frame.

city <- c("ny", "chi", "wash")  # creating a city variable for use in loops.
trip_duration <- data.frame() # Create an empty data frames to store data
trip_duration_temp <- data.frame()
for (x in city) {
  df<-get(x) # Retrieve the data frame using the variable value
  trip_duration_temp <- as.data.frame(select(df, Trip.Duration))
  trip_duration_temp$city <- x
  trip_duration <- bind_rows(trip_duration, trip_duration_temp)  # Combine the column with the existing data frame
}
### Data Check ###
dim(trip_duration)
head(trip_duration)
table(trip_duration)


### table shows that there is probably an issue with Washington data, it has data associated with non whole seconds only, while other two cities have data for whole seconds.


# Creating a histogram

ggplot(data = trip_duration, aes(x = Trip.Duration)) +
  geom_histogram(binwidth = 30) +
  xlim(0,10000) +
    ggtitle('Histogram of Trip Duration') +
  labs(x = 'Trip Duration') +
  facet_wrap(~city)

# right-skewed with too many outliers so Zooming in....

ggplot(data = trip_duration, aes(x = Trip.Duration)) +
  geom_histogram(binwidth = 30) +
  xlim(0,10000) +
  ggtitle('Histogram of Trip Duration') +
  labs(x = 'Trip Duration') +
  facet_wrap(~city)+
  coord_cartesian(xlim=c(100,1500))


# Creating Box Plot

ggplot(data = trip_duration, aes(x = Trip.Duration, y=city, fill = city)) +
  geom_boxplot(outlier.colour = "red") +
  scale_x_continuous(breaks = seq(0,3000,100))+
  stat_summary(fun=mean, geom='point', shape=20) +
  ggtitle('Box Plot of Trip Duration') +
  labs(x = 'Trip Duration') +
  coord_cartesian(xlim=c(250,1500)) # Zooming in to boxplot


# Summary data
stats=tapply(trip_duration$Trip.Duration, trip_duration$city, summary)
stats

#Medians -> Chi = 670 sec., ny = 609 sec., wash = 706.5 seconds

########' Summary of Question 2 Results: ########
##' 
##' While the largest peak of travel time count is in NY,  Washington has the largest median travel time and therefore the longest travel time on an average
##' 
##' there are many outliers with large values, which is driving the mean higher.  Some of these could be valid as people living far wont commute as often.  However some of the vlues, for example: the max values of 23.95 hours for Chicago, 598.83 hrs for NY, 343.23 hrs are probably data errors.

################################


###### Question 3 #####
###**Which age/gender combination travels on an average the longest? (only for NYC and Chicago)**##

current_year <- year(Sys.Date())
  
##* Extracting Trip.Duration, Gender and Birth Year from ny and chicago files, adding city name and combine in a new Data Frame.
  
city <- c("ny", "chi")  # creating a city variable for use in loops.
trip <- data.frame() # Create empty data frames to store data
trip_temp <- data.frame()
for (x in city) {
  df<-get(x) # Retrieve the data frame using the variable value
  trip_temp <- as.data.frame(select(df, Trip.Duration, Gender, Birth.Year))
  trip <- bind_rows(trip, trip_temp)  # Combine the column with the existing data frame
}
  trip$Age <- (current_year - trip$Birth.Year)
head(trip)
str(trip)


## Analyzing data frame:

# Point chart with gender as color
ggplot(data = subset(trip,!is.na(trip$Gender)), aes(y = Trip.Duration, x = Age, col=Gender)) +
  geom_point(alpha=1/10, 
             position = position_jitter(h=0)
             ) +
  ggtitle('Chart of trip duration v/s age') +
  labs(x = 'Age', y = 'Trip Duration')+
  xlim(19,85) +
  ylim(0,100000)

# point chart with gender facet wrap
ggplot(data = subset(trip,!is.na(trip$Gender)), aes(y = Trip.Duration, x = Age)) +
  geom_point(alpha=1/10, 
             position = position_jitter(h=0)
  ) +
  ggtitle('Chart of trip duration v/s age') +
  labs(x = 'Age', y = 'Trip Duration')+
  xlim(19,85) +
  ylim(0,100000)+
  facet_wrap(~Gender)


# Summary data
stats=tapply(trip$Trip.Duration, trip$Gender, summary)
print(stats)


output <- capture.output(tapply(trip$Trip.Duration, trip$Gender, summary), file=NULL,append=FALSE)
output_df <- as.data.frame(output)
head(output_df)

########' Summary of Question 3 Results: ########
##' 
##' While the largest peak of travel time count is in NY,  Washington has the largest median travel time and therefore the longest travel time on an average
##' 
##' there are many outliers with large values, which is driving the mean higher.  Some of these could be valid as people living far wont commute as often.  However some of the vlues, for example: the max values of 23.95 hours for Chicago, 598.83 hrs for NY, 343.23 hrs are probably data errors.

################################