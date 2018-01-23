# Extract data from the archive
if(!file.exists("activity.csv"))
{
  unzip("activity.zip", files = "activity.csv")
}

# Read data from CSV-file
activity <- read.csv("activity.csv", colClasses = c("integer", "character", "integer"))

# Convert data in 'date' column to Date class
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

summary(activity)

# Convert to dplyr table
library(dplyr)
activity <- tbl_df(activity)

# Calculate the total number of steps taken per day
stepsPerDay <- activity %>%
  group_by(date) %>%
  summarise(stepsPerDay = sum(steps, na.rm = TRUE))

# Plot histogramm
hist(stepsPerDay$stepsPerDay, 
     breaks = 10,
     xlab = "Steps per day", 
     main = "Histogram of steps per day")

stepsPerInterval <- activity %>%
  group_by(interval) %>%
  summarise(averageStepsPerInterval = mean(steps, na.rm = TRUE))

# activityWithoutNA <- activity
# 
# for(index in which(is.na(activityWithoutNA$steps)))
# {
#   interval <- activityWithoutNA[index,]$interval
#   activityWithoutNA[index,]$steps <- stepsPerInterval[stepsPerInterval$interval == interval,]$averageStepsPerInterval
# }

activityWithWeekDay <- activity %>%
  mutate(wday = recode_factor(as.POSIXlt(date)$wday, 
                          `0` = "weekend", 
                          `1` = "weekday", 
                          `2` = "weekday", 
                          `3` = "weekday", 
                          `4` = "weekday", 
                          `5` = "weekday", 
                          `6` = "weekend"))

activityOnWeekend <- activityWithWeekDay %>%
  filter(wday == "weekend") %>%
  group_by(interval) %>%
  summarise(averageStepsPerInterval = mean(steps, na.rm = TRUE))

activityOnWorkDay <- activityWithWeekDay %>%
  filter(wday == "weekday") %>%
  group_by(interval) %>%
  summarise(averageStepsPerInterval = mean(steps, na.rm = TRUE))

par(mfrow = c(2, 1))

plot(activityOnWeekend$interval, activityOnWeekend$averageStepsPerInterval, type = "l")

plot(activityOnWorkDay$interval, activityOnWorkDay$averageStepsPerInterval, type = "l")