---
title: "Peer-graded Assignment (Course Project 1)"
author: "Johannes Spiess"
date: "29 Dezember 2016"
output: html_document
---

## Introduction

The repo at hand contains the submission for the **Peer-graded Assignment (Course Project 1)** for the course **Reproducible Research**. 

The data is taken from an activity monitoring device that collects data at 5 minute intervals during the day.
The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken by the person.


## Loading and preprocessing the data

```{r Download and Data Processing, echo = TRUE}
knitr::opts_chunk$set(echo = TRUE)
setwd("C:\\Users\\johan\\Documents\\ReproducibleResearch\\Assignment1") # set working directory 
url2file <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" # define url to data file
data <- "step_data.zip" # define name of data file
download.file(url2file, data) # download zip file
unzip(data) # unzip downloaded file
activitydata <- read.csv("activity.csv", sep = ",") # read contained csv file

str(activitydata) # check structure of data

activitydata2 <- activitydata[which(!is.na(activitydata$steps)), ] # do away with missing values from variable 'steps'
head(activitydata2) # check result of NA cleaning

activitydata2$date <- as.Date(activitydata2$date)  # covert 'date' variable which is character to date format
class(activitydata2$date) # check result of conversion
```

## What is mean total number of steps taken per day?

**Tasks 1:** Sum the number of steps per day / **Tasks 2:** Create a histogram

```{r No. steps p. day and histogram, echo = TRUE}
steps_pd <- aggregate(steps ~ date, activitydata2, sum) # aggregate steps per day
step_mean <- mean(steps_pd$steps) # calculate mean to plot it
step_median <- median(steps_pd$steps) # calculate median to plot it
# plot 
hist(steps_pd$steps, main = "Histogram of total steps taken per day", xlab = "Total steps p. Day", ylab = "No. days", 
breaks = 10, col = "steel blue")
abline(v = mean(steps_pd$steps), lty = 1, lwd = 2, col = "grey")
abline(v = median(steps_pd$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("grey", "black"), 
lty = c(2, 1), lwd = c(2, 2))
```

<img src="https://github.com/SpiessJ/RepData_PeerAssessment1/blob/master/instructions_fig/TotStepspDay.jpeg" width="700">

**Tasks 3:** Calculate mean and median of steps per day

```{r Calculate and print results for `step_mean` and `step_median`, echo = TRUE}
step_mean <- mean(steps_pd$steps) # calculate mean
step_median <- median(steps_pd$steps) # calculate median
step_mean
step_median
```

**Answer**: The **mean** `(step_mean)` is **10766.19** and the `(step_median)` is **10765**.

## What is the average daily activity pattern?

**Task 1:** Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,  averaged across all days (y-axis)

```{r No. Steps p. interavl, echo = TRUE}
steps_per_interval <- aggregate(steps ~ interval, activitydata2, mean) # aggregate steps per interval
# plot
plot(steps_per_interval$interval,steps_per_interval$steps, type="l", xlab="Interval", ylab="No. Steps",main="Avg No. Steps per Day per Interval", col = "steel blue") 
```

<img src="https://github.com/SpiessJ/RepData_PeerAssessment1/blob/master/instructions_fig/AvgNoStepspI.jpeg" width="700">

**Task 2:** Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r Print results maximum interval, echo = TRUE}
maximum_interval <- steps_per_interval[which.max(steps_per_interval$steps),1] # Calculate max. 5 minute interval
maximum_interval
```

**Answer**: The **maximum 5-minute interavl** `(maximum_interval)` is at **835**.

## Imputing missing values

**Task 1:** Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r Calculate total number of missing values in the dataset, echo = TRUE}
tot_MV_dataset <- sum(is.na(activitydata$steps)) # Calculate no. missing values
tot_MV_dataset
```

**Answer**: The total number of missing values in the dataset is **2.304**.

**Task 2:** Devise a strategy for filling in all of the missing values in the dataset. 
**Task 3:**  Create a new dataset that is equal to the original dataset but with the missing data filled in.

The chosen strategy is to impute missing values using mean for each day. A new dataset is created based upon this approach.

```{r Strategy to impute and create new datase , echo = TRUE}
activitydata3 <- activitydata # load original data
nas<- is.na(activitydata3$steps) # define vector for missing values
avg_interval<- tapply(activitydata3$steps, activitydata3$interval, mean, na.rm=TRUE, simplify = TRUE) # replace missing values
activitydata3$steps[nas] <- avg_interval[as.character(activitydata3$interval[nas])] 
```

**Task 4:** Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```{r median, mean and histogram , echo = TRUE}
steps_pd2 <- aggregate(steps ~ date, activitydata3, sum) # aggregate steps per day
step_mean2 <- mean(steps_pd2$steps)  # calculate mean to plot it
step_median2 <- median(steps_pd2$steps)  # calculate median to plot it
# plot
hist(steps_pd2$steps, main = "Histogram of total steps taken per day", 
xlab = "Total steps p. Day", ylab = "No. days", 
breaks = 10, col = "steel blue")
abline(v = mean(steps_pd2$steps), lty = 1, lwd = 2, col = "grey")
abline(v = median(steps_pd2$steps), lty = 2, lwd = 2, col = "black")
legend(x = "topright", c("Mean", "Median"), col = c("grey", "black"), 
       lty = c(2, 1), lwd = c(2, 2))

```
<img src="https://github.com/SpiessJ/RepData_PeerAssessment1/blob/master/instructions_fig/TotStepspDay2.jpeg" width="700">

**Answer**: The new mean is at 10.766. The median takes exactly the same value. Before data was imputed, the mean was at 10.766 and the median was slighlty lower at 10.765. As we use means to replace missing values, we end up with more data close or identical to the mean. As a consequence of that, the median is shifted towards the mean. 
The impact of imputing missing data on the estimates of the total daily number of steps is that we increase frequence counts as we add values to records that did not have values for sthe step-variable before (as they were missing.)

## Are there differences in activity patterns between weekdays and weekends?

```{r differences in patterns weekday vs. weekend, echo = TRUE}
library(dplyr)
activitydata3$date <- as.Date(activitydata3$date) # convert to date
activitydata3 <- activitydata3 %>% mutate(weektype= ifelse(weekdays(activitydata3$date)=="Samstag" | weekdays(activitydata3$date)=="Sonntag", "Weekend", "Weekday")) # classify weekends and weekdays
# plot
library(lattice)
xyplot(steps ~ interval | factor(weektype),
layout = c(1, 2),
xlab="Interval",
ylab="Number of steps",
type="l",
lty=3,
data=activitydata3)
```

<img src="https://github.com/SpiessJ/RepData_PeerAssessment1/blob/master/instructions_fig/WeekendWeekdayComp.jpeg" width="700">


**Answer**: The test person seems to be active at an earlier time of the day on weekdays. Activity levels show a rightward shift on weekends which implies that the person starts being active later. This shift might be due to the fact that the person is working during the week.



