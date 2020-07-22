---
title: "Reproducible_Research_Project1"
author: "me"
date: "22/07/2020"
output: html_document
---
# Assignment Instructions
1.Code for reading in the dataset and/or processing the data  
2.Histogram of the total number of steps taken each day  
3.Mean and median number of steps taken each day  
4.Time series plot of the average number of steps taken  
5.The 5-minute interval that, on average, contains the maximum number of steps  
6.Code to describe and show a strategy for imputing missing data  
7.Histogram of the total number of steps taken each day after missing values are imputed  
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends  
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report  

# Loading and Preprocessing the Data

```{r, echo = TRUE}
getwd()

#Listing all the files in the current working directory

list.files()


#Reading the data

activity <- read.csv("activity.csv")
```

Exploring the basics of this data
```{r}
dim(activity)
names(activity)
head(activity)
str(activity)
```

# What is mean total number of steps taken per day?
```{r, echo = TRUE}
#calculate the total number of steps taken per day
stepsPerDay <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)

#make a histogram of the total number of steps taken each day
hist(stepsPerDay$steps)
```
calculate and report the mean and median of the total number of steps taken each day
```{r, echo = TRUE}
meanStepsPerDay <- mean(stepsPerDay$steps)
meanStepsPerDay
medianStepsPerDay <- median(stepsPerDay$steps)
medianStepsPerDay
```

* The mean total number of steps taken each day is stored in variable *meanStepsPerDay*

* The medina total number of steps taken each day is stores in variable *medianStepsPerDay*

# What is the average daily activity pattern?

1. Make a time series plot(i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r, echo = TRUE}
stepsPerInterval<-aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
plot(steps~interval, data=stepsPerInterval, type="l")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo = TRUE}
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
intervalWithMaxNbSteps
```

* The 5-minute interval across all the days containing the maximum number of steps is stored in variable intervalWithMaxNbSteps


# Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r, echo = TRUE}
totalValuesMissings <- sum(is.na(activity$steps))
totalValuesMissings
```

* The total number of missing values in the dataset is stored in the variable totalValuesMissings

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy : we’ll fill in all the missing values in the dataset with the mean per interval. Here’s the function that will return, for a particular interval, the mean value

```{r, echo = TRUE}
getMeanStepsPerInterval<-function(interval){
    stepsPerInterval[stepsPerInterval$interval==interval,]$steps
}

#creating a new dataset that is equal to the original dataset but with the missing data  filled in
activityDataNoNA<-activity
for(i in 1:nrow(activityDataNoNA)){
    if(is.na(activityDataNoNA[i,]$steps)){
        activityDataNoNA[i,]$steps <- getMeanStepsPerInterval(activityDataNoNA[i,]$interval)
    }
}
```
* The new data set with no missing values is contained in the variable activityDataNoNA

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r, echo = TRUE}
totalStepsPerDayNoNA <- aggregate(steps ~ date, data=activityDataNoNA, sum)
hist(totalStepsPerDayNoNA$steps)
```

```{r, echo = TRUE}
meanStepsPerDayNoNA <- mean(totalStepsPerDayNoNA$steps)
medianStepsPerDayNoNA <- median(totalStepsPerDayNoNA$steps)
```

* The mean total number of steps taken each day with no missing values is stored in variable **meanStepsPerDayNoNA**

* The median total number of steps taken each day with no missing values is stored in variable **medianStepsPerDayNoNA**

*The mean didn’t change after the replacements of NAs, the median changed about 0.1% of the original value.*

#Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r, echo = TRUE}
activityDataNoNA$date <- as.Date(strptime(activityDataNoNA$date, format="%Y-%m-%d"))
activityDataNoNA$day <- weekdays(activityDataNoNA$date)
for (i in 1:nrow(activityDataNoNA)) {
    if (activityDataNoNA[i,]$day %in% c("Saturday","Sunday")) {
        activityDataNoNA[i,]$day<-"weekend"
    }
    else{
        activityDataNoNA[i,]$day<-"weekday"
    }
}
stepsByDay <- aggregate(activityDataNoNA$steps ~ activityDataNoNA$interval + activityDataNoNA$day, activityDataNoNA, mean)
```
2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```{r, echo = TRUE}
names(stepsByDay) <- c("interval", "day", "steps")
library(lattice)
xyplot(steps ~ interval | day, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```




