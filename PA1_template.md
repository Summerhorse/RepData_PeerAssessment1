---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
Author: Rachel Foster

This is my submission for Course Project 1 of Reproducible Research.

For more information see the [README](https://github.com/Summerhorse/RepData_PeerAssessment1/blob/master/README.md) on GitHub   


## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken




## Loading and preprocessing the data
1.  Install libraries

2.  Load and preprocess **[DATA](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)**
    
    ```r
    library(readr)
    library(ggplot2)
    library(knitr)
    library(dplyr)
    ## 
    ## Attaching package: 'dplyr'
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union
    activity <- read_csv("activity.csv")
    ## Parsed with column specification:
    ## cols(
    ##   steps = col_double(),
    ##   date = col_date(format = ""),
    ##   interval = col_double()
    ## )
    ```



## What is mean total number of steps taken per day?
1. Find the total steps per day

2. Create a histogram that represents steps per day
    
3. Find the mean and median of steps per day
    
    ```r
    stepsaday <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)
    
    hist(stepsaday$steps, col = "lightpink", main = "Steps per Day", xlab = "Steps")
    ```
    
    ![](PA1_template_files/figure-html/stepsaday-1.png)<!-- -->
    
    ```r
    
    stepsadayMean <- mean(stepsaday$steps)
    stepsadayMean
    ## [1] 10766.19
    
    
    stepsadayMedian <- median(stepsaday$steps)
    stepsadayMedian
    ## [1] 10765
    ```

**The subject's mean steps a day is 1.0766189\times 10^{4}.**

**The subject's median steps a day is 1.0765\times 10^{4}.**



## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    
    ```r
    stepsaninterval<-aggregate(steps~interval, data=activity, mean,na.rm=TRUE)
    
    plot(steps~interval, data=stepsaninterval, type="l", main = "Steps per 5 Minute Interval", col = "darkgreen")
    ```
    
    ![](PA1_template_files/figure-html/moststeps-1.png)<!-- -->
    
    ```r
    
    MostSteps <- stepsaninterval[which.max(stepsaninterval$steps),]$interval
    ```

**Interval number 835 contains the maximum number of steps.**

 
 
## Imputing missing values

Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
    
    
    ```r
    stepsadayNoNA<- activity %>% 
                replace(is.na(.), 0)
    
    stepsaday2 <- aggregate(steps ~ date, stepsadayNoNA, sum, na.rm=TRUE)
    hist(stepsaday2$steps, main = "Adjusted Steps per Day", col = "pink", xlab = "Steps")
    ```
    
    ![](PA1_template_files/figure-html/na-1.png)<!-- -->
    
    ```r
    stepsaday2Mean <- mean(stepsaday2$steps)
    stepsaday2Median <- median(stepsaday2$steps)
    stepsaday2Mean
    ## [1] 9354.23
    stepsaday2Median
    ## [1] 10395
    ```

**The mean of the subject's steps per day imputing for NA variables is 9354.2295082 and the median is 1.0395\times 10^{4}.**



## Are there differences in activity patterns between weekdays and weekends?

Using the dataset with the filled-in missing values for this part:

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

    
    ```r
     
    stepsadayNoNA$RealDate <- as.Date(stepsadayNoNA$date, format = "%Y-%m-%d")
    stepsadayNoNA$weekday <- weekdays(stepsadayNoNA$RealDate)
    stepsadayNoNA$DayType <- ifelse(stepsadayNoNA$weekday=='Saturday' | stepsadayNoNA$weekday=='Sunday', 'weekend','weekday')
    StepsPerTimeDT <- aggregate(steps~interval+DayType,data=stepsadayNoNA, FUN=mean, na.action=na.omit)
    StepsPerTimeDTtime <- stepsadayNoNA$interval/100
    patternGraphs <- ggplot(StepsPerTimeDT, aes(interval, steps))
    patternGraphs+geom_line(col= "darkgreen")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
    ```
    
    ![](PA1_template_files/figure-html/weekdays-1.png)<!-- -->
    
