
## Loading and preprocessing the data
library(readr)
library(ggplot2)
library(knitr)
library(dplyr)
activity <- read_csv("activity.csv")



## What is mean total number of steps taken per day?
stepsaday <- aggregate(steps ~ date, activity, sum, na.rm=TRUE)

hist(stepsaday$steps, col = "lightpink", main = "Steps per Day", xlab = "Steps")

stepsadayMean <- mean(stepsaday$steps)
stepsadayMean


stepsadayMedian <- median(stepsaday$steps)
stepsadayMedian


## What is the average daily activity pattern?

stepsaninterval<-aggregate(steps~interval, data=activity, mean,na.rm=TRUE)

plot(steps~interval, data=stepsaninterval, type="l", main = "Steps per 5 Minute Interval", col = "darkgreen")

MostSteps <- stepsaninterval[which.max(stepsaninterval$steps),]$interval
MostSteps

## Imputing missing values
stepsadayNoNA<- activity %>% 
  replace(is.na(.), 0)

stepsaday2 <- aggregate(steps ~ date, stepsadayNoNA, sum, na.rm=TRUE)
hist(stepsaday2$steps, main = "Adjusted Steps per Day", col = "pink", xlab = "Steps")
stepsaday2Mean <- mean(stepsaday2$steps)
stepsaday2Median <- median(stepsaday2$steps)
stepsaday2Mean
stepsaday2Median

## Are there differences in activity patterns between weekdays and weekends?


# Create variable with date in correct format
stepsadayNoNA$RealDate <- as.Date(stepsadayNoNA$date, format = "%Y-%m-%d")
# create a variable with weekdays name
stepsadayNoNA$weekday <- weekdays(stepsadayNoNA$RealDate)
# create a new variable indicating weekday or weekend
stepsadayNoNA$DayType <- ifelse(stepsadayNoNA$weekday=='Saturday' | stepsadayNoNA$weekday=='Sunday', 'weekend','weekday')


# create table with steps per time across weekdays or weekend days
StepsPerTimeDT <- aggregate(steps~interval+DayType,data=stepsadayNoNA, FUN=mean, na.action=na.omit)

# variable time (more comprensible for the graph axis)
StepsPerTimeDTtime <- stepsadayNoNA$interval/100
# draw the line plot
patternGraphs <- ggplot(StepsPerTimeDT, aes(interval, steps))

patternGraphs+geom_line(col= "darkgreen")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)

