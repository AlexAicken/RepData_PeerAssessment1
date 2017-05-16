# Course Project 1

# RepData - Peer Assignment 1

## Overview
This analysis looks at the number of steps per day from wearable devices - see README.MD in the github repo for more information  

## Load libraries

Libraries needed for analysis  


```r
library(plyr)
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:plyr':
## 
##     is.discrete, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(lattice)
setwd("C:/Users/Alex/Documents/KNITR")
```

## Read in CSV data

Data downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip  
Data unzipped and loaded from directory below  


```r
dataActivity <- read.csv("activity.csv", header=TRUE)
```

## Histogram of Number of Steps per Day

Work out sum of steps for each date - plot on histogram  


```r
stepsPerDay <- aggregate(steps~date,data=dataActivity, sum)
hist(x=stepsPerDay$steps, xlab="Steps per Day", main="Histogram of Steps per Day")
```

![](PA1_Template_files/figure-html/stepsperday-1.png)<!-- -->

## Calculation of Mean and Median Steps

Calculate the mean, median and total steps per day  


```r
meanSteps <- mean(stepsPerDay$steps)
medianSteps <- median(stepsPerDay$steps)
totalSteps <- sum(stepsPerDay$steps)
```
The mean steps per day is 1.0766189\times 10^{4}  
The median steps per day is 10765  
The total number of steps per day is 570608  

## Plot of Steps per Interval  

Calculate and plot the average steps per interval  


```r
stepsPerInterval <- aggregate(steps~interval,data=dataActivity, FUN=mean)
plot(steps ~ interval, xlab="Interval", ylab="Steps", main="Average Steps per Interval", data=stepsPerInterval, type="l")
```

![](PA1_Template_files/figure-html/stepsperinterval-1.png)<!-- -->

## Interval that contains the maximum number of steps  

Find the interval with the maximum number of steps  


```r
intervalMaxSteps <- stepsPerInterval[stepsPerInterval$steps >= max(stepsPerInterval$steps), 1]
```
The interval with the max steps is 835

## Number of NAs  

Calculate the number of records with NA in the data  


```r
numberOfNAs <- sum(is.na(dataActivity$steps))
```
The number of NAs in the data is 2304

## Impute on NAs - Create new Imputed data set for NAs

Save the mean when there is an NA - use impute function to do this


```r
dataActivityImputed <- dataActivity
dataActivityImputed$stepsImputed <- with(dataActivity,impute(dataActivity$steps, mean))
dataActivityImputed$steps <- dataActivityImputed$stepsImputed
```

## Make new histogram off Imputed data set

calculate and plot the __imputed__ steps per day


```r
stepsPerDayImputed <- aggregate(steps~date,data=dataActivityImputed, sum)
hist(x=stepsPerDayImputed$steps, xlab="Steps per Day", main="Histogram of Steps per Day, with Imputed Values")
```

![](PA1_Template_files/figure-html/stepsperdayimpute-1.png)<!-- -->
Note that this looks very close to previous plot with NAs

## Mean and Median on Imputed data set - Comparison

Calculate the mean, median and total steps


```r
meanStepsImputed <- mean(stepsPerDayImputed$steps)
medianStepsImputed <- median(stepsPerDayImputed$steps)
totalStepsImputed <- sum(stepsPerDayImputed$steps)
```

## Comparing the results  
__Mean data the same__  
Mean Imputed Data 1.0766189\times 10^{4}, Mean Non-imputed Data 1.0766189\times 10^{4}  
__Median data increased slightly__  
Median Imputed Data 1.0766189\times 10^{4}, Median Non-imputed Data 10765  
__Total steps increased significantly__  
Total Steps Imputed Data 6.5673751\times 10^{5}, Total Steps Non-imputed Data 570608  


# Add Weekday and Weekend Indicators

Add weekday, weekend indicators


```r
dataActivityImputed$Weekday <- weekdays(as.Date(dataActivityImputed$date))
dataActivityImputed$IsWeekend <- grepl('Saturday|Sunday', dataActivityImputed$Weekday)
dataActivityImputed$IsWeekday <- !grepl('Saturday|Sunday', dataActivityImputed$Weekday)
dataActivityImputed$WeekdayStatus <- ifelse(dataActivityImputed$IsWeekday==TRUE,"weekday","weekend")
```

# Panel plot - Weekday Steps vs Weekend Steps
Panel plot the average number of steps per interval, splitting by whether it is a weekend or a weekday  


```r
stepsPerIntervalImputed <- aggregate(steps ~ interval + WeekdayStatus,data=dataActivityImputed, FUN=sum)
xyplot(steps~interval|factor(WeekdayStatus),type="l",layout=c(1,2), xlab="Interval",ylab="Number of Steps", data=stepsPerIntervalImputed)
```

![](PA1_Template_files/figure-html/weekdaysteps-1.png)<!-- -->
