# Reproducible Research: Peer Assessment 1
Hartwig Tödter  

## Introduction

In this assessment the usage of knitr should be practised. On a relative simple
dataset several calcuation shall be performed an a couple of graphics shall be
plotted. Together with the executed code an HTML document hsal be generated 
which clearly explains the steps undertaken and the results received.

## Data

The data for this assignment includes the number of steps taken in each 5 minute
interval during October and November 2012. The variables are:

- steps: Number of steps in a 5-minute interval
- date: The date on which the steps were counted
- interval: Identifier of the 5-minute interval on the recorded date

## Loading and preprocessing the data

The data is loaded and the date factor is converted into a date.


```r
unzip("activity.zip")
activities <- read.csv("activity.csv")
activities$date <- as.Date(activities$date)
```

## What is mean total number of steps taken per day?

For the histogram the data is grouped by date and the steps taken are 
summarized for each date ignoring those intervals where no values are available.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
act1 <- activities %>% 
    filter(!is.na(steps)) %>%
    group_by(date) %>% 
    summarize(stepsTaken = sum(steps))
hist(act1$stepsTaken, 
     breaks=10,
     main="Histogram of total numbers of steps taken per day", 
     xlab="numbers of steps taken a day", 
     ylab="Frequency") 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


```r
m1 <- mean(act1$stepsTaken)
med1 <- median(act1$stepsTaken)
```

The mean steps taken per day is 1.0766189\times 10^{4}. 
The median of steps taken per day is 10765.

## What is the average daily activity pattern?
Group by interval and calculate the average number of steps taken in each 
5-minute interval than plot the average daily activity pattern as a line 
diagram.

```r
library(dplyr)
act2 <- activities %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(mean = mean(steps))
plot(act2$interval, act2$mean, type='l', 
    xlab="5-Minute Interval",
    ylab="Mean Of Steps Taken",
    main="Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

From the maximum average steps counted the corespondong interval can be derived.
Some string conversions are done to report at what time the test person is usually most active during a day.


```r
maxSteps <- max(act2$mean)
index <- which(act2$mean == maxSteps)
interval <- act2[index,]$interval
strInterval <- sprintf("%04i", interval)
maxTime <- paste(substr(strInterval, 1, 2), ":", 
                 substr(strInterval, 3, 4), ":00", sep="")
```

The interval 835 with an average of 206.1698113 steps is the interval
with the maximum number of steps counted. That means that usually the maximum activity 
is performed at 08:35:00.

## Imputing missing values


```r
missingValues <- nrow(activities[is.na(activities$steps), ])
```

There are 2304 missing values that were omitted in the previous
calculations. To fill in the missing values for each intervall the average steps
counted for that interval is used.


```r
library(dplyr)
actFull <- merge(activities, act2)
actFull$steps[is.na(actFull$steps)] <- round(actFull$mean[is.na(actFull$steps)])
actFull <- arrange(actFull[c("steps", "date", "interval")], date, interval)
```

With these added values an new histogram is plotted:


```r
library(dplyr)
act3 <- actFull %>% 
    group_by(date) %>% 
    summarize(stepsTaken = sum(steps))
hist(act3$stepsTaken, 
     breaks=10,
     main="Histogram including filled missing values", 
     xlab="numbers of steps taken a day", 
     ylab="Frequency") 
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
m2 <- mean(act3$stepsTaken)
med2 <- median(act3$stepsTaken)
```

The shape of the new histogram does not differ much from the original one. 
This is evident because there were only intervals for eight days added 
with the average values from the other 53 days.

The mean steps taken per day of the completed data frame is 1.076564\times 10^{4}. 
The median steps taken per day of the completed data frame is 1.0762\times 10^{4}.

The new mean differs by -0.55 from the old mean. The new median differs by
-3 from the old median.

## Are there differences in activity patterns between weekdays and weekends?

For the comparison of workdays and weekends two sets are filtered one including
the workdays and the other including the weekends. Both are grouped by 5-minute
interals and the means of steps are calculated.


```r
library(dplyr)

actWD <- activities %>%
    filter(!is.na(steps) & 
                      !(weekdays(as.POSIXlt(date)) == "Samstag" | 
                        weekdays(as.POSIXlt(date)) == "Sonntag")) %>%
    group_by(interval) %>%
    summarize(mean = mean(steps))

actWE <- activities %>%
    filter(!is.na(steps) & 
                      (weekdays(as.POSIXlt(date)) == "Samstag" | 
                       weekdays(as.POSIXlt(date)) == "Sonntag")) %>%
    group_by(interval) %>%
    summarize(mean = mean(steps))

par(mfrow = c(2,1))
plot(actWD$interval, actWD$mean, type='l', 
    main="Average Activity Pattern on Workdays", xlab="", ylab="Frequency")
plot(actWE$interval, actWE$mean, type='l', 
    main="Average Daily Activity Pattern on Weekends", 
    xlab="Steps per 5-minute interval", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
stepsPerWorkday <- round(mean(actWD$mean) * 288)
stepsPerWeekendday <- round(mean(actWE$mean) * 288)
```
On weekends the activity is significant higher than on workdays. The test person
usually performs 2230 more steps on a Saturday
or Sunday than on a normal workday.
