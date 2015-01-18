---
title: 'Reproducible Research: Peer Assessment 1'
author: "Nikki Sutherland"
date: "January 17, 2015"
output:
  html_document:
    keep_md: yes
---

Reproducible Research: Peer Assessment 1
========================================

**Author:** Nikki Sutherland  
**Date:** January 17, 2015

## Introduction

This assignment takes activity data from a personal activity monitoring device 
and performs some basic analysis on the steps taken per 5-minute interval for 
a 2-month period.

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing values are
    coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken
    
The interval is essentially the given time of day. For example, 955 is 9:55 AM
and 1810 corresponds to 6:10 PM.

### Loading and Preprocessing the Data
  
Load the packages to be used for analysis:


```r
library(plyr)
library(knitr)
library(lattice)
```

  
Create a "data" directory and then load the data:


```r
if (!file.exists("./data")) { dir.create("./data") }
activity_file <- "activity.zip"
unzip(activity_file, overwrite = TRUE, list = FALSE, junkpaths = TRUE, 
      exdir = "./data", unzip = "internal", setTimes = FALSE)
activity_data <- read.csv("./data/activity.csv", colClasses = c("numeric", "character", "numeric"))
```


The activity data looks like this:


```r
head(activity_data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
tail(activity_data)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
summary(activity_data)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```


### Mean Total Number of Steps Taken per Day

Use ddply to get the total steps for each day:

```r
total_steps <- ddply(activity_data, .(date), summarize,
                     Total_steps=sum(steps, na.rm = TRUE))
head(total_steps)
```

```
##         date Total_steps
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

```r
tail(total_steps)
```

```
##          date Total_steps
## 56 2012-11-25       11834
## 57 2012-11-26       11162
## 58 2012-11-27       13646
## 59 2012-11-28       10183
## 60 2012-11-29        7047
## 61 2012-11-30           0
```

```r
summary(total_steps)
```

```
##      date            Total_steps   
##  Length:61          Min.   :    0  
##  Class :character   1st Qu.: 6778  
##  Mode  :character   Median :10395  
##                     Mean   : 9354  
##                     3rd Qu.:12811  
##                     Max.   :21194
```

Create a histogram of the total number of steps taken each day:


```r
hist(total_steps$Total_steps, 
     main = "Total Steps Taken per Day", 
     xlab = "Total Steps")
```

![plot of chunk total_daily_steps](figure/total_daily_steps-1.png) 

Then take the mean and median total number of steps taken per day:

```r
mean(total_steps$Total_steps)
```

```
## [1] 9354.23
```

```r
median(total_steps$Total_steps)
```

```
## [1] 10395
```


### Average Daily Activity Pattern

Use ddply to calculate the average number of steps taken at each interval:

```r
average_steps_interval <- ddply(activity_data, .(interval), summarize,
                                Average_steps=mean(steps, na.rm = TRUE))
head(average_steps_interval)
```

```
##   interval Average_steps
## 1        0     1.7169811
## 2        5     0.3396226
## 3       10     0.1320755
## 4       15     0.1509434
## 5       20     0.0754717
## 6       25     2.0943396
```

```r
tail(average_steps_interval)
```

```
##     interval Average_steps
## 283     2330     2.6037736
## 284     2335     4.6981132
## 285     2340     3.3018868
## 286     2345     0.6415094
## 287     2350     0.2264151
## 288     2355     1.0754717
```

```r
summary(average_steps_interval)
```

```
##     interval      Average_steps    
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

Create a time series plot of the 5-minute interval and the average number of 
steps taken, averaged across all days:

```r
plot(average_steps_interval$interval, average_steps_interval$Average_steps, 
     type = "l", main = "Average Steps Taken per 5-minute Interval",
     xlab = "Interval", ylab = "Average Steps Taken")
```

![plot of chunk average_interval_steps](figure/average_interval_steps-1.png) 

Determine which 5-minute interval, on average across all the days in the 
dataset,contains the maximum number of steps:

```r
max_steps <- which.max(as.double(average_steps_interval$Average_steps))
average_steps_interval[max_steps, "interval"]
```

```
## [1] 835
```

So, on average, 8:35 AM has the most steps taken.

### Imputing Missing Values

Calculate and report the total number of missing values in the dataset:


```r
nas <- activity_data[is.na(activity_data$steps),]
number_nas <- nrow(nas)
number_nas
```

```
## [1] 2304
```

There are 2,304 observations for steps that have NA values.  

To create a more complete data set, the average step value from the **average_steps_interval** 
data frame will replace an NA value for the corresponding interval. For example,
the first observation in the data set, on 2012-10-01, at interval 0 is NA:

```r
head(activity_data, n=1)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
```

It will be replaced with the average number of steps taken at that interval:

```r
head(average_steps_interval, n=1)
```

```
##   interval Average_steps
## 1        0      1.716981
```

Start by making a copy of the **activity_data** data frame:

```r
complete_data <- activity_data
```

Replace the NA values in **nas** data frame:

```r
for (i in 1:number_nas) {
    nas[i, "steps"] <- average_steps_interval[which(average_steps_interval$interval == nas[i, "interval"]), "Average_steps"]
}
head(nas)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
tail(nas)
```

```
##           steps       date interval
## 17563 2.6037736 2012-11-30     2330
## 17564 4.6981132 2012-11-30     2335
## 17565 3.3018868 2012-11-30     2340
## 17566 0.6415094 2012-11-30     2345
## 17567 0.2264151 2012-11-30     2350
## 17568 1.0754717 2012-11-30     2355
```

Replace the specific row numbers in **complete_data** with the corresponding
row numbers from the **nas** data frame:

```r
replacement_rows <- as.numeric(row.names(nas))
for (i in replacement_rows) {
    complete_data[i, ] <- nas[which(row.names(nas)==as.character(i)), ]
}
```

Use ddply to get the total steps for each day, using the new complete data:

```r
total_steps_complete <- ddply(complete_data, .(date), summarize, 
                              Total_steps=sum(steps, na.rm = TRUE))
```

Create a new histogram of the total number of steps taken each day:

```r
hist(total_steps_complete$Total_steps, 
     main = "Total Steps Taken per Day\n(NA values replaced by average values)", 
     xlab = "Total Steps")
```

![plot of chunk total_daily_steps_complete](figure/total_daily_steps_complete-1.png) 

Then take the new mean and median total number of steps taken per day:

```r
mean(total_steps_complete$Total_steps)
```

```
## [1] 10766.19
```

```r
median(total_steps_complete$Total_steps)
```

```
## [1] 10766.19
```

### Comparing Weekday and Weekend Activity Patterns

Start by getting the number of days in the experiment:

```r
num_days <- nrow(total_steps_complete)
```


Then loop through the days and calculate which days were weekends (Saturday and Sunday):

```r
weekends <- data.frame()

for (i in 1:num_days) {    
    if ((weekdays(as.Date(total_steps_complete[i, "date"])) %in% c('Saturday','Sunday'))) {
        weekends <- rbind(weekends, as.data.frame(total_steps_complete[i, "date"]))
    }   
}

colnames(weekends)[1] <- "date"
```

Then create a factor for weekdays and weekends. Add this column to the
**complete_data** data frame and set all the days to weekdays, since only a few
days are actually weekends:

```r
day_type <- factor(c("weekday", "weekend"))
names(day_type) <- c("weekday", "weekend")

nobs <- nrow(complete_data)
day <- rep(day_type[1], nobs)
complete_data <- cbind(complete_data, day)
```

Then, update the **complete_data** data frame with the dates that are known weekends:

```r
num_weekends <- nrow(weekends)
for (i in 1:num_weekends) {
    selections <- row.names(complete_data[complete_data$date == as.character(weekends[1, "date"]), ])
    complete_data[selections, "day"] <- "weekend"
}
```

Update the average steps per interval:

```r
average_steps_complete <- ddply(complete_data, .(interval, day), summarize, Average_steps=mean(steps))
```

Create a panel plot containing a time series plot of the 5-minute interval and
the average number of steps per interval for all weekend days and all weekday days:

```r
xyplot(Average_steps ~ interval | day, data = average_steps_complete,
       type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk average_steps_day_type](figure/average_steps_day_type-1.png) 
