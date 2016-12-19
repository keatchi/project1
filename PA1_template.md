Loading and preprocessing the data
==================================

1. Load the data
----------------

2. Process/transform the data (if necessary) into a format suitable for your analysis
-------------------------------------------------------------------------------------

### Some samples of the data

    activity<-read.csv("activity.csv")
    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
=================================================

For this part of the assignment, you can ignore the missing values in the dataset.
----------------------------------------------------------------------------------

1. Calculate the total number of steps taken per day
----------------------------------------------------

### Some samples of the data

    day_step<-aggregate(steps~date,activity,sum)
    head(day_step)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    hist(day_step$steps,main="Total steps in a day", col="blue",xlab="number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

3. Calculate and report the mean and median of the total number of steps taken per day
--------------------------------------------------------------------------------------

    m1<-mean(day_step$steps)
    med1<-median(day_step$steps)

### The mean is 1.076618910^{4} and the median is 10765.

What is the average daily activity pattern?
===========================================

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
-------------------------------------------------------------------------------------------------------------------------------------

    interval_step<-aggregate(steps~interval,activity,mean)
    plot(interval_step$interval,interval_step$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
----------------------------------------------------------------------------------------------------------------

    interval_max <- interval_step[which.max(interval_step$steps),1]

### 835

Imputing missing values
=======================

Note that there are a number of days/intervals where there are missing values (NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

1. Calculate and report the total number of missing values in the dataset
-------------------------------------------------------------------------

    sum(is.na(activity$steps))

    ## [1] 2304

### Some sample of data with missing values:

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    interval_mean<-aggregate(steps~interval,activity,mean,na.rm=TRUE);
    merge_data<-merge(activity,interval_mean,by="interval");
    merge_data$steps.x[is.na(merge_data$steps.x)] <- merge_data$steps.y[is.na(merge_data$steps.x)];
    merge_data<-merge_data[ ,1:3];
    s<-sum(is.na(merge_data$steps.x))

### Using mean of interval to fill up all the missing values, the number of missing values become 0.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
--------------------------------------------------------------------------------------------------

### Some examples of the filled data

    head(merge_data)

    ##   interval  steps.x       date
    ## 1        0 1.716981 2012-10-01
    ## 2        0 0.000000 2012-11-23
    ## 3        0 0.000000 2012-10-28
    ## 4        0 0.000000 2012-11-06
    ## 5        0 0.000000 2012-11-24
    ## 6        0 0.000000 2012-11-15

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    day_merge_data<-aggregate(steps.x~date,merge_data,sum)
    hist(day_merge_data$steps.x,main="Total steps in a day with remove NA", col="blue",xlab="number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    m2 <- mean(day_merge_data$steps.x)
    med2<-median(day_merge_data$steps.x)

### The mean of the imputed data is 1.076618910^{4} and median is 1.076618910^{4}. The mean is same as the first part but the median is higher than the first part.

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
-----------------------------------------------------------------------------------------------------------------------------------------------------

    merge_data$date<-as.Date(merge_data$date, format = "%Y-%m-%d")
    weekday<-weekdays(merge_data$date)
    merge_data<-cbind(merge_data,weekday)
    merge_data<- transform(merge_data, weekday = ifelse(weekday == "Saturday"|weekday == "Sunday", "weekend", "weekday"))

### Some examples of the data with new variable:

    head(merge_data)

    ##   interval  steps.x       date weekday
    ## 1        0 1.716981 2012-10-01 weekday
    ## 2        0 0.000000 2012-11-23 weekday
    ## 3        0 0.000000 2012-10-28 weekend
    ## 4        0 0.000000 2012-11-06 weekday
    ## 5        0 0.000000 2012-11-24 weekend
    ## 6        0 0.000000 2012-11-15 weekday

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    average<-aggregate(steps.x ~ interval + weekday, merge_data, mean)
    library(ggplot2)
    ggplot(average, aes(interval, steps.x)) + geom_line() + facet_grid(weekday ~ .) + xlab("5 minutes interval") + ylab("Average of number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-14-1.png)
