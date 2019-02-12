Code for reading in the dataset and/or processing the data
----------------------------------------------------------

    path<-"C:\\Users\\daatas\\Desktop"
    activity<-read.csv(file.path(path,"activity.csv"), header=TRUE)
    tail(activity)

    ##       steps       date interval
    ## 17563    NA 2012-11-30     2330
    ## 17564    NA 2012-11-30     2335
    ## 17565    NA 2012-11-30     2340
    ## 17566    NA 2012-11-30     2345
    ## 17567    NA 2012-11-30     2350
    ## 17568    NA 2012-11-30     2355

Histogram of the total number of steps taken each day
-----------------------------------------------------

    totalstepperday<-aggregate(steps ~ date, activity, FUN=sum,na.action = na.omit)
    head(totalstepperday)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

    hist(totalstepperday$steps, col="blue", main="Histogram of the total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)
\#\#Mean and median number of steps taken each day

    mean(totalstepperday$steps)

    ## [1] 10766.19

    median(totalstepperday$steps)

    ## [1] 10765

Time series plot of the average number of steps taken
-----------------------------------------------------

    average_number_steps <- aggregate(steps ~ interval, activity, FUN=sum)
    tail(average_number_steps)

    ##     interval steps
    ## 283     2330   138
    ## 284     2335   249
    ## 285     2340   175
    ## 286     2345    34
    ## 287     2350    12
    ## 288     2355    57

    plot(x=average_number_steps$interval, y=average_number_steps$steps, type="l", main="Time series plot of the average number of steps taken", xlab="Interval", ylab="Steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)
Which 5-minute interval, on average across all the days in the dataset,
contains the maximum number of steps?

    which.max(average_number_steps$steps)

    ## [1] 104

    print(average_number_steps[104,])

    ##     interval steps
    ## 104      835 10927

Code to describe and show a strategy for imputing missing data
--------------------------------------------------------------

1.  Calculate and report the total number of missing values in the
    dataset

<!-- -->

    table(is.na(activity))

    ## 
    ## FALSE  TRUE 
    ## 50400  2304

2.Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc.

    intervalmean<- aggregate(steps ~ interval, activity, FUN=mean)
    activity1<- merge(x=activity, y=intervalmean, by="interval")
    activity1$steps <- ifelse(is.na(activity1$steps.x), activity1$steps.y, activity1$steps.x)
    head(activity1)

    ##   interval steps.x       date  steps.y    steps
    ## 1        0      NA 2012-10-01 1.716981 1.716981
    ## 2        0       0 2012-11-23 1.716981 0.000000
    ## 3        0       0 2012-10-28 1.716981 0.000000
    ## 4        0       0 2012-11-06 1.716981 0.000000
    ## 5        0       0 2012-11-24 1.716981 0.000000
    ## 6        0       0 2012-11-15 1.716981 0.000000

3.Create a new dataset that is equal to the original dataset but with
the missing data filled in.

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    activity1 <- select(activity1, steps, date, interval)
    head(activity1)

    ##      steps       date interval
    ## 1 1.716981 2012-10-01        0
    ## 2 0.000000 2012-11-23        0
    ## 3 0.000000 2012-10-28        0
    ## 4 0.000000 2012-11-06        0
    ## 5 0.000000 2012-11-24        0
    ## 6 0.000000 2012-11-15        0

4.Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?

    steps_total<- aggregate(steps ~ date, activity1, FUN=sum)
    par(mfrow=c(2,1))
    hist(steps_total$steps, 
         col="blue",
         xlab = "Steps", 
         main = "Total Number Of Steps,after imputing NA values")
    hist(totalstepperday$steps, 
         col="yellow", 
         xlab = "Steps",
         main = "Total Number Of Steps-Orginal Dataset")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    mean1 <- mean(steps_total$steps)
    median1 <- median(steps_total$steps)
    paste("New Mean      :", mean1, "," ,  
          " Original Mean :", mean(totalstepperday$steps),"," , 
          " Difference :",mean1 -  mean(totalstepperday$steps))

    ## [1] "New Mean      : 10766.1886792453 ,  Original Mean : 10766.1886792453 ,  Difference : 0"

    paste("New Median      :", median1, "," ,  
          " Original Median :", median(totalstepperday$steps),"," , 
          " Difference :",median1 -  median(totalstepperday$steps))

    ## [1] "New Median      : 10766.1886792453 ,  Original Median : 10765 ,  Difference : 1.1886792452824"

Histogram of the total number of steps taken each day after missing values are imputed
--------------------------------------------------------------------------------------

    par(mfrow=c(1,1))
    hist(steps_total$steps, 
         col="blue",
         xlab = "Steps", 
         main = "Total Number Of Steps,after imputing NA values")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)
\#\# Panel plot comparing the average number of steps taken per 5-minute
interval across weekdays and weekends 1.Create a new factor variable in
the dataset with two levels - "weekday" and "weekend" indicating whether
a given date is a weekday or weekend day.

    library(chron)
    table(is.weekend(activity1$date))

    ## 
    ## FALSE  TRUE 
    ## 12960  4608

    activity1$day_week <- ifelse(is.weekend(activity1$date), "weekend", "weekday")
    table(activity1$day_week)

    ## 
    ## weekday weekend 
    ##   12960    4608

    tail(activity1)

    ##          steps       date interval day_week
    ## 17563 0.000000 2012-10-16     2355  weekday
    ## 17564 0.000000 2012-10-07     2355  weekend
    ## 17565 0.000000 2012-10-25     2355  weekday
    ## 17566 0.000000 2012-11-03     2355  weekend
    ## 17567 1.075472 2012-10-08     2355  weekday
    ## 17568 1.075472 2012-11-30     2355  weekday

2.Make a panel plot containing a time series plot of the 5-minute
interval (x-axis) and the average number of steps taken, averaged across
all weekday days or weekend days (y-axis). See the README file in the
GitHub repository to see an example of what this plot should look like
using simulated data.

    mean2<- aggregate(steps ~ interval + day_week, activity1, FUN=mean)
    head(mean2)

    ##   interval day_week      steps
    ## 1        0  weekday 2.25115304
    ## 2        5  weekday 0.44528302
    ## 3       10  weekday 0.17316562
    ## 4       15  weekday 0.19790356
    ## 5       20  weekday 0.09895178
    ## 6       25  weekday 1.59035639

    library(ggplot2)
    ggplot(mean2, aes(x=interval, y=steps)) + 
            geom_line(color="blue", size=1) + 
            facet_wrap(~day_week, nrow=2) 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-12-1.png)
