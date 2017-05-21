    knitr::opts_chunk$set(echo = FALSE)
    activity <- read.csv("activity.csv")
    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    summary(activity)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
    total_step <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
    head(total_step)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

    par(mfrow = c(1, 1))
    # use base plotting system and more bins than the default setting
    hist(total_step$steps, breaks = 20, 
         main = "Total Number of Steps Taken Each Day",
         col = "grey", border = "white", xlab = "Step", axes = FALSE)
    axis(1)
    axis(2, las = 1)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    mean(total_step$steps)

    ## [1] 10766.19

    median(total_step$steps)

    ## [1] 10765

    avg_step <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
    plot(avg_step$interval, avg_step$steps, type = "l", lwd = 2, col = "navy",
         main = "Time Series: Average Number of Steps Taken", axes = FALSE,
         xlab = "5-minute interval", ylab = "Average number of steps")
    axis(1)
    axis(2, las = 1)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-2.png)

    avg_step$interval[which.max(avg_step$steps)]

    ## [1] 835

    sum(is.na(activity))

    ## [1] 2304

    imp <- activity # new dataset called imp
    for (i in avg_step$interval) {
        imp[imp$interval == i & is.na(imp$steps), ]$steps <- 
            avg_step$steps[avg_step$interval == i]
    }
    head(imp)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

    sum(is.na(imp))

    ## [1] 0

    total_step_imp <- aggregate(steps ~ date, data = imp, sum, na.rm = TRUE)
    hist(total_step_imp$steps, breaks = 20, 
         main = "Total Number of Steps Taken Each Day (Imputed)",
         col = "grey", border = "white", xlab = "Step", axes = FALSE)
    axis(1)
    axis(2, las = 1)
    mean(total_step_imp$steps)

    ## [1] 10766.19

    median(total_step_imp$steps)

    ## [1] 10766.19

    imp$day <- weekdays(imp$date)
    imp$week <- ""
    imp[imp$day == "Saturday" | imp$day == "Sunday", ]$week <- "weekend"
    imp[!(imp$day == "Saturday" | imp$day == "Sunday"), ]$week <- "weekday"
    imp$week <- factor(imp$week)
    avg_step_imp <- aggregate(steps ~ interval + week, data = imp, mean)
    library(lattice)

    ## Warning: package 'lattice' was built under R version 3.3.3

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-3.png)

    xyplot(steps ~ interval | week, data = avg_step_imp, type = "l", lwd = 2,
           layout = c(1, 2), 
           xlab = "5-minute interval", 
           ylab = "Average number of steps",
           main = "Average Number of Steps Taken (across all weekday days or weekend days)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-1-4.png)
