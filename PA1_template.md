### What is the mean total number of steps taken per day?

10,766 is the mean total number of steps taken per day, 10,765 is the media number of steps taken.

``` r
byday <- setNames(aggregate(ActivityData$steps, by= list(date = ActivityData$date), FUN =sum),c("date","steps"))

hist(byday$steps, breaks = 50, col = "red", xlab = "Daily Total Steps", main = "Histogram of Daily Step Totals")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
summary(byday$steps)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##      41    8841   10765   10766   13294   21194       8

### What is the average daily activity pattern?

``` r
byinterval <- setNames(aggregate(ActivityData$steps, by = list(interval = ActivityData$interval), FUN =mean, na.rm = TRUE),c("interval","steps"))

plot(byinterval$steps ~ byinterval$interval, type = "l", xlab = "Interval", ylab = "Steps", main = "Average Steps per Interval")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
subset(byinterval,byinterval$steps == max(byinterval$steps))
```

    ##     interval    steps
    ## 104      835 206.1698

Interval 835 has the most steps on average with about 206.

### Imputing missing values

``` r
sum(is.na(ActivityData$steps))
```

    ## [1] 2304

There are 2304 missing entries.These missing entries will be replaced with the average of the interval when the missing value occured.

``` r
merged <- merge(ActivityData, byinterval, by = "interval")

merged$steps.x[is.na(merged$steps.x)]= merged$steps.y[is.na(merged$steps.x)]

mergedbyday <- setNames(aggregate(merged$steps.x, by= list(date = merged$date), FUN =sum),c("date","steps"))

hist(mergedbyday$steps, breaks = 50, col = "red", xlab = "Daily Total Steps", main = "Histogram of Daily Step Totals")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
summary(mergedbyday$steps)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      41    9819   10766   10766   12811   21194

Both the mean and the median remained almost identical, while there was a shift from the 3rd quartile to the 1st quartile.

### Are there differences in activity patterns between weekdays and weekends?

``` r
merged$weekday <- ifelse(weekdays(as.Date(ActivityData$date)) %in% c("Saturday","Sunday"),"weekend","weekday")

mergedbydaytype <- setNames(aggregate(merged$steps.x, by = list(merged$weekday, merged$interval), FUN = mean),c("weekday","interval","steps"))

library(lattice)
xyplot(steps ~ interval | weekday, mergedbydaytype, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Mean Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)
