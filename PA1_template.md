Body Tracker
============

Before starting the analysis, I usually setup the envirnoment.

``` r
library(knitr)
library(lattice)
library(reshape2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
opts_chunk$set(root.dir = '~/Data Science Spec/R practice/5.Reproducible Reserch/AssignmentWeek1/',
               fig.path='~/Data Science Spec/R practice/5.Reproducible Reserch/AssignmentWeek1/figure/')
```

This document reports the anlysis done on the below **dataset**:

``` r
dataset.url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
dataset.zip <- "activity.zip"
dataset.csv <- "activity.csv"
if (!file.exists(dataset.zip)){
    download.file(dataset.url, dataset.zip)
}  
if (!file.exists(dataset.csv)) { 
    unzip(dataset.zip) 
}
dataset.df <- read.csv(dataset.csv)
summary(dataset.df)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

``` r
str(dataset.df)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

This dataset was produced by a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The dataset consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Once, the data have been collected, I moved on to a first anlysis of the daily activities.

Total steps per day
-------------------

Firstly, I gave a look at the total steps done daily by the subject. The total steps per day are computed as follow:

``` r
(daily.steps <- with(dataset.df, sapply(split(steps,date),sum,na.rm=T)))
```

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
    ##          0        126      11352      12116      13294      15420 
    ## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
    ##      11015          0      12811       9900      10304      17382 
    ## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
    ##      12426      15098      10139      15084      13452      10056 
    ## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
    ##      11829      10395       8821      13460       8918       8355 
    ## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
    ##       2492       6778      10119      11458       5018       9819 
    ## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
    ##      15414          0      10600      10571          0      10439 
    ## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
    ##       8334      12883       3219          0          0      12608 
    ## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
    ##      10765       7336          0         41       5441      14339 
    ## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
    ##      15110       8841       4472      12787      20427      21194 
    ## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
    ##      14478      11834      11162      13646      10183       7047 
    ## 2012-11-30 
    ##          0

Then, a histogram was useful to quickly produce a first insight of the dataset:

``` r
hist(daily.steps, main="Histogram for daily activities", 
     xlab="Steps", 
     border="green", 
     col=rgb(0, 0, 0, 0.6),
     breaks=8)
```

![](~/Data%20Science%20Spec/R%20practice/5.Reproducible%20Reserch/AssignmentWeek1/figure/HistPerDay-1.png)

``` r
steps.mean <- mean(daily.steps, na.rm = T)
steps.median <- median(daily.steps, na.rm = T)
```

The mean and the median of the daily total steps are equal to 9354.2295082 and 10395.

Daily activity pattern
----------------------

This section shows the average number of steps taken every five minutes, averaged across all days.

Find the below example of the average number of steps taken at each interval:

``` r
# computing the the average number of steps taken 
interval.steps <- with(dataset.df, sapply(split(steps,interval),mean,na.rm=T))

# building a dataframe
interval.df = data.frame(interval = as.factor(names(interval.steps)), average= as.numeric(interval.steps))
head (interval.df)
```

    ##   interval   average
    ## 1        0 1.7169811
    ## 2        5 0.3396226
    ## 3       10 0.1320755
    ## 4       15 0.1509434
    ## 5       20 0.0754717
    ## 6       25 2.0943396

The following time series plot shows how the steps are distributed in average:

``` r
#plotting the time series
with(interval.df, plot(as.character(interval), average, type = "l", col = rgb(0.2, 0.4, 0.5, 0.6),  
                       main= "Daily activity pattern", xlab = "Interval", ylab = "Average across all days"))
with(interval.df,points(as.character(interval), average, col = rgb(0.2, 0.7, 0.5, 0.15), pch = 1))
```

![](~/Data%20Science%20Spec/R%20practice/5.Reproducible%20Reserch/AssignmentWeek1/figure/plotting-1.png)

``` r
max.interval <- interval.df[with(interval.df, which(average %in% max(average))),]
```

In average, the maximum number of steps, equals to 206.1698113, is taken at the 5-minute interval 835.

Imputing missing values
-----------------------

There are 2304 couples of days/intervals where there are missing values (coded as **NA**). These NAs may introduce bias into some calculations or summaries of the data. That is the reason why I tried to impute these values.

The example below shows some of the NA values in the dataset:

``` r
imputed.df <- dataset.df
na.id <- is.na(imputed.df$steps)
# before imputing
head(imputed.df[na.id,])
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

I imputed the missing values by replacing them with the average of the steps taken in the corresponding interval across all days. Below are the first 10 missing observations along with the corresponding interval steps mean:

``` r
# repeating the inter val steps mean over all the dates in the dataset
imputed.df$interval.steps.mean <- rep.int(interval.steps, times = length(unique(dataset.df$date)))
# getting the NAs ids                                          
na.id <- is.na(imputed.df$steps)
# before imputing
head(imputed.df[na.id,],10)
```

    ##    steps       date interval interval.steps.mean
    ## 1     NA 2012-10-01        0           1.7169811
    ## 2     NA 2012-10-01        5           0.3396226
    ## 3     NA 2012-10-01       10           0.1320755
    ## 4     NA 2012-10-01       15           0.1509434
    ## 5     NA 2012-10-01       20           0.0754717
    ## 6     NA 2012-10-01       25           2.0943396
    ## 7     NA 2012-10-01       30           0.5283019
    ## 8     NA 2012-10-01       35           0.8679245
    ## 9     NA 2012-10-01       40           0.0000000
    ## 10    NA 2012-10-01       45           1.4716981

Afterwards, the above shown observations were as follows:

``` r
# imputing the NAs values as the mean for that interval
imputed.df$steps [na.id] <- imputed.df$interval.steps.mean [na.id]
# after imputing
imputed.df <- imputed.df [,1:3] 
head(imputed.df[na.id,],10)
```

    ##        steps       date interval
    ## 1  1.7169811 2012-10-01        0
    ## 2  0.3396226 2012-10-01        5
    ## 3  0.1320755 2012-10-01       10
    ## 4  0.1509434 2012-10-01       15
    ## 5  0.0754717 2012-10-01       20
    ## 6  2.0943396 2012-10-01       25
    ## 7  0.5283019 2012-10-01       30
    ## 8  0.8679245 2012-10-01       35
    ## 9  0.0000000 2012-10-01       40
    ## 10 1.4716981 2012-10-01       45

Then, I gave a look at the impact ot the imputed values to the total steps done daily by the subject. The total steps per day are computed as follow:

``` r
daily.imputed.steps <- with(imputed.df, sapply(split(steps,date),sum,na.rm=T))
head(daily.imputed.steps)
```

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
    ##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00

Then, I compared the imputed dataset with the original one in a histogram:

``` r
par(mfrow = c(2, 1))
hist(daily.steps, main="Daily activities", 
     xlab="Steps", 
     border="green", 
     col=rgb(0, 0, 0, 0.6),
     breaks=8)
hist(daily.imputed.steps, main="Daily activities (imputed)", 
     xlab="Steps", 
     border="black", 
     col=rgb(0.8, 0.7, 0.2, 0.6),
     breaks=8)
```

![](~/Data%20Science%20Spec/R%20practice/5.Reproducible%20Reserch/AssignmentWeek1/figure/imputedHistPerDay-1.png)

``` r
imputed.steps.mean <- mean(daily.imputed.steps, na.rm = T)
imputed.steps.median <- median(daily.imputed.steps, na.rm = T)
```

The mean and the median of the daily imputed steps are slightly different from the originals:

``` r
datasets.name <- c("original","imputed")
datasets.means <- c(steps.mean,imputed.steps.mean)
datasets.medians <- c(steps.median,imputed.steps.median)
(comparison <- data.frame(dataset = datasets.name, mean = datasets.means, median = datasets.medians))
```

    ##    dataset     mean   median
    ## 1 original  9354.23 10395.00
    ## 2  imputed 10766.19 10766.19

Weekdays vs Weekends Analysis
-----------------------------

This section shows a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the imputed average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Firstly, I added a new factor variable to the dataset, the variable has two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:

``` r
imputed.df$weekday <- factor(as.numeric(wday(as.Date(dataset.df$date)) %in% 2:6), 
                             label = c("weekend","weekday"))
head(imputed.df)
```

    ##       steps       date interval weekday
    ## 1 1.7169811 2012-10-01        0 weekday
    ## 2 0.3396226 2012-10-01        5 weekday
    ## 3 0.1320755 2012-10-01       10 weekday
    ## 4 0.1509434 2012-10-01       15 weekday
    ## 5 0.0754717 2012-10-01       20 weekday
    ## 6 2.0943396 2012-10-01       25 weekday

Afterwards, I computed the imputed average number of steps taken in the weekdays and in the weekend:

``` r
# melt
melt.imputed.df <- melt(imputed.df, id = c("interval","weekday"),
                        measure.vars = c("steps"),variable_name = "var")
# dcast
imputed.weekdays.mean <- dcast(data = melt.imputed.df, formula = interval + weekday ~ variable, mean)
head(imputed.weekdays.mean)
```

    ##   interval weekday      steps
    ## 1        0 weekend 0.21462264
    ## 2        0 weekday 2.25115304
    ## 3        5 weekend 0.04245283
    ## 4        5 weekday 0.44528302
    ## 5       10 weekend 0.01650943
    ## 6       10 weekday 0.17316562

Finally, I made the panel plot using the lattice plot system:

``` r
with(imputed.weekdays.mean, xyplot(steps ~ interval | weekday, layout=c(1,2), type = "l", pch=20,
                                   main = "Weekdays and weekends activity pattern", 
                                   ylab = "Avg of steps", xlab = "5-minute interval"
                                   ))
```

![](~/Data%20Science%20Spec/R%20practice/5.Reproducible%20Reserch/AssignmentWeek1/figure/weekdaysPlot-1.png)
