Reproducible Research - Peer Assessment 1
============================================
This is r-markdown file for the peer assessment.

Loading some of the packages


```r
library(plyr)
library(ggplot2)
library(dplyr)
```

reading the data and changing the data types:


```r
data1 <- read.csv('activity.csv',sep = ',',header=T,na.strings='?')
data1$steps <- as.numeric(as.character(data1$steps))
```

```
## Warning: NAs introduced by coercion
```

```r
data1$date <- as.Date(data1$date,"%Y-%m-%d")
```

summarizing the data

total steps before without substituting NA

```r
total_with_na <- ddply(data1,.(date),summarize,steps_total=sum(steps),steps_mean=ceiling(mean(steps)))
head(total_with_na)
```

```
##         date steps_total steps_mean
## 1 2012-10-01          NA         NA
## 2 2012-10-02         126          1
## 3 2012-10-03       11352         40
## 4 2012-10-04       12116         43
## 5 2012-10-05       13294         47
## 6 2012-10-06       15420         54
```

plotting the histogram of total steps against date

```r
fig1<-ggplot(total_with_na,aes(date,steps_total)) + geom_bar(stat = "identity",binwidth = .5) + 
  labs(title = "Histogram of Total steps per day",xlab="Date",ylab="Total Steps")
print(fig1)
```

```
## Warning in loop_apply(n, do.ply): Removed 8 rows containing missing values
## (position_stack).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Calculating the mean and median

Mean of total number of steps per day

```r
mean(total_with_na$steps_total,na.rm=T)
```

```
## [1] 10766.19
```
Median of total number of steps per day

```r
median(total_with_na$steps_total,na.rm=T)
```

```
## [1] 10765
```

Average daily activity plan

Calculating the steps mean by interval


```r
steps_mean_by_interval <- ddply(data1,.(interval),summarize,steps_total=sum(steps,na.rm=TRUE),steps_mean=ceiling(mean(steps,na.rm=TRUE)))
head(steps_mean_by_interval,3)
```

```
##   interval steps_total steps_mean
## 1        0          91          2
## 2        5          18          1
## 3       10           7          1
```
Plotting the Daily activity plan

```r
with(steps_mean_by_interval,plot(interval,steps_mean,type='l',col='green',xlab="Interval period",ylab="Average steps of the interval",main="Daily activity chart"))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

The interval at which most steps are recorded


```r
d2<-max(steps_mean_by_interval$steps_mean)
index <- steps_mean_by_interval$steps_mean %in% d2
maxinterval <- steps_mean_by_interval[index, ]
maxinterval
```

```
##     interval steps_total steps_mean
## 104      835       10927        207
```

Total Number of missing values

```r
sum(!complete.cases(data1))
```

```
## [1] 2304
```

Imputing the missing values with the mean of that interval


```r
impute.data<- join(data1,steps_mean_by_interval,by="interval")
impute.data$steps <- with(impute.data,ifelse(is.na(steps)==TRUE,steps_mean,steps))
```

All the data has been replaced

```r
sum(!complete.cases(impute.data))
```

```
## [1] 0
```

plotting the histogram of total steps against date after imputing the missing values

```r
total_without_na <- ddply(impute.data,.(date),summarize,steps_total=sum(steps),steps_mean=ceiling(mean(steps)))
fig2<-ggplot(total_without_na,aes(date,steps_total)) + geom_bar(stat = "identity",binwidth = .5) + 
  labs(title = "Histogram of Total steps per day",xlab="Date",ylab="Total Steps")
print(fig2)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

Calculating the mean and median of Imputed data

Mean of total number of steps per day

```r
mean(total_without_na$steps_total,na.rm=T)
```

```
## [1] 10784.92
```
Median of total number of steps per day

```r
median(total_without_na$steps_total,na.rm=T)
```

```
## [1] 10909
```

Comparing means and median shows that there is significant increase in mean and median after imputing values


Finding the activity pattern between weekdays and weekends

Factoring the dates into weekdays and weekends

```r
impute.data1 <- impute.data
impute.data$weekdays <- factor(format(impute.data$date,"%A"))

levels(impute.data$weekdays) <- list(Weekday = c("Monday","Tuesday","Wednesday","Thursday","Friday"), 
                                       Weekend=c("Saturday","Sunday"))

levels(impute.data$weekdays)
```

```
## [1] "Weekday" "Weekend"
```

```r
averages_by_weekdays <- aggregate(impute.data$steps, list(interval = as.numeric(as.character(impute.data$interval)), 
                                                            weekdays = impute.data$weekdays),FUN = "mean")
names(averages_by_weekdays)[3] <- "steps_mean"
```

Plotting the activity pattern by weekdays and weekends


```r
library(lattice)
fig3 <- xyplot(averages_by_weekdays$steps_mean ~ averages_by_weekdays$interval | averages_by_weekdays$weekdays, 
                 layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
print(fig3)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 
