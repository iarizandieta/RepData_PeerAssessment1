setwd("~/COURSERA/DATA SCIENCE/Reproducible Data/repdata_data_activity")
library(readr)
activity <- read_csv("activity.csv")
View(activity)
library(ggplot2)
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity, weekday)

## what is the mean total number of steps taken per day? 

TotalSteps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(TotalSteps) <- c("date", "steps")
hist(TotalSteps$steps, main = "Total number of steps taken per day", xlab = "Total Steps Taken per Day", col = "turquoise3", ylim = c(0,20), breaks = seq(0, 25000, by = 2500))
mean(TotalSteps$steps)
median(TotalSteps$steps)

## What is the average daily activity pattern? 
AvDailyAct <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
names(AvDailyAct) <- c("interval", "mean")
plot(AvDailyAct$interval, AvDailyAct$mean, type ="l", col = "darkorchid4", lwd = 2, xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per Intervals")

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
AvDailyAct[which.max(AvDailyAct$mean), ]$interval

## Imputing missing values 
sum(is.na(activity$steps))
Imputed_NA_Steps <- AvDailyAct$mean[match(activity$interval, AvDailyAct$interval)]
New_Activity <- transform(activity, steps = ifelse(is.na(activity$steps), yes = Imputed_NA_Steps, no = activity$steps))
Total_steps_imputed <- aggregate(steps ~ date, New_Activity, sum)
names(Total_steps_imputed) <- c("date", "daily_steps")
hist(Total_steps_imputed$daily_steps, col = "plum4", xlab = "Total Steps per Day", ylim = c(0,30), main = "Total Number of Steps Taken Each Day", breaks = seq(0, 25000, by = 2500))
mean(Total_steps_imputed$daily_steps)
median(Total_steps_imputed$daily_steps)

## Are there differences in activity patterns between weekdays and weekends? 

activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
        {y <- "Weekend"} else 
        {y <- "Weekday"}
        y
})

ActByDate <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot <- ggplot(ActByDate, aes(x = interval, y = steps, color = datetype, facets = datetype)) + 
        geom_line() + 
        labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") + 
        facet_wrap(~datetype, ncol = 1, nrow = 2)
print(plot)


