---
title: "PA12_template.Rmd"
author: "Constanza Arizandieta"
date: "3 de septiembre de 2019"
output:
  html_document: default
  pdf_document: default
---

## Loading and Processing the Data
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd("~/COURSERA/DATA SCIENCE/Reproducible Data/repdata_data_activity")
library(readr)
activity <- read_csv("activity.csv")
View(activity)
library(ggplot2)
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity, weekday)
```


## 1. What is the mean total number of steps taken per day? 
```{r activity, echo = TRUE}
TotalSteps <- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))
names(TotalSteps) <- c("date", "steps")
hist(TotalSteps$steps, main = "Total number of steps taken per day", xlab = "Total Steps Taken per Day", col = "turquoise3", ylim = c(0,20), breaks = seq(0, 25000, by = 2500))
```

### Mean of the total number of steps per day: 
```{r mean, echo = FALSE}
mean <- mean(TotalSteps$steps)
```
        The mean of the total number of steps per day is `mean`. 
        
### Median of the total number of steps taken per day:
```{r median, echo=FALSE}
median <- median(TotalSteps$steps)
```
The median of the total number of steps taken per day is `median`.

## 2. What is the average daily activity pattern?
```{r activity, echo=TRUE}
AvDailyAct <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
names(AvDailyAct) <- c("interval", "mean")
plot(AvDailyAct$interval, AvDailyAct$mean, type ="l", col = "darkorchid4", lwd = 2, xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per Intervals")
```

## 3. Imputing missing values 

### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r activity, echo = TRUE}
sum(is.na(activity$steps))
```

### We need to get rid of the missing values (NAs) of the dataset. 
```{r NA, echo = TRUE}
Imputed_NA_Steps <- AvDailyAct$mean[match(activity$interval, AvDailyAct$interval)]
```

### Next, we create a new dataset with de missing data filled in. 
```{r, echo=TRUE}
New_Activity <- transform(activity, steps = ifelse(is.na(activity$steps), yes = Imputed_NA_Steps, no = activity$steps))
Total_steps_imputed <- aggregate(steps ~ date, New_Activity, sum)
names(Total_steps_imputed) <- c("date", "daily_steps")
```

### Finally we create an histogram of the new dataset. 
```{r, echo=TRUE}
hist(Total_steps_imputed$daily_steps, col = "plum4", xlab = "Total Steps per Day", ylim = c(0,30), main = "Total Number of Steps Taken Each Day", breaks = seq(0, 25000, by = 2500))
```
If we compare the Histogram in the first question, the values between 0 and 250 have a frequency over 10, meanwhile in this Histogram with de missing values filled with the average daily activity mean presents a reduction of the values' frequency between 0-250. The impact is that exists a tendendy of the subject to take steps between 10,000 and 12,500 per day. 

### Mean of the total number of steps taken per day: 
```{r, echo=FALSE}
mean1 <- mean(Total_steps_imputed$daily_steps)
```
The mean of the total number of steps taken per day is `mean1`. 

### median of the total number of steps taken per day:
```{r, echo=FALSE}
median1 <- median(Total_steps_imputed$daily_steps)
```
The median of the total number of steps taken per day is `median1`. 

## 4. Are there differences in activity patterns between weekdays and weekends? 

### Weekdays & weekends
```{r, echo = TRUE}
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
        {y <- "Weekend"} else 
        {y <- "Weekday"}
        y
})
```

### Making a plot that shows the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r, echo = TRUE}
ActByDate <- aggregate(steps~interval + datetype, activity, mean, na.rm = FALSE)
plot <- ggplot(ActByDate, aes(x = interval, y = steps, color = datetype, facets = datetype)) + 
        geom_line() + 
        labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") + 
        facet_wrap(~datetype, ncol = 1, nrow = 2)
print(plot)
```
The plot demonstrates that a person has more distributed activity between intervals during the Weekends.

