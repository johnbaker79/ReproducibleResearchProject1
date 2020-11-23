#Load pacakages (dplyr, ggplot2, scales)
library(dplyr)
library(ggplot2)
library(scales)

#Load in the data (csv file) and process into a formate suitable for analysis
steps <- read.csv("~/Desktop/Coursera/activity.csv", header = TRUE) 

##Step 1 - C#onvert date to Date 
steps$date <- as.Date(steps$date)

##Step 2 - Convert time format to HH:MM
steps$interval <- format(strptime(formatC(steps$interval, width = 4, format = "d",
                                          flag = "0"), format = "%H%M"), format = "%H:%M")

#What is the mean total number of steps taken per day?

##Step 1 - Calculate total steps per day 
dailySteps <- steps %>%
        group_by(date) %>%
        summarize(total=sum(steps))

##Step 2 - Plot histogram of mean steps per day
ggplot(dailySteps, aes(total)) +
        geom_histogram(binwidth = 500, fill="deeppink4") +
        labs(x="Total Daily Steps", y="Frequency", title = "Daily Steps")

##Calculate and report the mean and median of the total number of steps per day
mean(dailySteps$total, na.rm = TRUE)
###[1] 10766.19

median(dailySteps$total, na.rm = TRUE)
###[1] 10765

#What is the avg. daily activity pattern?

##Step 1 - Calculate the mean number of steps taken for every five minute interval and average that across days

intSteps <- steps %>%
        group_by(interval) %>%
        summarize(mean=mean(steps, na.rm = TRUE))

##Step 2 - Create a plot showing time series and the average daily pattern of activity
ggplot(intSteps, aes(as.POSIXct(interval, format="%H:%M"), mean)) +
        geom_line(col="deeppink4") +
        scale_x_datetime(labels = date_format("%H:%M"),
                         date_breaks = "4 hours") +
        labs(x="Five Minute Interval", y="Number of Steps", title="Average steps by time of day")

##Step 3 - Calculate which 5-minute interval, on avg. across all days in the dataset, contains the maximum number of steps...
max(intSteps$mean)
###[1] 206.1698

##, which occurs in this time interval: 
intSteps[which.max(intSteps$mean), ]
### A tibble: 1 x 2 interval  mean <chr>    <dbl> 1 08:35     206.

#Imput all missing values in dataset

##Step 1 - Calculate and show total number of missing values in dataset 
sum(!complete.cases(steps))
###[1] 2304

##Step 2 - Create a new dataset that equals the original dataset but w/ missing data...
imputeSteps <- steps %>%
        group_by(interval) %>%
        mutate(steps=ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))

##Step 3 - Create a new histogram of the total number of steps taken each day w/ missing values...
imputedDailySteps <- imputeSteps %>%
        group_by(date) %>%
        summarize(total=sum(steps))

ggplot(imputedDailySteps, aes(total)) +
        geom_histogram(binwidth = 500, fill="deeppink4") +
        labs(x="Total Daily Steps", y="Frequency", title = "Daily Steps")

##Calculate and report the mean and median of the total number of steps per day
mean(imputedDailySteps$total, na.rm = TRUE)
###[1] 10766.19
median(imputedDailySteps$total, na.rm = TRUE)
###[1] 10766.19

#Are there differences in activity patterns when comparing weekdays to weekends?

##Step 1 - Create new factor variable in dataset with weekday and weekend levels
wSteps <- imputeSteps %>%
        mutate(wkday=ifelse(weekdays(date) %in% c("Saturdar", "Sunday"), "Weekend", "Weekday"))

##Turn into a factor 
wSteps <- wSteps %>%
        mutate(wkday=as.factor(wkday))

##Step 2 - Group according to interval and weekday (wkday/factor) and again calculate the mean # steps
wIntSteps <- wSteps %>%
        group_by(interval,wkday) %>%
        summarize(mean=mean(steps, na.rm = TRUE))

##Create time series plot with faceting on the new weekday (wkday) factor 
ggplot(wIntSteps, aes(as.POSIXct(interval, format = "%H:%M"), mean, col=wkday)) +
        geom_line(show.legend = F) + 
        facet_grid(rows = wIntSteps$wkday) +
        scale_x_datetime(labels = date_format("%H:%M"), 
                         date_breaks = "4 hours") +
        labs(x="Five Minute Interval", y="Number of Steps", title = "Average steps by time of day")


        











