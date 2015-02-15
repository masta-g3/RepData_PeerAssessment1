library("dplyr")
library("ggplot2")

##Load and convert data.
activity <- read.csv("activity.csv")
activity <- tbl_df(activity)

##Get total steps per day.
step_sum <-activity %>%
  group_by(date) %>%
  summarise(sum = sum(steps))

##Plot histogram of steps taken per day.
qplot(sum, data = step_sum, geom = "histogram")

##Mean and median steps per day.
mean(activity$steps, na.rm=T)
median(activity$steps, na.rm=T)

##Average steps per interval.
step_int <-activity %>%
  group_by(interval) %>%
  summarise(mean = mean(steps, na.rm=T))

qplot(x = interval, y = mean, data = step_int) + geom_line()

step_int[step_int$mean == max(step_int$mean), ]

##Total number of missing values per row, and incomplete cases.

sapply(activity, function(x) sum(is.na(x)))

sum(!complete.cases(activity))

##Dealing with missing values (use 5 min mean interval)

##step_int
naRemove <- function(x) {
  
}