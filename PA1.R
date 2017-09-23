# Reproducible Research
# Project 1 in Week 2

if(!file.exists("./data")){dir.create("./data")}
# unzip file
unzip("./data/repdata_data_activity.zip",exdir = "./data")

Raw <- read.csv("./data/activity.csv")
Activity <- na.omit(Raw)

# Calculate the total number of steps taken daily
TotalStepsPerDay <- tapply(Activity$steps, Activity$date, FUN = sum)

# Make Histogram of total number of steps taken each day.
hist(TotalStepsPerDay,
     main = "Steps Per Day",
     xlab = "Steps/Day",
     ylab = "Frequency",
     col = "green",
     ylim = c(0,30))

# Calculate the mean and median of number of steps per day.
MeanStepsDay <- mean(TotalStepsPerDay, na.rm = TRUE)
MedianStepsDay <- median(TotalStepsPerDay, na.rm = TRUE)

# Calculate the 5 min averages over all days
FiveMinAvgs <- tapply(Activity$steps, Activity$interval, FUN = mean, na.rm = TRUE)
Intervals = Activity$interval[1:288]
plot(Intervals,FiveMinAvgs,
     main = "Average Steps Taken Per 5 Minute Interval",
     ylab = "Average Steps Taken",
     xlab = "5 Min Interval",
     type = "l")

# Which 5 minute interval on average had the largest number of steps (the 104th 5 minute interval)
MaxStepInt <- which.max(FiveMinAvgs)

# Calculate the total number of missing values in the dataset (number of rows with NAs) 2304
NumNAs <- sum(is.na(Raw))


# RawNoNAs <- replace(Raw,is.na(Raw),0)

NumObs <- dim(Raw)
RawNAsToAvg <- Raw

# Replace any NAs with the average of the corresponding interval
jj <- 1
for ( ii in 1:NumObs[1] ) {
  if ( ii %% length(FiveMinAvgs) != 0 ) {
    jj <- jj + 1
  } else {
    jj <- 1
  }
  if ( is.na(Raw[ii,1]) ) {
    RawNAsToAvg[ii,1] <- FiveMinAvgs[jj]
  }
}


# Calculate the total number of steps taken daily for the new dataset
NewTotalStepsPerDay <- tapply(RawNAsToAvg$steps, RawNAsToAvg$date, FUN = sum)

# Make New Histogram of total number of steps taken each day.
hist(NewTotalStepsPerDay,
     main = "Steps Per Day",
     xlab = "Steps/Day",
     ylab = "Frequency",
     col = "blue",
     ylim = c(0,30))

# Calculate the mean and median of number of steps per day.
NewMeanStepsDay <- mean(NewTotalStepsPerDay, na.rm = TRUE)
NewMedianStepsDay <- median(NewTotalStepsPerDay, na.rm = TRUE)

# Add factor column to new dataset that indicates "WeekDay" or "WeekEnd" 
RawNAsToAvg$DoW <- ifelse(weekdays(as.Date(RawNAsToAvg$date)) %in% c("Saturday","Sunday"), "WeekEnd","WeekDay")

# get mean values for weekdays and weekends
a <- RawNAsToAvg$steps[RawNAsToAvg$DoW == "WeekDay"]
b <- RawNAsToAvg$interval[RawNAsToAvg$DoW == "WeekDay"]
c <- tapply(a,b,FUN = mean)

a1 <- RawNAsToAvg$steps[RawNAsToAvg$DoW == "WeekEnd"]
b1 <- RawNAsToAvg$interval[RawNAsToAvg$DoW == "WeekEnd"]
c1 <- tapply(a1,b1,FUN = mean)

# Create panel plot to compare weekday steps and weekend steps
par(mfrow = c(2,1))
plot(Intervals,c,
     main = "WeekDay Avg Steps Taken Per 5 Minute Interval",
     ylab = "Average Steps Taken",
     xlab = "5 Min Interval",
     ylim = c(0,200),
     type = "l")
plot(Intervals,c1,
     main = "WeekEnd Avg Steps Taken Per 5 Minute Interval",
     ylab = "Average Steps Taken",
     xlab = "5 Min Interval",
     ylim = c(0,200),
     type = "l")


WeekDayMean <- mean(RawNAsToAvg$steps[RawNAsToAvg$DoW == "WeekDay"])
WeekEndMean <- mean(RawNAsToAvg$steps[RawNAsToAvg$DoW == "WeekEnd"])





