library(knitr)

asis_output(TRUE)

##setting working dir as knitr likes it this way

setwd("C:/Users/Stuart/Documents/R/repdata_data_activity")

## please have working directory set to where data is stored 

Data <- read.csv("activity.csv", header = TRUE, sep =",")

## Lets take a look at what we are working with

summary(Data)

## Calculate steps per day

stepsperday <- aggregate(steps ~ date, data = Data, FUN = sum)

## Histogram of steps

plot_001 <- hist(stepsperday$steps, freq = TRUE, xlab = "Steps per day", ylab = "Count days", 
                    main = "Activity: total steps per day")

## Get the mean and media for steps per day

mean(stepsperday$steps)

median(stepsperday$steps)

## get the average steps for each interval across all days

avg_steps_per_int <- aggregate(steps ~ interval, data = Data, FUN = mean)

## do the plot showing average steps for each interval

plot(avg_steps_per_int, type = "o")

## max steps per interval

max(avg_steps_per_int$steps)

## so we need to know the missing values, consulting our summary we see 2304 NA's 

summary(Data)

## interval and date are time base vales so unlikey to have NA lets check steps

sum(is.na(Data$steps))

## there they are, so we can replace them with the mean of the existing data

filledData <- Data 
  
filledData[is.na(Data)] <- 37

## so we apply the same process as ealier to see if the results differ after populating

filledstepsperday <- aggregate(steps ~ date, data = filledData, FUN = sum)

hist(filledstepsperday$steps, freq = TRUE)

mean(filledstepsperday$steps)

median(filledstepsperday$steps)

## which it has but not by much

## so lets look at weekday vs weekened

daytype <- function(date) {
  whichday <- weekdays(date)
  if (whichday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
    return("weekday")
  else (whichday %in% c("Saturday", "Sunday"))
  return("weekend")
}
Data$date <- as.Date(Data$date)
Data$daytype <- sapply(Data$date, FUN=daytype)

head(Data); table(Data$daytype)
weekdayAvg <- aggregate(steps ~ interval, data = Data, subset = Data$daytype == "weekday", FUN = mean)
weekendAvg <- aggregate(steps ~ interval, data = Data, subset = Data$daytype == "weekend", FUN = mean)
plot_006 <- plot(weekdayAvg , type = "l", col = "red", lwd =3, xlab="interval", ylab="avg steps", main ="Activty - avg steps")
lines(weekendAvg, type = "l", col = "blue", lwd=2)
legend("topright", legend = "Weekend:blue Weekday:red", box.col=8)


