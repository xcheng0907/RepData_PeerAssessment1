setwd("/home/xcheng0907/GoogleDrive/Courses/DataScience_Coursera/ReproducibleResearch/RepData_PeerAssessment1")
data = read.csv("activity.csv")
data$date = as.Date(data$date)
str(data)

stepDay = aggregate(data$steps, list(data$date), sum)
colnames(stepDay) = c("date", "step")
stepDay$date = as.Date(stepDay$date)
str(stepDay)

hist(stepDay$step, 20, main="Number of Steps per day", xlab="steps", ylab="number of days")
as.integer(1.2)

(step5m = aggregate(steps ~ interval, data=data, FUN=mean))
step5m$interval[which.max(step5m$steps)]

data2 = merge(data, step5m, by="interval", all.x=TRUE)
colnames(data2) = c("interval", "steps", "date", "aveStep")
NAcols = is.na(data2$steps)
data2$steps[NAcols] = as.integer(data2$aveStep[NAcols])
data2 = data2[, 1:3]
data2 = data2[order(data2$date, data2$interval), ]
data2 = data2[, names(data)]
head(data2)

str(data2)
days = weekdays(data2$date)
wkd <- function(x) {
  if (x %in% c("Saturday", "Sunday")){ return("weekend") }
  else{ return("weekday")}
}
days = as.vector(sapply(days, wkd))
data2$wkd = as.factor(days)


data3 = aggregate(steps ~ wkd + interval, data=data2, FUN=mean)
library(lattice)
xyplot(steps~interval | wkd, data=data3,
       main="Steps for weekdays and weekends", xlab="interval",  ylab="steps",layout=c(1,2),type=c("l","l"))
