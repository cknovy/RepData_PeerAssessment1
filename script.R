library(ggplot2)
library(mice)

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

raw <- read.csv("./activity/activity.csv")
raw.noNA <- raw[complete.cases(raw), ]
#a histogram of the total number of steps taken each
daily <- data.frame()
funcs <-  c("mean", "sum", "median")
for (i in 1:length(funcs)) {

  temp <- tapply(raw.noNA$steps, raw.noNA$date, funcs[i], na.rm = T)
  temp <- assign(paste("daily." , funcs[i], sep = ""), temp)

}
daily.total <- data.frame(daily.sum)

steps <- ggplot(daily.total, aes(daily.sum)) +
  geom_histogram(binwidth = 500)
steps
#a time series plot (i.e. type = "l")
#of the 5-minute interval (x-axis) and
#the average number of steps taken, averaged across all days (y-axis)

avg.step <- aggregate(steps ~ interval, data = raw.noNA, FUN = mean)

activityPattern <- ggplot(data = avg.step, aes(x = interval, y = steps)) + 
  geom_line()+ 
  xlab("Five-Min Interval") + 
  ylab("Number of steps")
activityPattern

countNA <- sum(is.na(raw$steps))

raw.fillNA <- mice(raw, meth = "norm.predict")
raw.fill <- complete(raw.fillNA)
checkNA <- sum(is.na(raw.fill))

daily <- data.frame()

for (i in 1:length(funcs)) {
  
  temp <- tapply(raw.fill$steps, raw.fill$date, funcs[i], na.rm = T)
  temp <- assign(paste("daily2." , funcs[i], sep = ""), temp)
  
}
daily2.total <- data.frame(daily2.sum)

steps2 <- ggplot(daily.total, aes(daily2.sum)) +
  geom_histogram(binwidth = 500)
steps2


raw.fill$date <- as.Date(raw.fill$date)
raw.fill$day <-   weekdays(raw.fill$date, abbreviate = TRUE)
raw.filL$interval <- as.Date(raw.fill$interval)

for(i in 1:nrow(raw.fill)){
  if(raw.fill$day[i] == "Sat" || raw.fill$day[i] == "Sun"){
    raw.fill$weekday[i] <- "WEEKEND" } else 
    {raw.fill$weekday[i] <- "WEEKDAY" }
}

avgWeek <- aggregate(steps ~ interval + weekday, raw.fill, mean)

weekendWarrior <- ggplot(avgWeek,aes(x=interval, y=(steps))) + 
  geom_line(color="blue") + 
  facet_wrap(~ weekday, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()

weekendWarrior
