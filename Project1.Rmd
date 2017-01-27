---
title: "Reproducible Research Course Project 1"
author: "Connor"
date: "January 23, 2017"
output:
  html_document: default
  pdf_document: default
---
# Reproducible Research: Peer Assessment 1 
##Just loading the data and removing NA values (for now)
```{r rawraw,warning = FALSE}
library(ggplot2)
library(mice)

raw <- read.csv("./activity/activity.csv")
raw.noNA <- raw[complete.cases(raw), ]
```

##Next, looking at the mean total steps per day:

```{r} 


daily.total <- tapply(raw.noNA$steps, raw.noNA$date, sum, na.rm = T)



steps <- qplot(daily.total, xlab = "steps/day", ylab= "frequency (bin 500)", binwidth = 500)
steps
```

### Calculating the mean and median total number of steps:

```{r mandm} 
meansteps <- mean(daily.total, na.rm = T)
medsteps <- median(daily.total, na.rm = T)

```

 * Mean: `r meansteps`
 * Median: `r medsteps`


## Ok, so what's the average daily activity pattern? 

```{r adap, warning = FALSE} 

avg.step <- aggregate(steps ~ interval, data = raw.noNA, FUN = mean)

activityPattern <- ggplot(data = avg.step, aes(x 
                                               = interval, y = steps)) + 
  geom_line()+ 
  xlab("Five-Min Interval") + 
  ylab("Number of steps")
activityPattern
```

### What's the five minute interal with the most steps? 

```{r moststeps, echo = T}
most<- which.max(avg.step$steps)
mosttime <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avg.step[most,'interval'])

```

It's `r mosttime` !

## So how many missing observations were there in the original data?

```{r somanyNAs}
 nacount <- sum(is.na(raw))
```

There are `r nacount` NAs. 

## Devising a sneaky strategy to impute NAs. 
```{r sneaky} 
raw.fillNA <- mice(raw, meth = "norm.predict")
raw.fill <- complete(raw.fillNA)
checkNA <- sum(is.na(raw.fill))
```

Now there are `r checkNA` NAs. 

##A histogram of tital number of steps per day, with imputed data. 

```{r impgraph}

daily <- data.frame()


daily2.sum <- tapply(raw.fill$steps, raw.fill$date, sum, na.rm = T)

daily2.total <- data.frame(daily2.sum)

steps2 <- qplot(daily2.sum, xlab = "step/day", ylab = "freq bin 500" , binwidth = 500)
steps2
```

### and the mean and mean of total number of steps per diem 

```{r mandmimp}
 impmean <- mean(daily2.sum, na.rm=T)
 impmed <- median(daily2.sum, na.rm = T)
```
 
  * Mean: `r impmean`
  * Median: `r impmed`
  
## Creating weekend/weekdays 
```{r week}


raw.fill$date <- as.Date(raw.fill$date)
raw.fill$day <- weekdays(raw.fill$date, abbreviate = TRUE)

for(i in 1:nrow(raw.fill)){
  if(raw.fill$day[i] == "Sat" || raw.fill$day[i] == "Sun"){
    raw.fill$weekday[i] <- "WEEKEND" } else 
    {raw.fill$weekday[i] <- "WEEKDAY" }
}
```

### Panel plot! 

```{r panel} 


avgWeek <- aggregate(steps ~ interval + weekday, raw.fill, mean)

weekendWarrior <- ggplot(avgWeek,aes(x=interval, y=(steps))) + 
  geom_line(color="blue") + 
  facet_wrap(~ weekday, nrow=2, ncol=1) +
  labs(x="Interval", y="Number of steps") +
  theme_bw()

weekendWarrior
```