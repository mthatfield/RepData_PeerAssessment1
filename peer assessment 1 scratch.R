####peer assesment 1

library(ggplot2)

dat <- read.csv("activity.csv")

dat$date <- as.Date(as.character(dat$date),fomat= "%Y"-"%m"-"%d")

datrna <- dat[!is.na(dat$steps),]

datrna$date <- as.Date(as.character(datrna$date),fomat= "%Y"-"%m"-"%d")

days <- length(unique(datrna$date))

steps <- sum(datrna$steps)

avg <- steps/days
daystepframe <- NULL

uniquedates <- unique(datrna$date)

for(i in 1:length(unique(datrna$date))){
    tdf <- datrna
    tdf <- tdf[tdf$date==uniquedates[i],]
    stp <- sum(tdf$steps)
    daysteps <- stp
    daystepframe <- rbind(daystepframe,daysteps)
}

daystepframe <- as.data.frame(daystepframe)
rownames(daystepframe) <- NULL
daystepframe <- cbind(daystepframe,uniquedates)
colnames(daystepframe) <- c("steps", "date")

ggplot(data=daystepframe,aes(x=steps))+
    geom_histogram()


####### 2


meandsf <- mean(daystepframe$steps)

mediandsf <- median(daystepframe$steps)

timegap <- datrna

timeavs <- data.frame()

for(j in 1: length(unique(timegap$interval))){
    timeint <- timegap[timegap$interval==unique(timegap$interval)[j],]
    timeavg <- mean(timeint$steps)
    timeconc <- c(timeavg,unique(timegap$interval)[j])
    timeavs <- rbind(timeavs,timeconc)
}

names(timeavs)<- c("steps","interval")

ggplot(data=timeavs,aes(x=interval,y=steps))+
    geom_line()


####### 3

which(timeavs$steps==max(timeavs$steps))
timeavs$interval[104]

sum(is.na(dat$steps))

datimp <- dat

nas <- which(is.na(datimp$steps))
datnas <- datimp[nas,]
impnas <- data.frame()
for(k in 1:length(unique(timeavs$interval))){
    nasred <- datnas[datnas$interval==timeavs$interval[k],]
    nasred$steps <- timeavs$steps[k]
    impnas <- rbind(impnas,nasred)
}


fullimpdat <- rbind(datrna,impnas)

daysimp <- length(unique(fullimpdat$date))

stepsimp <- sum(fullimpdat$steps)

avgimp <- stepsimp/daysimp
daystepframeimp <- data.frame()

uniquedatesimp <- unique(fullimpdat$date)

for(i in 1:length(unique(fullimpdat$date))){
    tdf <- fullimpdat
    tdf <- tdf[tdf$date==uniquedatesimp[i],]
    stp <- sum(tdf$steps)
    daysteps <- stp
    daystepframeimp <- rbind(daystepframeimp,daysteps)
}



daystepframeimp <- cbind(daystepframeimp,uniquedatesimp)
colnames(daystepframeimp) <- c("steps", "date")

median(daystepframeimp$steps)

ggplot(data=daystepframeimp,aes(x=steps))+
    geom_histogram()

######## 4
fullimpdat <- cbind(fullimpdat,rep(0,times=nrow(fullimpdat)))

names(fullimpdat)[4]<- "weekday"

ends <- which(weekdays(fullimpdat$date)=="Saturday"  | weekdays(fullimpdat$date)=="Sunday")
wdays <- which(!(weekdays(fullimpdat$date)=="Saturday"  | weekdays(fullimpdat$date)=="Sunday"))


fullimpdat$weekday[ends] <- "e"

fullimpdat$weekday[wdays] <- "d"

timegapd <- fullimpdat[fullimpdat$weekday=="d",]

timeavsd <- data.frame()

for(m in 1: length(unique(timegapd$interval))){
    timeint <- timegapd[timegapd$interval==unique(timegapd$interval)[m],]
    timeavg <- mean(timeint$steps)
    timeconc <- c(timeavg,unique(timegapd$interval)[m])
    timeavsd <- rbind(timeavsd,timeconc)
}

names(timeavsd)<- c("steps","interval")

timegape <- fullimpdat[fullimpdat$weekday=="e",]

timeavse <- data.frame()

for(n in 1: length(unique(timegape$interval))){
    timeint <- timegape[timegape$interval==unique(timegape$interval)[n],]
    timeavg <- mean(timeint$steps)
    timeconc <- c(timeavg,unique(timegape$interval)[n])
    timeavse <- rbind(timeavse,timeconc)
}

names(timeavse)<- c("steps","interval")

ggplot(data=timeavsd,aes(x=interval,y=steps))+
    geom_line()+
    geom_line(data=timeavse,aes(x=interval,y=steps),color="yellow")



   