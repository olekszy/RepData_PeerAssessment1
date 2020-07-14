library(lattice)
activities <- read.csv("activity.csv", stringsAsFactors=FALSE) # Read df from csv
activities$date <- as.POSIXct(activities$date, format="%Y-%m-%d") #import date as date format

activities <- data.frame(date=activities$date, 
                           weekday=tolower(weekdays(activities$date)), 
                           steps=activities$steps, 
                           interval=activities$interval) # check weekdays
activities <- cbind(activities, 
                      daytype=ifelse(activities$weekday == "sobota" | 
                                       activities$weekday == "niedziela", "weekend", 
                                     "weekday")) # separate weekdays from weekends
activity <- data.frame(date=activities$date, 
                       weekday=activities$weekday, 
                       daytype=activities$daytype, 
                       interval=activities$interval,
                       steps=activities$steps) # dataframe creation


assumeddata <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE) # local values without DF
names(assumeddata) <- c("date", "total") # new column names
#Creation of histogram of total number of steps peer day
hist(assumeddata$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="purple", 
     xlab="Total_steps", 
     ylim=c(0, 20), 
     main="Total number of steps witout NAs")

means <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE) # count means

names(means) <- c("interval", "mean") # renaming
#create plot for time series
plot(means$interval, 
     means$mean, 
     type="l", 
     col="purple", 
     lwd=2, 
     xlab="Time in minutes", 
     ylab="Average steps", 
     main="Times without NAs)")
#####################
max_pos <- which(means$mean == max(means$mean)) #count maximum mean
max_interval <- means[max_pos, 1]
NAs <- sum(is.na(activity$steps)) #Count sum of NAs
NAs_positions <- which(is.na(activity$steps)) #find where nas are
mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(NAs_positions)) # means to vetor
activity[NAs_positions, "steps"] <- mean_vec
assumeddata <- aggregate(activity$steps, by=list(activity$date), FUN=sum)
names(assumeddata) <- c("date", "total") # new attributes
#histogram of total steps, but NAs repleaced
hist(assumeddata$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="purple", 
     xlab="Total steps", 
     ylim=c(0, 30), 
     main="Total steps, but NAs repleaced with mean value")

means <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean) #mean values
names(means) <- c("daytype", "weekday", "interval", "mean") # rename columns
#plotting
xyplot(mean ~ interval | daytype, means, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))

