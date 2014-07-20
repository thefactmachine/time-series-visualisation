rm(list=ls())
library("classInt")
library("RCurl")


fnCalendarFlow <- function(dfCurrent) {    
    #==============================================
   span <- range(dfCurrent$jobs)[2] - range(dfCurrent$jobs)[1]
    startDate <- strptime(min(dfCurrent$date), "%Y-%m-%d")
    endDate <- strptime(max(dfCurrent$date), "%Y-%m-%d")
    timespan <- difftime(endDate, startDate, units="weeks")
    numweeks <- as.numeric(timespan, units="weeks") + 2
    numObservations <- nrow(dfCurrent)
    
    #blank plot
    plot(0, 0, type="n", xlim=c(0, numweeks), ylim=c(0, 8), asp=1, xaxt='n', yaxt='n', ann=FALSE, bty="n")
    
    # Helper function to find the last day of month in datestring
    lastDayOfMonth <- function(datestring, date.form = "%Y-%m-%d") {
        
        thedate <- strptime(datestring, date.form)
        theyear <- thedate$year + 1900
        themonth <- thedate$mon + 1
        
        themonth.posix <- as.POSIXct(paste(theyear, themonth, '1', sep='-'), format=date.form)
        month.next <- seq(themonth.posix, length=2, by='1 month')[2]
        last.day <- seq(month.next, length=2, by='-1 day')[2]
        
        return(strptime(last.day, date.form))
    }
    
    fnRemapDay <- function(intDayNum) {
        #remap day number 
        if (intDayNum == 0) {intReturnNum = 0}
        else if (intDayNum == 1 ) {intReturnNum = 6}
        else if (intDayNum == 2 ) {intReturnNum = 5}
        else if (intDayNum == 3 ) {intReturnNum = 4}
        else if (intDayNum == 4 ) {intReturnNum = 3}
        else if (intDayNum == 5 ) {intReturnNum = 2}
        else if (intDayNum == 6 ) {intReturnNum = 1}
    }

   # colours
   ncolors = 40
   pal <- colorRampPalette(c("#FFFFFF", "#0000cc")) # blue
   colorgrad <- pal(ncolors)
   gridColor <- "white"
   
   #ci <- classIntervals(dfNoWeekEND$jobs, n = ncolors, style = "quantile")
   ci <- classIntervals(dfNoWeekEND$jobs, n = ncolors, style = "fisher")
    
    fnReturnColor <- function(intJobNumber)  {
        colorNumber <- findInterval(intJobNumber, ci$brks)
        cellcolor <- colorgrad[colorNumber]
        return(cellcolor)
    }
    
   
    for (i in 1:numObservations) {
        if (dfCurrent$jobs[i] > 0) {
            currDate <- strptime(dfCurrent$date[i], "%Y-%m-%d")
            #wday ==> 0 for Sunday, 6 for Saturday
            dayofweek <- fnRemapDay(currDate$wday)
            diff <- difftime(currDate, startDate, units="weeks") + startDate$wday/6
            weeknum <- ceiling( as.numeric(diff, units="weeks") )
            cellcolor <- fnReturnColor(dfCurrent$jobs[i])
            rect(weeknum, dayofweek, (weeknum+1), (dayofweek+1), col=cellcolor, border=NA)
        }
    }

   # Draw calendar grid Grid of 7 x 52
    for (i in 1:numweeks) {
        #iterate between 1 and 52 Week 26  lines(26,26, 0, 7)
        lines(c(i, i), c(2, 7), col=gridColor, lwd=.5) 
    }
    for (j in 2:7) {
        lines(c(1, numweeks), c(j, j), col=gridColor, lwd=0.5)
    }
    
    
    # Month lines
    dateseq <- seq(startDate, endDate, by="1 month")
    #vector of dates typically 12.  Start at the first day of the month
    for (i in 1:(length(dateseq)-1)) {
        #typically iterates 11 times
        lastDay <- lastDayOfMonth(format(dateseq[i], "%Y-%m-%d"))
        diff <- difftime(lastDay, startDate, units="weeks") + startDate$wday/6
        weeknum <- ceiling( as.numeric(diff, units="weeks") )
        #end of month on a sunday
        dayofweek <- fnRemapDay(strptime(lastDay, "%Y-%m-%d")$wday)
        
        if (dayofweek == 0 || dayofweek == 1) {
            lines( c( (weeknum+1), (weeknum+1) ), c(2, 7), col=gridColor, lwd=3, lend=1)
        }
        else {
    
            lines( c( (weeknum+1), (weeknum+1) ), c(2, (dayofweek)), col=gridColor, lwd=3, lend=1)
            if (dayofweek != 2) {
                lines(c( (weeknum+1), weeknum+2), c( (dayofweek), (dayofweek)), col=gridColor, lwd=3, lend=1)
            }
            lines(c(weeknum +2, weeknum +2), c((dayofweek), 7), col=gridColor, lwd=3, lend=1)
        } 
    }
}


x <- getURL("https://raw.githubusercontent.com/thefactmachine/time-series-visualisation/master/Alljob.csv")
data <- read.csv(text = x)




data$RealDate <- as.character(data$RealDate)
date <- as.Date(data$RealDate, format = "%d.%m.%y")
df <-data.frame(date = date, jobs = data$Jobs, holiday = data$Holiday)
df2012 <- subset(df, as.numeric(format(df$date, "%Y")) == 2012)
df2013 <- subset(df, as.numeric(format(df$date, "%Y")) == 2013)
df2014 <- subset(df, as.numeric(format(df$date, "%Y")) == 2014)
#set up details for plot
dfCurrent <- df2012

dfNoWeekEND <-subset(df, strptime(df$date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))
#get rid of weekends

dfCurrent12 <- subset(df2012, strptime(df2012$date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))
dfCurrent13 <- subset(df2013, strptime(df2013$date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))
dfCurrent14 <- subset(df2014, strptime(df2014$date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))
par(mfrow = c(3,1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0))

fnCalendarFlow(dfCurrent12)
fnCalendarFlow(dfCurrent13)
fnCalendarFlow(dfCurrent14)
