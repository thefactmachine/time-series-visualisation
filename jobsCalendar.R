rm(list=ls())
library("classInt")
library("RCurl")


fnCalendarFlow <- function(dfCurrent) {    
    #==============================================
    #span <- max - mix 
    span <- range(dfCurrent$jobs)[2] - range(dfCurrent$jobs)[1]
    
    # get the earliest date for dfCurrent
    startDate <- strptime(min(dfCurrent$date), "%Y-%m-%d")
    
    # get the latest date for dfCurrent
    endDate <- strptime(max(dfCurrent$date), "%Y-%m-%d")
    
    # for a whole year this will be 52 weeks
    timespan <- difftime(endDate, startDate, units="weeks")
    
    numweeks <- as.numeric(timespan, units="weeks") + 2
    numObservations <- nrow(dfCurrent)
    
    #blank plot
    plot(0, 0, type="n", xlim=c(0, numweeks), ylim=c(0, 8), asp=1, xaxt='n', yaxt='n', ann=FALSE, bty="n")
    
    # Helper function to find the last day of month in datestring
    lastDayOfMonth <- function(datestring, date.form = "%Y-%m-%d") {
        # PURPOSE: receives a date (i.e. 4th May) and returns the last day 
        #of the month (i.e. 31st May) in specified format.
        
        # convert a datastring to a "POSIXlt" "POSIXt" date
        thedate <- strptime(datestring, date.form)
        
        # extract the year (type = numeric)
        theyear <- thedate$year + 1900
        
        # "POSIXlt" "POSIXt"  is zero based so month 0 -> January
        themonth <- thedate$mon + 1
        
        # receives the year and month and returns the first day of that month / year
        themonth.posix <- as.POSIXct(paste(theyear, themonth, '1', sep='-'), format=date.form)
        
        # generates a two element(i.e. month) sequence of this month and next month. returns next month
        month.next <- seq(themonth.posix, length=2, by='1 month')[2]
        
        # generates a two element (i.e. day) sequence of today and yesterday, returns yesterday.
        last.day <- seq(month.next, length=2, by='-1 day')[2]
        
        # converts to the appropriate form
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
   # pal is a function
   pal <- colorRampPalette(c("#FFFFFF", "#0000cc")) # blue   
   
   #colorgrad is a vector of colors
   colorgrad <- pal(ncolors)   
   gridColor <- "white"
   
   # returns "classIntervals" object. ci$brks is the break point.  
   # Uses the entire dataset to calculate appropriate breaks. style = "fisher" implies clustering
   ci <- classIntervals(dfNoWeekEND$jobs, n = ncolors, style = "fisher")
    
    fnReturnColor <- function(intJobNumber)  {
      #given the number of jobs (i,e intJobNumber return a color in RGB format)  
      colorNumber <- findInterval(intJobNumber, ci$brks)
        cellcolor <- colorgrad[colorNumber]
        return(cellcolor)
    }
    
  # numObservations is the number of rows for the current year. Iterate through the number of days
    for (i in 1:numObservations) {
        # if there are more than zero jobs
        if (dfCurrent$jobs[i] > 0) {
          #get the date for the current iteration. In the correct format  
            currDate <- strptime(dfCurrent$date[i], "%Y-%m-%d")
            
            #wday ==> 0 for Sunday, 6 for Saturday
            dayofweek <- fnRemapDay(currDate$wday)
            
            # find the difference in weeks between current date and start date and then round up
            # after adding 1 day
            diff <- difftime(currDate, startDate, units="weeks") + startDate$wday/6
            weeknum <- ceiling(as.numeric(diff, units="weeks") )
            
            #get the cell colour
            cellcolor <- fnReturnColor(dfCurrent$jobs[i])
            
            # draw the bounding box and then colour it it
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
# following is if you want to read the data in locally.
#data <- read.csv("Alljob.csv")


#Read Date was a factor, change it to a string
data$RealDate <- as.character(data$RealDate)

# define a vector called date
date <- as.Date(data$RealDate, format = "%d.%m.%y")

# contruct a data frame with all years
df <-data.frame(date = date, jobs = data$Jobs, holiday = data$Holiday)

# now create subsets for three years
df2012 <- subset(df, as.numeric(format(df$date, "%Y")) == 2012)
df2013 <- subset(df, as.numeric(format(df$date, "%Y")) == 2013)
df2014 <- subset(df, as.numeric(format(df$date, "%Y")) == 2014)

# combined data frame with all years.
dfNoWeekEND <-subset(df, strptime(df$date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))

# now just include 3 years 112 = 2012
dfNoWeekEND <-subset(df, strptime(df$date,  "%Y-%m-%d")$year %in% c(112,113,114))

#get rid of weekends
dfCurrent12 <- subset(df2012, strptime(df2012$date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))
dfCurrent13 <- subset(df2013, strptime(df2013$date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))
dfCurrent14 <- subset(df2014, strptime(df2014$date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))

# mfrow c(nRows, nColumns)
# mar gives outer margins in the form of c(bottom, left, top, right)

par(mfrow = c(3,1)  , mar=c(0, 0, 0, 0))

fnCalendarFlow(dfCurrent12)
fnCalendarFlow(dfCurrent13)
fnCalendarFlow(dfCurrent14)
