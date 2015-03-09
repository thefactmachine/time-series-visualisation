rm(list=ls())
library(timeDate)
library(plyr)
library(ggplot2)
library("RCurl")


x <- getURL("https://raw.githubusercontent.com/thefactmachine/time-series-visualisation/master/Alljob.csv")
data <- read.csv(text = x)



date <- as.Date(data$RealDate, format = "%d.%m.%y")

df <-data.frame(date = date, jobs = data$Jobs, holiday = data$Holiday)

#Do some filtering
#filter in Monday...Friday only
dfFiltered <- subset(df, strptime(date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))
#filter out public holidays
dfFiltered <- subset(dfFiltered, holiday == FALSE)
#filter out zeros
dfFiltered <- subset(dfFiltered, jobs != 0)

# Add WeekNumber: 1) convert to timeDate 2) convert to day of year (i.e. 1..365) 
# 3) divide by 7 to get flowing week number 4) get celing to convert to integer
dfFiltered$weekNum <-ceiling(timeDate::dayOfYear(timeDate::timeDate(dfFiltered$date)) / 7)


# Add MonthNumber
dfFiltered$monthNum <- as.numeric(format(dfFiltered$date, "%m"))

# Add Year
dfFiltered$YearNum <- as.numeric(format(dfFiltered$date, "%Y"))


#Add Cum Month
dfFiltered$cumMonth <- ((dfFiltered$YearNum - 2012) * 12) + dfFiltered$monthNum

#Add Cum Year
dfFiltered$cumWeek <- ((dfFiltered$YearNum - 2012) * 52) + dfFiltered$weekNum


df2012 <- subset(dfFiltered, dfFiltered$YearNum == 2012)
df2013 <- subset(dfFiltered, dfFiltered$YearNum == 2013)
df2014 <- subset(dfFiltered, dfFiltered$YearNum == 2014)

#==============================
# CHANGE CHANGE CHANGE this for each year
#dfCurrent is about 247 observations; dfAll is about 792 observations
dfCurrent <- df2014
dfAll <- dfFiltered

#summarise by weekNum
mm <- ddply(dfCurrent, "weekNum", summarise, meanJob=mean(jobs))


all <- ddply(dfAll, "cumWeek", summarise, meanJob=mean(jobs))


#create scaling based on the entire period (3 years)
# gap is max - min
gap <- range(all$meanJob)[2] - range(all$meanJob)[1]

min <- range(all$meanJob)[1]

#add the scaled mean obtained globally to current year
mm$scaleMean <- (mm$meanJob - min)/ gap

# SetUp Basic Plot:  X = weekNum, Y = ScaledMean
p <- ggplot(mm, aes(x = factor(weekNum) , y = scaleMean * 4))

# Plot It
p <- p + geom_bar(stat = "identity", fill="#999999", colour="#999999")

# Add a LOESS curve
#p <- p + stat_smooth(se = FALSE, aes(group = 1), n=30, fill="red")

p <- p + coord_equal()

p <- p + theme(axis.line=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               legend.position="none",
               panel.background=element_blank(),
               panel.border=element_blank(),
               panel.grid.major=element_blank(),
               panel.grid.minor=element_blank(),
               plot.background=element_blank())
p

# CHANGE CHANGE CHANGE this for each year
ggsave(file = "2014Bar.pdf",  useDingbats=FALSE)

