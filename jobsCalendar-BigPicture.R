rm(list=ls())
setwd('/Users/zurich/Google Drive/SITES/FactMachine-Final/CalendarHeatmap')
library(timeDate)
library(plyr)
library(ggplot2)

data <- read.csv('Alljob.csv', stringsAsFactors = FALSE)
date <- as.Date(data$RealDate, format = "%d.%m.%y")

df <-data.frame(date = date, jobs = data$Jobs, holiday = data$Holiday)
#filter in Monday...Friday only
dfFiltered <- subset(df, strptime(date,  "%Y-%m-%d")$wday %in% c(1,2,3,4,5))
#filter out public holidays
dfFiltered <- subset(dfFiltered, holiday == FALSE)
#filter out zeros
#dfFiltered <- subset(dfFiltered, jobs != 0)


# Add WeekNumber
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

dfCurrent <- dfFiltered




mm <- ddply(dfCurrent, "cumWeek", summarise, meanJob=mean(jobs))

gap <- range(mm$meanJob)[2] - range(mm$meanJob)[1]
min <- range(mm$meanJob)[1]
mm$scaleMean <- (mm$meanJob - min)/ gap
#get rid of zeros
mm <- subset(mm, mm$meanJob > 0.1)

#get rid of very low weekly scores
mm$zMean <- scale(mm$meanJob)

#get rid of very low weekly scores
mm <- subset(mm, mm$zMean > -2.33)


#calculate the resultant mean
mean <- mean(mm$meanJob)
#create deviations from the mean
mm$devMean <- mm$meanJob - mean
#create boolean for above or below mean
mm$DevBool <- (mm$devMean >= 0)

m <- ggplot(mm, aes(x = factor(cumWeek), y= devMean, fill= DevBool))
m <- m + geom_bar(stat="identity", position="identity", colour="Grey", size = 0.25)
m <- m + scale_fill_manual(values=c("#E41A1C", "#377EB8"), guide=FALSE)
m <- m + theme(panel.background = element_rect(fill="white"))
m <- m + theme(panel.grid.minor = element_line(colour="blue", linetype="dashed", size=0.1))
m <- m + theme(panel.grid.major = element_line(colour="blue", linetype="dashed", size=0.2))
m <- m + theme(panel.grid.major.x = element_blank())
#m <- m + stat_smooth(se = FALSE, aes(group = 1), n=30, fill="red")
m <- m + scale_x_discrete(breaks=mm$cumWeek)
m

ggsave(file = "bp.pdf",  useDingbats=FALSE)










