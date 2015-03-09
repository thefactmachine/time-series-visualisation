rm(list=ls())
library(timeDate)
library(plyr)
library(ggplot2)
library("RCurl")

#get the data
x <- getURL("https://raw.githubusercontent.com/thefactmachine/time-series-visualisation/master/Alljob.csv")
data <- read.csv(text = x, , stringsAsFactors = FALSE)

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

# upto now the data set contains years: 2012, 2013, 2014 & 2015. Exclude 2015.
dfFiltered <- subset(dfFiltered, YearNum != 2015)


#Add Cum Month
dfFiltered$cumMonth <- ((dfFiltered$YearNum - 2012) * 12) + dfFiltered$monthNum

#Add Cum Year
dfFiltered$cumWeek <- ((dfFiltered$YearNum - 2012) * 52) + dfFiltered$weekNum


mm <- ddply(dfFiltered, "cumWeek", summarise, meanJob=mean(jobs))

gap <- range(mm$meanJob)[2] - range(mm$meanJob)[1]
min <- range(mm$meanJob)[1]
mm$scaleMean <- (mm$meanJob - min)/ gap

#get rid of zeros
mm <- subset(mm, mm$meanJob > 0.1)

#get rid of very low weekly scores.  First canvert to Z scores
mm$zMean <- scale(mm$meanJob)

#get rid of Z scores that are LESS than -2.33
mm <- subset(mm, mm$zMean > -2.33)


#calculate the resultant mean for all data
mean <- mean(mm$meanJob)
#create deviations from the mean
mm$devMean <- mm$meanJob - mean
#create boolean for above or below mean
mm$DevBool <- (mm$devMean >= 0)

# data is mm, x is cumWeek, y = deviations from mean. Fill TRUE / FALSE
m <- ggplot(mm, aes(x = factor(cumWeek), y= devMean, fill= DevBool))

# set up bar chart. size is width of grey border and width is width of border
m <- m + geom_bar(stat="identity", position="identity", colour="Grey", size = 0.1, width=0.8)

# this determines colors for the bars
m <- m + scale_fill_manual(values=c("#E41A1C", "#377EB8"), guide=FALSE)

m <- m + theme(panel.background = element_rect(fill="white"))
m <- m + theme(panel.grid.minor = element_line(colour="blue", linetype="dashed", size=0.1))
m <- m + theme(panel.grid.major = element_line(colour="blue", linetype="dashed", size=0.2))
m <- m + theme(panel.grid.major.x = element_blank())
#m <- m + stat_smooth(se = FALSE, aes(group = 1), n=30, fill="red")
m <- m + scale_x_discrete(breaks=mm$cumWeek)
# the following command is useful. It changes the aspect.
m <- m + coord_equal(1/1.5) 
m
ggsave(file = "bp.pdf",  useDingbats=FALSE)










