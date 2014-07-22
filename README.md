time-series-visualisation
=========================
*There are three R files used to create this visualisation*

- **JobsCalendar.R** Each working day is represented by a square
- **jobsCalendar-MonthlySummary.R** A bar graph is created for each week.
- **jobsCalendar-BigPicture.R** This creates a bar graph using GGPlot2. The daily observations are binned into weeks. 
An average is taken for a two and half year period and then a deviation is calculated from this average.


All R files import the data from *Alljob.csv*
