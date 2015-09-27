library(googleVis)

smaprufa <- read.csv('litidtest.csv', sep=";")

Motion=gvisMotionChart(total, 
                       idvar="Age", 
                       timevar="Date",
                       sizevar="eignir")
plot(Motion)
