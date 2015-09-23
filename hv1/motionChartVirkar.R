library(googleVis)

smaprufa <- read.csv('litidtest.csv', sep=";")

Motion=gvisMotionChart(smaprufa, 
                       idvar="Hopur", 
                       timevar="Date",
                       sizevar="eignir")
plot(Motion)
