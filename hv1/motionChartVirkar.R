<<<<<<< HEAD
Motion = gvisMotionChart(
  allData, 
  idvar="Age", 
  timevar="Date")
=======
library(googleVis)

smaprufa <- read.csv('litidtest.csv', sep=";")

Motion=gvisMotionChart(total, 
                       idvar="Age", 
                       timevar="Date",
                       )
>>>>>>> origin/master
plot(Motion)
