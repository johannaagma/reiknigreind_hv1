library(shiny)
library(googleVis)

smaprufa <- read.csv('litidtest.csv', sep=";")

#==============================================

source("helpFunctions.R")

hjonavigslur_kvk <- read.csv("data/hjonavigslur.csv", skip=2, sep=";")
hjonavigslur_kvk <- hjonavigslur_kvk[,2:ncol(hjonavigslur_kvk)]
hjonavigslur_kvk <- makeDataFrame(hjonavigslur_kvk)

tekjur_kvk <- read.csv("data/tekjur.csv", skip=2, sep=";")
tekjur_kvk <- tekjur_kvk[,2:ncol(tekjur_kvk)]
tekjur_kvk <- transposeTable(tekjur_kvk)
tekjur_kvk <- renameColumn(tekjur_kvk, 1)
tekjur_kvk <- makeDataFrame(tekjur_kvk)

