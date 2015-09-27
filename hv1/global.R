library(shiny)
library(googleVis)
library(data.table)
library(pxweb)
source("helperFunctions.R")

smaprufa <- read.csv('litidtest.csv', sep=";")

#==============================================
# Hjónavígslur eftir aldri brúðhjóna 1971-2011
#==============================================
hjonavigslur_kvk <-getData("http://px.hagstofa.is/pxis/api/v1/is/Ibuar/fjolsk/Giftingar/MAN06101.px",
                           list("Kyn"="1", "Ár"=as.character(c(8:48)), "Aldur"=as.character(c(1:10))))
hjonavigslur_kvk <- removeFirstColumn(hjonavigslur_kvk)
#hjonavigslur_kvk <- fixColNames_age(hjonavigslur_kvk)
names(hjonavigslur_kvk)[2:ncol(hjonavigslur_kvk)] <- 
  fixAgeString(names(hjonavigslur_kvk)[2:ncol(hjonavigslur_kvk)], getCorrectString2)
hjonavigslur_kvk <- setUpFinalTable(hjonavigslur_kvk, "Hjonavigslur_kvk")

#=====================================================
# Meðalatvinnutekjur í aðalstarfi eftir kyni og aldri
#=====================================================
tekjur_kvk <- getData("http://px.hagstofa.is/pxis/api/v1/is/Samfelag/launogtekjur/3_tekjur/VIN07002.px",
                      list("Landshlutar"="0", "Aldur"=as.character(c(1:14)),"Kyn"="2", "Ár"=c("*")))
tekjur_kvk <- removeFirstColumn(tekjur_kvk)
tekjur_kvk <- transposeTable(tekjur_kvk)
tekjur_kvk[,1] <- removeFirstWords(as.character(tekjur_kvk[,1]))
tekjur_kvk <- setUpFinalTable(tekjur_kvk, "Tekjur_kvk")

total <- merge(hjonavigslur_kvk, tekjur_kvk, by=c("Date", "Age"))

