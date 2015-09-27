library(shiny)
library(googleVis)
library(data.table)
library(pxweb)
source("helperFunctions.R")

smaprufa <- read.csv('litidtest.csv', sep=";")

#===================================
# Hjónavígslur eftir aldri brúðhjóna
#===================================
hjonavigslur_kvk <- getData("http://px.hagstofa.is/pxis/api/v1/is/Ibuar/fjolsk/Giftingar/MAN06101.px",
                           list("Kyn"="1", "Ár"=as.character(c(8:48)), "Aldur"=as.character(c(1:10))))
hjonavigslur_kvk <- removeFirstColumn(hjonavigslur_kvk)
names(hjonavigslur_kvk)[2:ncol(hjonavigslur_kvk)] <- 
  fixAgeString(names(hjonavigslur_kvk)[2:ncol(hjonavigslur_kvk)], getCorrectString1)
hjonavigslur_kvk <- setUpFinalTable(hjonavigslur_kvk, "Hjonavigslur_kvk")

#===============================
# Lögskilnaðir eftir aldri hjóna
#===============================
logskilnadur_kvk <- getData("http://px.hagstofa.is/pxis/api/v1/is/Ibuar/fjolsk/Skilnadir/MAN06202.px",
                            list("Kyn"="1", "Ár"=as.character(c(10:20)), "Aldur"=as.character(c(1:10))))
logskilnadur_kvk <- removeFirstColumn(logskilnadur_kvk)
names(logskilnadur_kvk)[2:ncol(logskilnadur_kvk)] <- 
  fixAgeString(names(logskilnadur_kvk)[2:ncol(logskilnadur_kvk)], getCorrectString1)
logskilnadur_kvk <- setUpFinalTable(logskilnadur_kvk, "Logskilnadur_kvk")

#====================================================
# Meðalatvinnutekjur í aðalstarfi eftir kyni og aldri
#====================================================
##TODO breyta 0-15 í <15
tekjur_kvk <- getData("http://px.hagstofa.is/pxis/api/v1/is/Samfelag/launogtekjur/3_tekjur/VIN07002.px",
                      list("Landshlutar"="0", "Aldur"=as.character(c(1:14)),"Kyn"="2", "Ár"=c("*")))
tekjur_kvk <- removeFirstColumn(tekjur_kvk)
tekjur_kvk <- transposeTable(tekjur_kvk)
tekjur_kvk[,1] <- removeFirstWords(as.character(tekjur_kvk[,1]))
tekjur_kvk[,1] <- as.numeric(tekjur_kvk[,1])
tekjur_kvk <- setUpFinalTable(tekjur_kvk, "Tekjur_kvk")

#=====================================================================================
# Skuldir, eignir og eiginfjárstaða einstaklinga eftir fjölskyldugerð, aldri og búsetu
#=====================================================================================
skuldir <- getData("http://px.hagstofa.is/pxis/api/v1/is/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09000.px",
                      list("Ár"=c("*"),
                           "Fjölskyldugerð, aldur og búseta"=as.character(c(5:14)), 
                           "Skuldir, eignir og eiginfjárstaða"="8"))
colnames(skuldir) <- c("Date", "Age", "Skuldir")

#==============
# Fæðingartíðni
#==============
faeding <- getData("http://px.hagstofa.is/pxis/api/v1/is/Ibuar/Faeddir/faedingar/MAN05201.px",
                   list("Ár"=as.character(c(149:193)),
                        "Aldur"=c("*")))
faeding <- setUpFinalTable(faeding, "Faedingartidni")

#=================
# Merging the data
#=================
total <- merge(hjonavigslur_kvk, logskilnadur_kvk, by=c("Date", "Age"), all=TRUE)
total <- merge(total, tekjur_kvk, by=c("Date", "Age"), all=TRUE)
total <- merge(total, skuldir, by=c("Date", "Age"), all=TRUE)
total <- merge(total, faeding, by=c("Date", "Age"), all=TRUE)




