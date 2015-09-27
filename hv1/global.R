library(shiny)
library(googleVis)
library(data.table)
library(pxweb)
source("helperFunctions.R")

#=================================================================
# Marriages by age of spouses (female)
#=================================================================
marriages_f_TITLE <- "Marriages of spouses (female)"
marriages_f <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/fjolsk/Giftingar/MAN06101.px",
  list("Sex"="1", 
       "Year"=as.character(c(8:48)), 
       "Age"=as.character(c(1:10))))
marriages_f <- removeFirstColumn(marriages_f)

names(marriages_f)[2:ncol(marriages_f)] <- 
  fixAgeString(names(marriages_f)[2:ncol(marriages_f)], getCorrectString1)
marriages_f <- setUpFinalTable(marriages_f, marriages_f_TITLE)

#=================================================================
# Divorces by age of couples (female)
#=================================================================
divorces_f_TITLE <- "Divorces of couples (female)"
divorces_f <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/fjolsk/Skilnadir/MAN06202.px",
  list("Sex"="1", 
       "Year"=as.character(c(10:20)), 
       "Age"=as.character(c(1:10))))
divorces_f <- removeFirstColumn(divorces_f)
names(divorces_f)[2:ncol(divorces_f)] <- 
  fixAgeString(names(divorces_f)[2:ncol(divorces_f)], getCorrectString1)
divorces_f <- setUpFinalTable(divorces_f, divorces_f_TITLE)

#=================================================================
# Income and expenses by family type, age and residence
#=================================================================
#TODO breyta 0-15 í <15
annualIncome_TITLE <- "Total annual income [Million ISK]"
annualIncome <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09001ens.px",
  list("Year"=c("*"), 
       "Family type, age and residence"=as.character(c(5:14)),
       "Income and Expenses"="0"))
names(annualIncome) <- c("Date", "Age", annualIncome_TITLE)
annualIncome$Age <- fixAgeString(annualIncome$Age, getCorrectString3)

#=================================================================
# Liabilities, assets and net worth of individuals by family type, 
# age and residence (total liabilities)
#=================================================================
liabilities_TITLE <- "Liabilities of individuals [Million ISK]"
liabilities <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09000ens.px",
  list("Year"=c("*"),
       "Family type, age and residence"=as.character(c(5:14)), 
       "Liabilities, Assets and Net worth"="8"))
colnames(liabilities) <- c("Date", "Age", liabilities_TITLE)
liabilities$Age <- fixAgeString(liabilities$Age, getCorrectString3)

#=================================================================
# Fertility
#=================================================================
fertility_TITLE <- "Fertility [pr 1000 women]"
fertility <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/Faeddir/faedingar/MAN05201.px",
  list("Year"=as.character(c(149:193)),
       "Age"=c("*")))
fertility <- setUpFinalTable(fertility, fertility_TITLE)

#=================================================================
# Merging the data
#=================================================================
total <- merge(marriages_f, divorces_f, by=c("Date", "Age"), all=TRUE)
total <- merge(total, annualIncome, by=c("Date", "Age"), all=TRUE)
total <- merge(total, liabilities, by=c("Date", "Age"), all=TRUE)
total <- merge(total, fertility, by=c("Date", "Age"), all=TRUE)