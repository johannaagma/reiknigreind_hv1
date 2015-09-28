library(shiny)
library(googleVis)
library(data.table)
library(pxweb)
source("helperFunctions.R")

#=================================================================
# Marriages by age of spouses (female)
#=================================================================
marriages_f_TITLE <- "Marriages of spouses"
marriages_f <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/fjolsk/Giftingar/MAN06101.px",
  list("Sex"="1", 
       "Year"=as.character(c(8:48)), 
       "Age"=as.character(c(1:10))))
marriages_f <- removeColumns(marriages_f, 1)

names(marriages_f)[2:ncol(marriages_f)] <- 
  fixAgeString(names(marriages_f)[2:ncol(marriages_f)], getCorrectString1)
marriages_f <- setUpFinalTable(marriages_f, marriages_f_TITLE)

#=================================================================
# Divorces by age of couples (female)
#=================================================================
divorces_f_TITLE <- "Divorces of couples"
divorces_f <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/fjolsk/Skilnadir/MAN06202.px",
  list("Sex"="1", 
       "Year"=as.character(c(10:20)), 
       "Age"=as.character(c(1:10))))
divorces_f <- removeColumns(divorces_f, 1)
names(divorces_f)[2:ncol(divorces_f)] <- 
  fixAgeString(names(divorces_f)[2:ncol(divorces_f)], getCorrectString1)
divorces_f <- setUpFinalTable(divorces_f, divorces_f_TITLE)

#=================================================================
# Income and expenses by family type, age and residence
#=================================================================
annualIncome_TITLE <- "Total annual income [Million ISK]"
annualIncome <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09001ens.px",
  list("Year"=c("*"), 
       "Family type, age and residence"=as.character(c(5:14)),
       "Income and Expenses"="1"))
names(annualIncome) <- c("Date", "Age", annualIncome_TITLE)
annualIncome$Age <- fixAgeString(annualIncome$Age, getCorrectString3)

#=================================================================
# Liabilities (average)
#=================================================================
liabilities_TITLE <- "Liabilities of individuals [Million ISK]"
liabilities <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09000ens.px",
  list("Year"=c("*"),
       "Family type, age and residence"=as.character(c(5:14)), 
       "Liabilities, Assets and Net worth"="9"))
colnames(liabilities) <- c("Date", "Age", liabilities_TITLE)
liabilities$Age <- fixAgeString(liabilities$Age, getCorrectString3)


#=================================================================
# Assets (average)
#=================================================================
assets_TITLE <- "Assets of individuals [Million ISK]"
assets <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09000ens.px",
  list("Year"=c("*"),
       "Family type, age and residence"=as.character(c(5:14)), 
       "Liabilities, Assets and Net worth"="1"))
colnames(assets) <- c("Date", "Age", assets_TITLE)
assets$Age <- fixAgeString(assets$Age, getCorrectString3)


#=================================================================
# Fertility
#=================================================================
fertility_TITLE <- "Fertility [pr 1000 women]"
fertility <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/Faeddir/faedingar/MAN05201.px",
  list("Year"=as.character(c(149:193)),
       "Age"=c("*")))
names(fertility)[1] <- "Date"
names(fertility)[2:ncol(fertility)] <- 
  fixAgeString(names(fertility)[2:ncol(fertility)], getCorrectString3)
fertility <- setUpFinalTable(fertility, fertility_TITLE)
fertility$Age <- sub(" years","",fertility$Age)

#=================================================================
# Students by level, field of study, age and sex
#=================================================================
students_TITLE <- "Students in University"
students <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Samfelag/skolamal/4_haskolastig/0_hsNemendur/SKO04106.px",
  list("ISCED level" = "Total",
       "Broad fields of study" = "Total",
       "Age"=as.character(c(15:90)),
       "Year"=c("*"),
       "Sex" = "Total"))
students <- removeColumns(students, 2)
students$Age <- removeLatterWords(students$Age)
names(students)[2:ncol(students)] <- 
  removeLatterWords(names(students)[2:ncol(students)])
students <- sumIntoAgeGroups1(students)
students <- transposeTable(students)
students <- setUpFinalTable(students, students_TITLE)

#=================================================================
# External migration
#=================================================================
migration_TITLE <- "Net immigration"
migration <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/buferlaflutningar/Buferlaflutningar/MAN01401.px",
  list("Sex" = "Total",
       "Year"=c("*"),
       "Age"=as.character(c(1:110)),
       "Citizenship" = "Total",
       "Type of migration" = "Net immigration"))
migration <- removeColumns(migration, 1)
migration$Age <- removeLatterWords(migration$Age)
migration$"Total Net immigration" <- replaceItemsInList(migration$"Total Net immigration", "-", 0)
migration <- sumIntoAgeGroups2(migration)
names(migration) <- c("Date", "Age", migration_TITLE)

#=================================================================
# Merging the data
#=================================================================
allData <- merge(marriages_f, divorces_f, by=c("Date", "Age"), all=TRUE)
allData <- merge(allData, annualIncome, by=c("Date", "Age"), all=TRUE)
allData <- merge(allData, liabilities, by=c("Date", "Age"), all=TRUE)
allData <- merge(allData, fertility, by=c("Date", "Age"), all=TRUE)
allData <- merge(allData, assets, by=c("Date", "Age"), all=TRUE)
allData <- merge(allData, students, by=c("Date", "Age"), all=TRUE)
allData <- merge(allData, migration, by=c("Date", "Age"), all=TRUE)