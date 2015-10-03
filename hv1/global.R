library(shiny)
library(googleVis)
library(data.table)
library(pxweb)
library(rdatamarket)
library(plyr)
source("helperFunctions.R")

#=================================================================
# Marriages by age of spouses (female)
#=================================================================
marriages_f_TITLE <- "Marriages of spouses"
marriages_f <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/fjolsk/Giftingar/MAN06101.px",
  list("Sex"="1", 
       "Year"=as.character(c(8:48)), 
       "Age"=as.character(c(1:10))),
  "backupData/marriages_f.csv")
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
       "Age"=as.character(c(1:10))),
  "backupData/divorces_f.csv")
divorces_f <- removeColumns(divorces_f, 1)
names(divorces_f)[2:ncol(divorces_f)] <- 
  fixAgeString(names(divorces_f)[2:ncol(divorces_f)], getCorrectString1)
divorces_f <- setUpFinalTable(divorces_f, divorces_f_TITLE)

#=================================================================
# Income and expenses (average)
#=================================================================
annualIncome_TITLE <- "Total annual income [Million ISK]"
annualIncome <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09001ens.px",
  list("Year"=c("*"), 
       "Family type, age and residence"=as.character(c(5:14)),
       "Income and Expenses"="1"),
  "backupData/annualIncome.csv")
names(annualIncome) <- c("Date", "Age", annualIncome_TITLE)
annualIncome$Age <- fixAgeString(as.character(annualIncome$Age), getCorrectString3)

#=================================================================
# Liabilities (average)
#=================================================================
liabilities_TITLE <- "Liabilities of individuals [Million ISK]"
liabilities <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09000ens.px",
  list("Year"=c("*"),
       "Family type, age and residence"=as.character(c(5:14)), 
       "Liabilities, Assets and Net worth"="9"),
  "backupData/liabilities.csv")
colnames(liabilities) <- c("Date", "Age", liabilities_TITLE)
liabilities$Age <- fixAgeString(as.character(liabilities$Age), getCorrectString3)

#=================================================================
# Assets (average)
#=================================================================
assets_TITLE <- "Assets of individuals [Million ISK]"
assets <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/thjodhagsreikningar/skuldastada_heimili/THJ09000ens.px",
  list("Year"=c("*"),
       "Family type, age and residence"=as.character(c(5:14)), 
       "Liabilities, Assets and Net worth"="1"),
  "backupData/assets.csv")
colnames(assets) <- c("Date", "Age", assets_TITLE)
assets$Age <- fixAgeString(as.character(assets$Age), getCorrectString3)

#=================================================================
# Fertility
#=================================================================
fertility_TITLE <- "Fertility [pr 1000 women]"
fertility <- getData(
  "http://px.hagstofa.is/pxen/api/v1/en/Ibuar/Faeddirdanir/faeddir/faedingar/MAN05201.px",
  list("Year"=as.character(c(159:193)),
       "Age"=as.character(c(1:7))),
  "backupData/fertility.csv")
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
       "Sex" = "Total"),
  "backupData/students.csv")
students <- removeColumns(students, 2)
students$Age <- removeLatterWords(as.character(students$Age))
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
       "Type of migration" = "Net immigration"),
  "backupData/migration.csv")
migration <- removeColumns(migration, 1)
migration$Age <- removeLatterWords(as.character(migration$Age))
migration$"Total Net immigration" <- replaceItemsInList(migration$"Total Net immigration", "-", 0)
migration <- sumIntoAgeGroups2(migration)
names(migration) <- c("Date", "Age", migration_TITLE)

#=================================================================
# Unemployment (Hofudborgarsvaedid)
#=================================================================
unemployment_TITLE <- "Unemployment (Capital area)"
unemployment <- tryCatch(
  {
    dmlist("https://datamarket.com/data/set/14/atvinnulausir-eftir-aldri-og-busetu")
  },
  error = function(cond) {
    message("Error: Could not upload data, using backup data.")
    return(read.csv("backupData/unemployment.csv", sep=",", check.names=FALSE))
  },
  warning = function(cond) { },
  finally = { }
)
unemployment <- unemployment[!(unemployment[1] == "Samtals"),]
unemployment <- unemployment[unemployment[2] == "Höfuðborgarsvæðið",] #Hofudborgarsvaedid
unemployment <- removeColumn(unemployment, 2)
unemployment <- switchColumns(unemployment, 1, 2)
names(unemployment) <- c("Date", "Age", unemployment_TITLE)
unemployment[1] <- removeMonthFromDate(as.character(unemployment[[1]]))
unemployment[2] <- fixAgeString(as.character(unemployment[[2]]), getCorrectString3)
unemployment <- ddply(unemployment, .(Date, Age), numcolwise(mean)) #merging the rows
unemployment[3] <- round(unemployment[3])

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
allData <- merge(allData, unemployment, by=c("Date", "Age"), all=TRUE)