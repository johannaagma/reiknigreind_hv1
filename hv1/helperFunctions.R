# Notkun: data = getData(url, dims)
# Fyrir: ekkert
# Eftir: data eru dataFrame af gögnunum frá url, með query-ið dims
getData <- function(url, dims) {
  table <- data.table(get_pxweb_data(
    url=url, 
    dims=dims, 
    clean=FALSE
  ))
  return(as.data.frame(table))
}

# Notkun: newTable = setUpFinalTable(table, colName)
# Fyrir: table er tafla þar sem fyrsti dálkurinn er ártal,
#        og hinu dálkarnir eru aldurshópar. Þ.e.:
#           aldur aldur aldur
#       ár
#       ár
#       ár
# Eftir: newTable er dataframe með Date(ártal), 
#        Age(aldursbil) og colName(meginupplýsingarnar úr table)
setUpFinalTable <- function(table, colName) {
  Date <- numeric()
  for(i in (1:nrow(table))) {
    Date <- c(Date, rep(table$Ár[i], ncol(table)-1))
  }
  
  Age <- rep(names(table)[2:ncol(table)], nrow(table))
  
  Data <- numeric()
  for(i in (1:nrow(table))) {
    for(j in (2:ncol(table))) {
      Data <- c(Data, table[i,j])
    }
  }
  dataFrame <- data.frame(Date, Age, Data)
  colnames(dataFrame)[3] = colName
  return(dataFrame)
}

# Notkun: newTable = transposeTable(table)
# Fyrir: table lítur út svona:
#        z  x x
#        y
#        y
# Eftir: newTable lítur út svona:
#        Ár y y
#        x
#        x
#        Búið er að snúa töflunni við, og fyrsti dálkurinn
#        fær nafnið "Ár"
transposeTable <- function(table) {
  firstColumn <- names(table)
  newTable <- data.frame("Ár" = firstColumn[2:length(firstColumn)])
  
  for(i in(1:nrow(table))) {
    newTable[, as.character(table[i,1])] <- as.numeric(table[i,2:ncol(table)])
  }
  return(newTable)
}

# Notkun: newList = fixAgeString(list, function)
# Fyrir: strengirnir í list eru annaðhvort á forminu "X15.19" og "X60.ára.og.eldri"
#        og með fallið getCorrectString1, EÐA:
#        á forminu "15-19" og "60 ára og eldri" og fallið getCorrectString2
# Eftir: búið er að breyta þessum strengjunum yfir á formið: "15-19 ára" og
#        "60+ ára"
fixAgeString <- function(list, correctStringFunction) {
  for(i in (1:length(list))) {
    list[i] <- correctStringFunction(list[i])
  }
  return(list)
}

# Notkun: newStr = getCorrectString1(str)
# Fyrir: str er á forminu: "15-19" og "60 ára og eldri"
# Eftir: búið er að breyta string yfir á formið: "15-19 ára" og
#        "60+ ára"
getCorrectString1 <- function(str) {
  strSplit <- strsplit(str, " ")
  strSplit <- strSplit[[1]]
  
  newString <- ""
  if(length(strSplit) == 1) {
    newString <- paste0(strSplit[1], " ára")
  } 
  else if("yngri" %in% strSplit) { #ef það er yngri þarna
    newString <- paste0("< ",strSplit[1], " ára")
  } 
  else if("eldri" %in% strSplit){ #ef það er eldri þarna
    newString <- paste0(strSplit[1], "+ ára")
  } 
  else {
    newString <- str
  }
  
  return(newString)
}

# Notkun: newStr = getCorrectString2(str)
# Fyrir: str er á forminu: "X15.19" og "X60.ára.og.eldri"
# Eftir: búið er að breyta string yfir á formið: "15-19 ára" og
#        "60+ ára"
getCorrectString2 <- function(str) {
  interval <- strsplit(str, "\\.")
  newString <- ""
  if(length(interval[[1]]) == 2) {
    newString <- paste0(interval[[1]][1], "- ára")
  }
  else if(length(interval[[1]]) == 3) {
    newString <- paste0(interval[[1]][1], "-", interval[[1]][2]," ára")
  }
  else if (length(interval[[1]]) == 4) {
    newString <- paste0(interval[[1]][1], "+ ára")
  }
  return(newString)
}

# Notkun: newTable = removeFirstWords(list)
# Fyrir: strengirnir í list innihalda ".", t.d. 
#        "Konur.2001" eða "Alls.lifandi.fædd.börn.35.39.ára"
# Eftir: búið er að klippa alla strengi í list þannig að eftir er strengurinn
#        fyrir aftan fyrstu tilkomu tölustafs, t.d. "2001" og "35.39.ára"
removeFirstWords <- function(list) {
  newList = c() # eða = character()
  for(i in (1:length(list))) {
    idx = getIndexOfFirstNumber(list[i])
    temp = substring(list[i], idx)
    newList[i] = temp
  }
  return(newList)
}

# Notkun: idx = getIndexOfFirstNumber(str)
# Fyrir: str inniheldur amk eina tölu í sér
# Eftir: idx segir til um hvar fyrst tölustafurinn í str er
#        staðsettur
getIndexOfFirstNumber <- function(str) {
  idx  = c()
  for(i in(0: nchar(str))) {
    temp = lapply(strsplit(str, ''), function(x) which(x == toString(i)))
    temp = temp[[1]][1]
    idx = c(idx, temp)
  }
  return(min(idx, na.rm=TRUE))
}

# Notkun: newTable = removeFirstColumn(table)
# Fyrir: ekkert
# Eftir: búið er að fjarlægja fyrsta dálkinn í table
removeFirstColumn <- function(table) {
  return(table[,2:ncol(table)])
}