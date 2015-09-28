# Usage: data = getData(url, dims)
# Before: nothing
# After: data is the table retrieved from url with the query dims 
getData <- function(url, dims) {
  table <- data.table(get_pxweb_data(
    url=url, 
    dims=dims, 
    clean=FALSE
  ))
  return(as.data.frame(table))
}

# Usage: newTable = setUpFinalTable(table, colName)
# Before: table is a table where the first column is the date (in years),
#         and the first row (name of columns) is the age. That is:
#             age age age
#       year
#       year
#       year
# After: newTable is a new table with three columns: Date(year)
#        Age and the main data itself
setUpFinalTable <- function(table, colName) {
  Date <- numeric()
  for(i in (1:nrow(table))) {
    Date <- c(Date, rep(as.character(table[i,1]), ncol(table)-1))
  }
  Date = as.numeric(Date)

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

# Usage: newTable = transposeTable(table)
# Before: table looks like this:
#        z      x x
#        y
#        y
# After: newTable looks like this:
#        Date   y y
#        x
#        x
#        That is, newTable is the transpose of table,
#        and the name of the first column is "Date"
transposeTable <- function(table) {
  firstColumn <- names(table)
  newTable <- data.frame("Date" = firstColumn[2:length(firstColumn)])
  
  for(i in(1:nrow(table))) {
    newTable[, as.character(table[i,1])] <- 
      as.numeric(table[i,2:ncol(table)])
  }
  return(newTable)
}

# Usage: newList = fixAgeString(list, stringFunction)
# Before: the strings in list are age intervals, and the function
#         stringFunction can convert each string  to the right
#         format (i.e. "14>", "15-19", "60<")
# After: newList contains all the strings in list, but in the right
#        format
fixAgeString <- function(list, correctStringFunction) {
  for(i in (1:length(list))) {
    list[i] <- correctStringFunction(list[i])
  }
  return(list)
}

# Usage: newStr = getCorrectString1(str)
# Before: str can be in the format: "14 and under", "15-19" and 
#         "60 years and over"
# After: newStr is str but in the right format: "14>", "15-19" and "60<"
getCorrectString1 <- function(str) {
  strSplit <- strsplit(str, " ")
  strSplit <- strSplit[[1]]
  
  newString <- ""
  if("under" %in% strSplit) { 
    newString <- paste0(strSplit[1], ">")
  } 
  else if("over" %in% strSplit){
    newString <- paste0(strSplit[1], "<")
  } 
  else if(length(strSplit) == 2) {
    newString <- strSplit[1]
  }
  else {
    newString <- str
  }
  
  return(newString)
}

# Usage: newStr = getCorrectString2(str)
# Before: str can be in the format: "14.years", "15.19.years" and 
#         "60.years.and.older"
# After: newStr is str but in the right format: "14>", "15-19"
#        and "60<"
getCorrectString2 <- function(str) {
  strSplit <- strsplit(str, "\\.")
  strSplit <- strSplit[[1]]
  
  newString <- ""
  if(length(strSplit) == 2) {
    newString <- paste0(strSplit[1], ">")
  }
  else if(length(strSplit) == 3) {
    newString <- paste0(strSplit[1], "-", strSplit[2])
  }
  else if (length(strSplit) == 4) {
    newString <- paste0(strSplit[1], "<")
  }
  return(newString)
}

# Usage: newStr = getCorrectString3(str)
# Before: str can be in the format: "< 14 years", "15-19 years" 
#         and "60+ years"
# After: newStr is str but in the right format: "14>", "15-19" 
#        and "60<"
getCorrectString3 <- function(str) {
  strSplit <- strsplit(str, " ")
  strSplit <- strSplit[[1]]
  
  newString <- ""
  if("<" %in% strSplit) { 
    newString <- paste0(strSplit[2], ">")
  } 
  else if(grepl("\\+", str)){
    strSplit <- strsplit(gsub("\\+","",str), " ")
    strSplit <- strSplit[[1]]
    newString <- paste0(strSplit[1], "<")
  } 
  else {
    newString <- strSplit[1]
  }
  
  return(newString)
}

# Usage: newList = removeFirstWords(list)
# Before: the strings in list contain ".", e.g. "Women.2001", 
#         "x.35.39.years" and "bla.asdf.12.lorem.ipsum"
# After: every string is list has been cut, so that the only thing
#        that is left is the part after the first index of a number. 
#        e.g. "2001", "12.lorem.ipsum" and "35.39.years"
removeFirstWords <- function(list) {
  newList = c()
  for(i in (1:length(list))) {
    idx = getIndexOfFirstNumber(list[i])
    temp = substring(list[i], idx)
    newList[i] = temp
  }
  return(newList)
}

# Usage: idx = getIndexOfFirstNumber(str)
# Before: str contains at least one number
# After: idx is the index of where the first number is in str
getIndexOfFirstNumber <- function(str) {
  idx  = c()
  for(i in(0: nchar(str))) {
    temp = lapply(strsplit(str, ''), 
                  function(x) which(x == toString(i)))
    temp = temp[[1]][1]
    idx = c(idx, temp)
  }
  return(min(idx, na.rm=TRUE))
}

# Usage: newTable = removeColumn(table, x)
# Before: nothing
# After: newTable is table but where the first x columns 
#        have been removed
removeColumn <- function(table, x) {
  return(table[,(x+1):ncol(table)])
}

# Usage: newList = removeLatterWords(list)
# Before: nothing
# After: each string item in newList contains the first word
#        in the correspondin string in list. That is goes from
#        "word1 word2.." to "word1"
removeLatterWords <- function(list)  {
  newList <- c()
  for(i in (1:length(list))) {
    strSplit <- strsplit(list[i], " ")
    strSplit <- strSplit[[1]]
    newList[i] = strSplit[1]
  }
  return(as.numeric(newList))
}

# Usage: newTable = aggregateByAge(table)
# Before: the first column in table is a list of age, and the
#         first row is a list of date (year), that is:
#         age   year  year  year
#         15
#         ..
#         19
# After: each column in newTable has the mean of 5 years from
#        the data in table
#         age   year  year  year
#         15-19 mean  mean  mean
aggregateByAge <- function(table) {
  ageList <- table[[1]]
  lowestAge <- ageList[1]
  
  #find the index of the first number that is a multiply of 5
  lowestAgeMark <- -1
  for(i in (0:4)) {
    if((lowestAge+i) %% 5 == 0) {
      lowestAgeMark <- 1+i
      break
    }
  }
  
  #creating a list of age intervals
  idx <- lowestAgeMark
  tempAgeList <- c()
  while(idx+4 <= nrow(table)) {
    temp <- paste0(ageList[idx], "-", ageList[idx+4])
    tempAgeList <- c(tempAgeList, temp)
    idx <- idx+5
  }
  
  #our new table with age intervals
  newTable <- data.frame(Age <- tempAgeList)
  
  #creating a list of means and adding them to the new table
  for(i in (2:ncol(table))) {
    idx <- lowestAgeMark
    tempList <- c()
    while(idx+4 <= nrow(table)) {
      temp <- mean(table[idx:idx+4, i])
      tempList = c(tempList, temp)
      idx <- idx+5
    }
    name <- names(table)[i]
    newTable[i] <- tempList
  }
  names(newTable)[2:ncol(table)] <- names(table)[2:ncol(table)]
  
  return(newTable)
}