# Notkun: data = getData(url, dims)
# Fyrir: ekkert
# Eftir: data eru dataFrame af g�gnunum fr� url, me� query-i� dims
getData <- function(url, dims) {
  table <- data.table(get_pxweb_data(
    url=url, 
    dims=dims, 
    clean=FALSE
  ))
  return(as.data.frame(table))
}

# Notkun: newTable = setUpFinalTable(table, colName)
# Fyrir: table er tafla �ar sem fyrsti d�lkurinn er �rtal,
#        og hinu d�lkarnir eru aldursh�par. �.e.:
#           aldur aldur aldur
#       �r
#       �r
#       �r
# Eftir: newTable er dataframe me� Date(�rtal), 
#        Age(aldursbil) og colName(meginuppl�singarnar �r table)
setUpFinalTable <- function(table, colName) {
  Date <- numeric()
  for(i in (1:nrow(table))) {
    Date <- c(Date, rep(table$�r[i], ncol(table)-1))
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
# Fyrir: table l�tur �t svona:
#        z  x x
#        y
#        y
# Eftir: newTable l�tur �t svona:
#        �r y y
#        x
#        x
#        B�i� er a� sn�a t�flunni vi�, og fyrsti d�lkurinn
#        f�r nafni� "�r"
transposeTable <- function(table) {
  firstColumn <- names(table)
  newTable <- data.frame("�r" = firstColumn[2:length(firstColumn)])
  
  for(i in(1:nrow(table))) {
    newTable[, as.character(table[i,1])] <- as.numeric(table[i,2:ncol(table)])
  }
  return(newTable)
}

# Notkun: newTable = fixColNames_age(table)
# Fyrir: titlarnir � d�lkunum � table (fr� og me� d�lk 2) eru � 
#        forminu: "X15.19" og "X60.�ra.og.eldri"
# Eftir: b�i� er a� breyta �essum titlum yfir � formi�: "15-19 �ra" og
#        "60+ �ra"
fixColNames_age <- function(table) {
  for(i in (2:ncol(table))) {
    names(table)[i] <- getCorrectString(names(table)[i])
  }
  return(table)
}

# Notkun: newStr = getCorrectString(str)
# Fyrir: str er � forminu: "X15.19" og "X60.�ra.og.eldri"
# Eftir: b�i� er a� breyta string yfir � formi�: "15-19 �ra" og
#        "60+ �ra"
getCorrectString <- function(str) {
  interval <- strsplit(str, "\\.")
  newString <- ""
  if(length(interval[[1]]) == 2) {
    newString <- paste0(interval[[1]][1], "- �ra")
  }
  else if(length(interval[[1]]) == 3) {
    newString <- paste0(interval[[1]][1], "-", interval[[1]][2]," �ra")
  }
  else if (length(interval[[1]]) == 4) {
    newString <- paste0(interval[[1]][1], "+ �ra")
  }
  return(newString)
}


# Notkun: newTable = renameVector(vector)
# Fyrir: gildin � vector eru strengir sem innihalda ".", t.d. 
#        "Konur.2001" e�a "Alls.lifandi.f�dd.b�rn.35.39.�ra"
# Eftir: b�i� er a� setja fyrsta gildi� sem gefur eftir fyrstu tilkomu
#        t�lustafs, t.d. "2001" og "35.39.�ra"
renameVector <- function(vector) {
  list = c() # e�a = character()
  for(i in (1:length(vector))) {
    idx = getIndexOfFirstNumber(vector[i])
    temp = substring(vector[i], idx)
    list[i] = temp
  }
  return(list)
}

# Notkun: idx = getIndexOfFirstNumber(str)
# Fyrir: str inniheldur amk eina t�lu � s�r
# Eftir: idx segir til um hvar fyrst t�lustafurinn � str er
#        sta�settur
getIndexOfFirstNumber <- function(str) {
  idx  = c()
  for(i in(0: nchar(str))) {
    temp = lapply(strsplit(str, ''), function(x) which(x == toString(i)))
    temp = temp[[1]][1]
    idx = c(idx, temp)
  }
  return(min(idx, na.rm=TRUE))
}