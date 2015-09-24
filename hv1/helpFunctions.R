# Notkun: newTable = makeDataFrame(table)
# Fyrir: table er tafla þar sem fyrsti dálkurinn er ártal,
#        og hinu dálkarnir eru aldurshópar. Þ.e.:
#           aldur aldur aldur
#       ár
#       ár
#       ár
# Eftir: newTable er dataframe með Date(ártal), 
#        Age(aldursbil) og Data(meginupplýsingar úr table)
makeDataFrame <- function(table) {
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
  
  return(data.frame(Date, Age, Data))
}

# Notkun: newTable = transposeTable(table)
# Fyrir: table lítur út svona:
#          x x
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

# Notkun: newTable = renameColumn(table, 1)
#
#
renameColumn <- function(table, col) {
  table[,col] = as.character(table[,col])
  for(i in (1:nrow(table))) {
    table[i,col] = strsplit(table[i,col], "\\.")[[1]][2]
  }
  return(table)
}


