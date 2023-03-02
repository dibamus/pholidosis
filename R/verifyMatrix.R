#Find problems in scale table
verifyMatrix <- function(df, verbose = TRUE){
  require("tibble")
  if(!has_rownames(df)){
    df <- df %>% column_to_rownames("scale name")
  }

  #delete extra, erroneous columns
  if(length(colnames(df)) > length(row.names(df))){
    if(verbose){cat("\n more columns than rows
        \n deleting unmatched columns.\n")}
    mismatched <- which(!(colnames(df) %in% row.names(df)))
    mismatched <- mismatched[which(mismatched > length(row.names(df)))]
    df <- df[,-mismatched]

  }

  #are all the column and row names matched?
  if(all(colnames(df) == row.names(df))){
    if(verbose){cat("\n > column & row names match\n")}
  }
  else{
    mismatched <- which(colnames(df) != row.names(df))
    check_names <- data.frame(
      columnName = colnames(df)[mismatched],
      index = mismatched,
      rowName = row.names(df)[mismatched])
    if(verbose){cat("\n mismatched column & row names \n")
      print(check_names)

      cat("\n deleting false columns\n")}
    df <- df[,-check_names$index[which(is.na(check_names$rowName))]]

    if(verbose){cat("\n reverifying matrix")}
    verifyMatrix(df[,-emptycol])
  }

  #are there non-numeric data in the matrix?

  if("character" %in%
     sapply(colnames(df), function(x){typeof(df[[x]])}) %>%
     unique){
    #which columns contain character data rather than numeric data?
    badCol<- which(sapply(colnames(df), function(x){typeof(df[[x]])}) == "character")

    badEntries <- df[,badCol] %>% unique()

    if(verbose){cat("\n > unable to proceed \n
        non-numeric entries in martix: \n\n")
    print(badEntries)}
    return(data.frame("bad.input.matrix" = 1))
  }

  return(df)

}

