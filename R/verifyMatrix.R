#' verifyMatrix
#'
#' Find problems in an input adjacency matrix.
#'
#' Helpful when importing a matrix from an excel sheet that maybe you didn't
#' check every cell on.
#'
#' @import tidyverse
#' @importFrom tibble has_rownames column_to_rownames
#' @param df A data frame, the scale table.
#' @param verbose A logical scalar. If TRUE, it prints progress & errors.
#' @param edgecodes A character/integer vector of codes used to denote different
#' edge properties in an adjacency matrix. Defaluts to c(1,2,3), corresponding
#' to completely separated, partially fused, and completely fused vertices
#' (see Krone pub)
#' @return A data frame, either the original (df), the original data frame modified
#'    to work with other pholidosis functions, or a dummy data frame
#'    including a "bad.data.matrix" column and information about the error.
#' @examples
#' library(readxl)
#' filepath <- system.file("extdata", "DibamidaeDemo.xlsx", package = "pholidosis")
#' anelytropsis.adj <- read_excel(filepath, sheet = 1)
#'
#' # verify that all column and row names have matches
#' anelytropsis.adj <- verifyMatrix(anelytropsis.adj) #they do!
#'
#' # What about bad files?
#' badmatrix <- system.file("extdata", "bad_matrix.xlsx", package = "pholidosis")
#' badmat.adj <- read_excel(badmatrix, sheet = 1)
#'
#' # verify that all column and row names have matches
#' badmat <- verifyMatrix(badmat.adj) #looks like some problems
#' # column 2 is called "frontonasag" but it seems like it should be "frontonasal"
#' # to match row 2
#'
#' # that's ok, I fixed it in the second sheet of this excel workbook...
#' # let's load up sheet 2
#' badmat2 <- verifyMatrix(read_excel(badmatrix, sheet = 2)) #whoops
#' # in row 7, column 20, there's a non-numeric entry.
#' # once that's cleaned up, you should be able to import this file.
#'
#' # there's a cleared-up version in the third sheet of this excel workbook
#'
#' badmat3 <- verifyMatrix(read_excel(badmatrix, sheet = 3)) #seems good
#'
#' badmat3 #looks like it worked!
#' @export

verifyMatrix <- function(df, verbose = FALSE, edgecodes = 1:3){
  df <- as.data.frame(df) #make sure it's not a tibble
  if(!has_rownames(df)){
    col1 <- colnames(df)[1]
    df <- df %>% column_to_rownames(col1)
  }

  #is the matrix square?
  if(dim(df)[1] != dim(df)[2]){
    which()
    warning("not a square matrix: ",dim(df)[1]," × ",dim(df)[1],"\n")
  }

  #are all the column and row names matched?
  if(all(colnames(df) == row.names(df))){
    if(verbose){cat("\n > column & row names match\n")}
  }
  else{
    mismatched <- which(colnames(df) != row.names(df))
    check_names <- data.frame(
      bad.input.matrix = 1,
      columnName = colnames(df)[mismatched],
      rowName = row.names(df)[mismatched])
    warning("mismatched column & row names \n",
            paste(check_names[,2:3]," "),"\n")
    return(check_names)
  }

  #are there non-numeric data in the matrix?

  if("character" %in%
     sapply(colnames(df), function(x){typeof(df[[x]])}) %>%
     unique){

    #typeof(df)

    #which columns contain character data rather than numeric data?
    badCol<- which(sapply(colnames(df), function(x){typeof(df[[x]])}) == "character")

    # which entries are not numeric or NA? (what row & column?)
    badEntries <-which(df == (df[,badCol] %>% unique())[
      which(!(df[,badCol] %>% unique()  %in% c(c(0:3),NA)))], #
                       arr.ind = TRUE)
    df[badEntries]

    warning("Uninterpretable entry or entries ", df[badEntries],
    " in martix @ ", paste(badEntries," "), "\n")

    return(data.frame("bad.input.matrix" = 1,badEntries))
  }

  #is the graph fully triangulated?
  m <- as.matrix(df)
  if(length(which(m[lower.tri(m)]>=1)) != 3*dim(df)[1] - 6){
    warning("graph is not fully triangulated: E != 3V-6 \n > loading graph for visualization only - NOT suitable for comparisons\n")
  }

  return(df)

}
