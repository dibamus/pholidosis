#' Import a scale network or many scale networks from an excel file
#' @import igraph tidyverse readxl stats
#' @param filename A character vector specifying the file path of the excel file.
#' @param verbose A logical vector. If TRUE, funciton prints progress and errors.
#' @return A list of igraph objects, one for each sheet in the file.

#IMPORT FROM EXCEL
excel_to_network <- function(filename, verbose = FALSE) {
  sheets <- excel_sheets(filename)

  ls <- lapply(sheets, function(s){
    if(verbose){cat(s)}
    read_excel(filename, sheet = s, trim_ws = TRUE, .name_repair = "minimal") %>%
                 verifyMatrix(verbose) %>%
                as.data.frame()})

  names(ls) <- sheets #name the dfs

  #did verifyMatrix flag any matrices with "bad.input.matrix"
  badInputs <- sapply(names(ls), function(x){colnames(ls[[x]])[1] == "bad.input.matrix"}) %>%
    which()

  if(any(badInputs)){
    cat("\n WARNING: \n
        The following input matrices contain bad data and will not be converted:\n")
    cat("\t- ",sheets[badInputs],"\n")
    cat("Run excel_to_network with verbose = TRUE to see matrix verifications.\n
        Proceeding to convert all verified matrices. \n")
    ls <- ls[-badInputs]
    sheets <- sheets[-badInputs]
  }

  ls <- lapply(ls, FUN = function(x){x[1:dim(x)[2],1:dim(x)[2]]}) #trim out any excess rows

  #This is kind of hacky but it works to tell the user where problems are
  lapply(names(ls), FUN = function(x){if(verbose){cat("\n",x," - ")};
    scaleNetwork(ls[[x]], verbose = verbose)}) %>% setNames(sheets)
}
