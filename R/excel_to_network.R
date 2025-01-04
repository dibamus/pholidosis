#' excel_to_network
#'
#' Import a scale network or many scale networks from an excel file.
#'
#' Calls verifyMatrix to troubleshoot and creates igraph objects using
#' scaleNetwork.
#'
#' @import tidyverse
#' @importFrom readxl excel_sheets read_excel
#' @param filename A character vector specifying the file path of the excel file.
#' @param verbose A logical vector. If TRUE, funciton prints progress and errors.
#' @param ... other parameters, including a setup function for use by scaleNetwork()
#' @return A list of igraph objects, one for each sheet in the file.
#' @examples
#'
#' filepath <- system.file("extdata", "DibamidaeDemo.xlsx", package = "pholidosis")
#' Dib <- excel_to_network(filepath)
#' plot(Dib$Anelytropsis_papillosus) #check it out, it's Anelytropsis papillosus
#' @export

#IMPORT FROM EXCEL
excel_to_network <- function(filename, verbose = FALSE, ...) {
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
