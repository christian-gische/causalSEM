## Changelog:
# CG 0.0.3 2022-01-14: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# CG 0.0.2 2021-11-18: changed name from lav_parTable_fill_labels.R
#                      to add_labels_in_lavaan_parTable.R
# CG 0.0.2 2021-09-09: update verbose handling
# CG 0.0.1 2021-09-04: initial programing

## Documentation
#' @title Fill in Labels in lavaan parTable Object
#' @description Fills in default labels from the lavaan model 
#' syntax into empty slots (i.e., parameters that do not have a user
#' specified label) of the lavaan parTable object.
#' @param internal_list A list with various information extracted 
#' from the model.
#' @return A lavaan parTable object where empty slots (i.e., parameters 
#' that do not have a user specified label) in the original 
#' parTable object have been filled in with default labels from 
#' the lavaan model syntax.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z

add_labels_in_lavaan_parTable <- function(internal_list) {
  
  # function name
  fun.name <- "add_labels_in_lavaan_parTable"
  
  # function version
  fun.version <- "0.0.3 2022-01-14"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )

  # get fit object from internal_list
  fit <- internal_list$fitted_object
  
  # get parameter table
  lav_ParTable <- lavaan::lavMatrixRepresentation(
    partable = fit@ParTable
  )
  
  # label unlabeled parameters
  unlabelled_params <- which(lav_ParTable[, "label"] == "")
  lav_ParTable[unlabelled_params, "label"] <- apply(
    lav_ParTable[unlabelled_params, 2:4], MARGIN = 1, FUN = paste, collapse = ""
  )
  
  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", 
                                  Sys.time(), "\n" ) )
  
  # return parameter table with complete label column
  return(lav_ParTable)
  
}