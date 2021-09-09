## Changelog:
# CG 0.0.2 2021-09-09: update verbose handling
# CG 0.0.1 2021-09-04: initial programing

## Documentation
#' @title Fill in labels in lavaan parTable object
#' @description Internal function that fills in default labels from the lavaan model syntax into empty slots (i.e., parameters that do not have a user specified label) of the lavaan parTable object.
#' @param internal_list a list with various information extracted from the
#' model.
#' @return \code{lav_parTable_fill_labels} returns a lavaan parTable object where empty slots (i.e., parameters that do not have a user specified label) in the original parTable object have been filled in with default labels from the lavaan model syntax.
#' @references
#' Gische, C. & Voelkle, M. C. (under review???). Beyond the mean: A flexible framework for studying causal effects using linear models
#' \href{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}

lav_parTable_fill_labels <- function(internal_list) {
  
  # function name
  fun.name <- "lav_parTable_fill_labels"
  
  # function version
  fun.version <- "0.0.2 2021-09-09"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

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
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # return parameter table with complete label column
  return(lav_ParTable)
  
}