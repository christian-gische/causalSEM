## Changelog:
# CG 0.0.6 2023-02-28: check if argument is of class causalSEM 
# CG 0.0.5 2023-02-23: set use_model_values argument in 
#                      calculate_constant_matrices function
# CG 0.0.4 2023-02-21: changes in preamble and comments
# MH 0.0.3 2022-03-17: removed "seealso", solves NOTE in package checking
# CG 0.0.2 2022-01-13: changed name from fill_in_constant_matrices
#                       to fill_in_constant matrices
#                       changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# CG 0.0.1 2021-11-22: initial programming

## Documentation
#' @title Fill in Zero-One Matrices to List
#' @description Fill in constant zero-one matrices used for the computation of 
#' the interventional distribution into the internal list (see, for example, 
#' Definition 1 in Gische and Voelkle, 2022).
#' @param internal_list A list with information extracted from the model.
#' @return The inputted list with several slots in \code{..$constant_matrices} 
#' filled in.\cr
#'\tabular{ll}{
#' \tab   \code{$select_intervention} \cr
#' \tab   \code{$select_non_intervention} \cr
#' \tab   \code{$select_outcome} \cr
#' \tab   \code{$eliminate_intervention} \cr
#' \tab   \code{$duplication_matrix} \cr
#' \tab   \code{$elimination_matrix} \cr
#' \tab   \code{$commutation_matrix}}
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_constant_matrices <- function( internal_list = NULL ){ 
  
  # function name
  fun.name <- "fill_in_constant_matrices"
  
  # function version
  fun.version <- "0.0.6 2023-02-28"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # CG 0.0.6 2023-02-28: check if argument is of class causalSEM 
  # check function arguments 
  ## get class of model object
  model_class <- class(internal_list)
  
  ## set supported classes of model objects
  supported_model_classes <- c( "causalSEM" )
  
  ## check if argument model is supported
  if(!any(model_class %in% supported_model_classes)) stop(
    paste0(
      fun.name.version, ": model of class ", model_class,
      " not supported. Supported fit objects are: ",
      paste(supported_model_classes, collapse = ", ")
    )
  )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  
  
  constant_matrices_list <- calculate_constant_matrices(model = internal_list,
                                                        use_model_values = TRUE)
    
  
  
  # fill in slots of ..$constant_matrices
  
  internal_list$constant_matrices$select_intervention <- 
    constant_matrices_list$select_intervention
  internal_list$constant_matrices$select_non_intervention <- 
    constant_matrices_list$select_non_intervention
  internal_list$constant_matrices$eliminate_intervention <- 
    constant_matrices_list$eliminate_intervention
  internal_list$constant_matrices$select_outcome <- 
    constant_matrices_list$select_outcome
  internal_list$constant_matrices$duplication_matrix <- 
    constant_matrices_list$duplication_matrix
  internal_list$constant_matrices$elimination_matrix <- 
    constant_matrices_list$elimination_matrix
  internal_list$constant_matrices$commutation_matrix <- 
    constant_matrices_list$commutation_matrix
  
  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  
  # return internal list
  return( internal_list )
}
