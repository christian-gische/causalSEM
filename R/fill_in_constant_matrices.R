## Changelog:
# CG 0.0.2 2022-01-13: changed name from fill_in_constant_matrices
#                       to fill_in_constant matrices
#                       changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# CG 0.0.1 2021-11-22: initial programming

## Documentation
#' @title Fill in Constant Matrices Used for Compuations to the Internal List
#' @description Fill in constant matrices used for the computation of the 
#' interventional distribution into the internal list.
#' (see for example Definition 1 in Gische and Voelkle, 2021)
#' @param internal_list A list with information extracted from the model.
#' @return The inputted internal_list 
#' with several slots in ..$constant_matrices filled. 
#'    ..$select_intervention 
#'    ..$select_non_intervention
#'    ..$eliminate_intervention
#'    ..$duplication_matrix
#'    ..$elimination_matrix
#'    ..$commutation_matrix
#' @seealso \code{\link{}}
#' @references
#' Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible framework for 
#' studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal

## Function definition
fill_in_constant_matrices <- function( internal_list ){ 
  
  # function name
  fun.name <- "fill_in_constant_matrices"
  
  # function version
  fun.version <- "0.0.2 2022-01-13"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  
  # TODO: exctract the required arguments of the calculate_constant_matrices 
  # function from the internal list and call the calculate_constant_matrices 
  # function with this arguments
  
  constant_matrices_list <- calculate_constant_matrices ( 
    model = internal_list, 
    intervention_names = internal_list$info_interventions$intervention_names, 
    outcome_names = internal_list$info_interventions$outcome_names, 
    verbose = internal_list$control$verbose )
  
  
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
