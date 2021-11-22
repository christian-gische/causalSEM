## Changelog:
# CG 0.0.1 2021-11-22: initial programming

## Documentation
#' @title Function to fill in the zero-one matrices (see for example Definition 1 in Gische and Voelkle, 2021)
#'  for the computation of the interventional distribution into the internal list
#' @description Internal function that fills in the zero-one matrices needed for the computation
#' of the interventional distribution into the internal list
#' @param internal_list A list with various information extracted from the model.
#' @return \code{fill_in_zero_one_matrices} returns the inputted internal_list with several slots
#'    in ..$zero_one_matrices filled. 
#'    ..$select_intervention 
#'    ..$select_non_intervention
#'    ..$eliminate_intervention
#'    ..$duplication_matrix
#'    ..$elimination_matrix
#'    ..$commutation_matrix
#' @seealso \code{\link{}}
#' @references
#' Gische, C. & Voelkle, M. C. (2021). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. Psychometrika.
#' @keywords internal

## Function definition
fill_in_zero_one_matrices <- function( internal_list ){ 
  
  # function name
  fun.name <- "fill_in_zero_one_matrices"
  
  # function version
  fun.version <- "0.0.1 2021-11-22"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # TODO: exctract the required arguments of the calculate_zero_one_matrices function from the 
  # internal list and call the calculate_zero_one_matrices function with this arguments
  
  zero_one_matrices_list <- calculate_zero_one_matrices ( model = internal_list, 
                                                          intervention_names = internal_list$info_interventions$intervention_name, 
                                                          outcome_names = internal_list$info_interventions$outcome_name, 
                                                          verbose = internal_list$control$verbose )
  
  
  # fill in slots of ..$zero_one_matrices
  
  internal_list$interventional_distribution$zero_one_matrices$select_intervention <- zero_one_matrices_list$select_intervention
  internal_list$interventional_distribution$zero_one_matrices$select_non_intervention <- zero_one_matrices_list$select_non_intervention
  internal_list$interventional_distribution$zero_one_matrices$eliminate_intervention <- zero_one_matrices_list$eliminate_intervention
  internal_list$interventional_distribution$zero_one_matrices$select_outcome <- zero_one_matrices_list$select_outcome
  internal_list$interventional_distribution$zero_one_matrices$duplication_matrix <- zero_one_matrices_list$duplication_matrix
  internal_list$interventional_distribution$zero_one_matrices$elimination_matrix <- zero_one_matrices_list$elimination_matrix
  internal_list$interventional_distribution$zero_one_matrices$commutation_matrix <- zero_one_matrices_list$commutation_matrix
  
  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # return internal list
  return( internal_list )
}
