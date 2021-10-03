## Changelog:
# CG 0.0.1 2021-10-03: initial programming

## Documentation
#' @title Builds zero-one matrices (see Definition 1 in Gische and Voelkle, 2021) for the computation of the interventional distribution
#' @description Internal function that build zero-one matrices (e.g., selection matrices) for the computation of the interventional distribution
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{build_zero_one_matrix} returns the inputted internal_list with several slots
#'    in ..$zero_one_matrices filled. 
#'    ..$select_intervention: 
#'    ..$select_non_intervention:
#'    ..$eliminate_intervention: 
#' @seealso \code{\link{intervention_moments}}
#' @references
#' Gische, C. & Voelkle, M. C. (2021). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. Psychometrika.
#' @keywords internal

## Function definition
build_zero_one_matrix <- function( internal_list ){
  
  # function name
  fun.name <- "build_zero_one_matrix"
  
  # function version
  fun.version <- "0.0.1 2021-10-03"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # set of interventional indeces
  intervention_index <- numeric(internal_list$info_interventions$n_intervention)
  for (i in 1:internal_list$info_interventions$n_intervention){
    intervention_index[i] <- which(internal_list$info_interventions$intervention_name[i] == internal_list$info_model$var_names)
  }
  
  # set of non-interventional indeces
  non_intervention_index <- setdiff(1:internal_list$info_model$n_ov, intervention_index)
  
  # selection matrix for interventional variables
  select_intervention <- matrix(0, nrow = internal_list$info_model$n_ov, ncol = internal_list$info_interventions$n_intervention)
  
  for (i in 1:internal_list$info_interventions$n_intervention){
    select_intervention[intervention_index[i],i] <- 1
  }
  
  # selection matrix for non-interventional variables
  select_non_intervention <- matrix(0, nrow = internal_list$info_model$n_ov, ncol = (internal_list$info_model$n_ov - internal_list$info_interventions$n_intervention))
  
  for (i in 1:(internal_list$info_model$n_ov - internal_list$info_interventions$n_intervention)){
    select_non_intervention[non_intervention_index[i],i] <- 1
  }
  
  # elimination matrix for interventional variables
  eliminate_intervention <- diag(internal_list$info_model$n_ov)
  eliminate_intervention[intervention_index,intervention_index] <- 0
  
  
  # populate slots of ..$zero_one_matrices
  internal_list$interventional_distribution$zero_one_matrices$select_intervention <- select_intervention
  internal_list$interventional_distribution$zero_one_matrices$select_non_intervention <- select_non_intervention
  internal_list$interventional_distribution$zero_one_matrices$eliminate_intervention <- eliminate_intervention
  
  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # return internal list
  return( internal_list )
}
