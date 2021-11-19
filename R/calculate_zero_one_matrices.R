## Changelog:
# CG 0.0.1 2021-11-18: initial programming

## Documentation
#' @title Function to calculate zero-one matrices (see for example Definition 1 in Gische and Voelkle, 2021)
#'  for the computation of the interventional distribution
#' interventional level and a specific value in the range of the outcome variable
#' @description Internal function that calculates zero-one matrices needed for the computation
#' of the interventional distribution
#' @param model internal_list or object of class  causalSEM
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable 
#' @param verbose verbosity of console outputs
#' @return \code{calculate_zero_one_matrices} returns a list with the following elements:
#'    ..$select_intervention 
#'    ..$select_non_intervention
#'    ..$eliminate_intervention
#'    ..$duplication_matrix
#'    ..$elimination_matrix
#'    ..$commutation_matrix
#' @seealso \code{\link{calculate_interventional_means, calculate_interventional_variances}}
#' @references
#' Gische, C. & Voelkle, M. C. (2021). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. Psychometrika.
#' @keywords internal

# TODO if at one point we get runtime problems we might consider using the function
# Matrix which uses sparsity information 
## Function definition
calculate_zero_one_matrices <- function( model, intervention_names, outcome_names, verbose ){
  
  # function name
  fun.name <- "calculate_zero_one_matrices"
  
  # function version
  fun.version <- "0.0.1 2021-11-18"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # TODO check if user argument model is the internal_list or
  # an object of class causalSEM
  # CURRENTLY, the function assumes that the input model is
  # of type internal_list. After allowing for objects of class causalSEM
  # the pathes starting with internal_list$ might need adjustment
  
  # get variable names of interventional variables
  if( is.character( intervention_names ) && all( intervention_names %in% internal_list$info_model$var_names ) ){
    intervention_labels <- intervention_names
  } else {
    intervention_labels <- internal_list$info_interventions$intervention_name
  }
  
  # get variable names of outcome variables
  if( is.character( outcome_names ) && outcome_names %in% setdiff(internal_list$info_model$var_names, intervention_labels) ){
    outcome_labels <- outcome_names
  } else {
    stop( paste0( fun.name.version, ": Argument outcome_name needs to be the a character string with the name of a non-interventional variable."  ) )
  }
  
  # number of interventions
  n_int <- length(intervention_labels)
  
  # number of outcome variables
  n_out <- length(outcome_labels)
  
  # set of interventional indeces
  intervention_index <- numeric(n_int)
  for (i in 1:n_int){
    intervention_index[i] <- which(intervention_labels[i] == internal_list$info_model$var_names)
  }
  
  # set of non-interventional indeces
  non_intervention_index <- setdiff(1:internal_list$info_model$n_ov, intervention_index)
  
  # selection matrix for interventional variables
  select_intervention <- matrix(0, nrow = internal_list$info_model$n_ov, ncol = n_int)
  
  for (i in 1:n_int){
    select_intervention[intervention_index[i],i] <- 1
  }
  
  # selection matrix for non-interventional variables
  select_non_intervention <- matrix(0, nrow = internal_list$info_model$n_ov, ncol = (internal_list$info_model$n_ov - n_int))
  
  for (i in 1:(internal_list$info_model$n_ov - n_int)){
    select_non_intervention[non_intervention_index[i],i] <- 1
  }
  
  # elimination matrix for interventional variables
  eliminate_intervention <- diag(internal_list$info_model$n_ov)
  eliminate_intervention[intervention_index,intervention_index] <- 0
  
  
  # set of outcome indeces
  outcome_index <- numeric(n_out)
  for (i in 1:n_out){
    outcome_index[i] <- which(outcome_labels[i] == internal_list$info_model$var_names)
  }
  
  # selection matrix for outcome variables
  select_outcome <- matrix(0, nrow = internal_list$info_model$n_ov, ncol = n_out)
  
  for (i in 1:n_out){
    select_outcome[outcome_index[i],i] <- 1
  }
  
  
  # Elimination, duplication, and commutation matrices
  # CG 0.0.2 2021-11-09: added elimination, duplication, and commutation matrix
  # TODO compute the following three matrices by a different function?
  elimination_matrix <- matrixcalc::elimination.matrix(n = internal_list$info_model$n_ov)
  duplication_matrix <- lavaan::lav_matrix_duplication(n = internal_list$info_model$n_ov)
  commutation_matrix <- lavaan::lav_matrix_commutation(m = internal_list$info_model$n_ov, 
                                                       n = internal_list$info_model$n_ov)
  
  
  # prepare list for output
  zero_one_matrices <- list( select_intervention = select_intervention,
                     select_non_intervention = select_non_intervention,
                     eliminate_intervention = eliminate_intervention,
                     select_outcome = select_outcome,
                     duplication_matrix = duplication_matrix,
                     elimination_matrix = elimination_matrix,
                     commutation_matrix = commutation_matrix )
  
  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # return internal list
  return( zero_one_matrices )
}
  
 
  
  