## Changelog:
# CG 0.0.3 2022-01-13: changed name from calculate_constant_matrices
#                       to calculate_constant_matrices
#                       changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.2 2021-11-19: updated the object name 'internal_list' in the code to
#                      'model', so that it is in line with the function
#                      arguments
# CG 0.0.1 2021-11-18: initial programming


## Documentation
#' @title Calculate Constant Matrices for the Computation of the 
#' Interventional Distribution
#' @description Calculates constant matrices used in the computation
#' of the interventional distribution 
#' (see for example Definition 1 in Gische and Voelkle, 2021).  
#' @param model internal_list or object of class  causalSEM
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable
#' @param verbose verbosity of console outputs
#' @return \code{calculate_constant_matrices} returns a list with the following
#'  elements:
#'    ..$select_intervention
#'    ..$select_non_intervention
#'    ..$eliminate_intervention
#'    ..$duplication_matrix
#'    ..$elimination_matrix
#'    ..$commutation_matrix
#' @seealso \code{\link{calculate_interventional_means, 
#' calculate_interventional_variances}}
#' @references
#' Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible framework for 
#' studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal

# TODO if at one point we get runtime problems we might consider using
# the function Matrix which uses sparsity information

# Function definition
calculate_constant_matrices <- function( model, intervention_names, 
                                         outcome_names, verbose ){

  # function name
  fun.name <- "calculate_constant_matrices"

  # function version
  fun.version <- "0.0.2 2021-11-19"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # get verbose argument
  verbose <- model$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )

  # TODO check if user argument model is the internal_list or
  # an object of class causalSEM
  # CURRENTLY, the function assumes that the input model is
  # of type internal_list. After allowing for objects of class causalSEM
  # the pathes starting with internal_list$ might need adjustment

  # get variable names of interventional variables
  if( is.character( intervention_names ) && 
      all( intervention_names %in% model$info_model$var_names ) ){
    intervention_labels <- intervention_names
  } else {
    intervention_labels <- model$info_interventions$intervention_names
  }

 
  # get variable name of outcome variable
  # TODO think of allowing calculation only for NON interventional variables 
  # according to the following code
  #  if( is.character( outcome_names ) && 
  #  outcome_names %in% setdiff(model$info_model$var_names, x_labels) ){
  #    outcome_labels <- outcome_names
  #  } else {
  #    stop( paste0( fun.name.version, ": Argument outcome_names needs to be the a 
  #                  character string with the name of a non-interventional 
  #                  variable."  ) )
  #  }
  
  if( is.character( outcome_names ) && 
      all ( outcome_names %in% model$info_model$var_names ) ){
    outcome_labels <- outcome_names
  } else {
    stop( paste0( fun.name.version, ": Argument outcome_names needs to be a 
                  character string with the name of an observed variable."  ) )
  }

  # number of interventions
  n_int <- length(intervention_labels)

  # number of outcome variables
  n_out <- length(outcome_labels)

  # set of interventional indeces
  intervention_index <- numeric(n_int)
  for (i in 1:n_int){
    intervention_index[i] <- 
      which(intervention_labels[i] == model$info_model$var_names)
  }

  # set of non-interventional indeces
  non_intervention_index <- setdiff(1:model$info_model$n_ov, intervention_index)

  # selection matrix for interventional variables
  select_intervention <- matrix(0, nrow = model$info_model$n_ov, ncol = n_int)

  for (i in 1:n_int){
    select_intervention[intervention_index[i],i] <- 1
  }

  # selection matrix for non-interventional variables
  select_non_intervention <- matrix(0, nrow = model$info_model$n_ov, 
                                    ncol = (model$info_model$n_ov - n_int))

  for (i in 1:(model$info_model$n_ov - n_int)){
    select_non_intervention[non_intervention_index[i],i] <- 1
  }

  # elimination matrix for interventional variables
  eliminate_intervention <- diag(model$info_model$n_ov)
  eliminate_intervention[intervention_index,intervention_index] <- 0


  # set of outcome indeces
  outcome_index <- numeric(n_out)
  for (i in 1:n_out){
    outcome_index[i] <- which(outcome_labels[i] == model$info_model$var_names)
  }

  # selection matrix for outcome variables
  select_outcome <- matrix(0, nrow = model$info_model$n_ov, ncol = n_out)

  for (i in 1:n_out){
    select_outcome[outcome_index[i],i] <- 1
  }


  # Elimination, duplication, and commutation matrices
  # CG 0.0.2 2021-11-09: added elimination, duplication, and commutation matrix
  # TODO compute the following three matrices by a different function?
  
  elimination_matrix <- 
    matrixcalc::elimination.matrix(n = model$info_model$n_ov)
  duplication_matrix <- 
    lavaan::lav_matrix_duplication(n = model$info_model$n_ov)
  commutation_matrix <- 
    lavaan::lav_matrix_commutation(m = model$info_model$n_ov,
                                   n = model$info_model$n_ov)


  # prepare list for output
  constant_matrices <- list( select_intervention = select_intervention,
                     select_non_intervention = select_non_intervention,
                     eliminate_intervention = eliminate_intervention,
                     select_outcome = select_outcome,
                     duplication_matrix = duplication_matrix,
                     elimination_matrix = elimination_matrix,
                     commutation_matrix = commutation_matrix )

  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", 
                                  Sys.time(), "\n" ) )

  # return internal list
  return( constant_matrices )
}



