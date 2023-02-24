## Changelog:
# CG 0.0.5 2023-02-23: include argument use_model_values and change 
#                       checks of user-specified arguments accordingly
# CG 0.0.4 2023-02-20: changes to preamble to print documentation
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
#' @title Calculate Zero-One Matrices
#' @description Calculates zero-one matrices used in the computation
#' of the interventional distribution 
#' (see for example Definition 1 in Gische and Voelkle, 2022).  
#' @param model Object of class causalSEM
#' @param intervention_names Names of interventional variables
#' @param outcome_names Names of outcome variables
#' @param verbose Verbosity of console outputs
#' @return List with several zero-one matrices:\cr
#' \tabular{ll}{
#' List of 7 \tab \cr
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


# TODO if at one point we get runtime problems we might consider using
# the function Matrix which uses sparsity information

# CG 0.0.5 2023-02-23: include argument use_model_values and change 
#                      checks of user-specified arguments accordingly
# Function definition
calculate_constant_matrices <- function(model = NULL,
                                        intervention_names = NULL,
                                        outcome_names = NULL,
                                        verbose = NULL,
                                        use_model_values = FALSE){

  # function name
  fun.name <- "calculate_constant_matrices"

  # function version
  fun.version <- "0.0.5 2023-02-23"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # get verbose argument
  verbose <- handle_verbose_argument(verbose)

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  
  # CG 0.0.5 2023-02-23: include argument use_model_values and change 
  #                      checks of user-specified arguments accordingly
  
  # get class of model object
  model_class <- class(model)
  
  # set supported classes of model objects
  supported_model_classes <- c( "causalSEM" )
  
  # check if argument model is supported
  if(!any(model_class %in% supported_model_classes)) stop(
    paste0(
      fun.name.version, ": model of class ", model_class,
      " not supported. Supported fit objects are: ",
      paste(supported_model_classes, collapse = ", ")
    )
  )
  
  # CG 0.0.5 2023-02-23: include argument use_model_values and change 
  #                      checks of user-specified arguments accordingly
  
  # check if model values of should be used; if not, use user specified 
  # arguments (after checking if they are admissible)
  if(use_model_values == TRUE) {
    intervention_labels <- model$info_interventions$intervention_names
    outcome_labels <- model$info_interventions$outcome_names
    verbose <- model$control$verbose
  } else {
  
  # CG 0.0.5 2023-02-23: include argument use_model_values and change 
  #                      checks of user-specified arguments accordingly  
    
  # get intervention_names and check if admissible
  if( is.character( intervention_names ) && 
      all( intervention_names %in% model$info_model$var_names )){
    # set intervention_names
    intervention_labels <- intervention_names
  } else if ( is.null( intervention_names ) ){
    intervention_labels <- model$info_interventions$intervention_names
  } else {
    stop( paste0( fun.name.version, ": Argument intervention_names needs to be 
    a character vector of variable names." ))
  }
    
  #TODO: 
  # 1) set use_model_values = TRUE/FALSE in all calls of the
  # compute_constant_matrices function
  # 2) remove the else if parts in the statements
  # 3) allow outcome names to take NON interventional values only
  
  # CG 0.0.5 2023-02-23: include argument use_model_values and change 
  #                      checks of user-specified arguments accordingly
    
  # get outcome_names and check if admissible
  if( is.character( outcome_names ) && 
      all( outcome_names %in% model$info_model$var_names))
    {
    # set in internal_list
    outcome_labels <- outcome_names
  } else if ( is.null( outcome_names ) ){
    outcome_labels <- model$info_interventions$outcome_names
  } else {
    stop( paste0( fun.name.version, ": Argument outcome needs to be a character 
    vector of variable names of variables that are not subject to intervention."
                  ))
  }
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



