## Changelog:
# CG 0.0.3 2022-01-13:  changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.1 2021-11-19: Initial programming

## Documentation
#' @title Calculate Jacobian of the Covariance Matrix of the Interventional
#' Distribution
#' @description Calculate Jacobian of the covariance matrix of the
#' interventional distribution for a specific interventional level.
#' @param model internal_list or object of class causalSEM
#' @param intervention_names names of interventional variables
#' @param outcome_names names of outcome variable
#' @param verbose verbosity of console outputs
#' @return The Jacobian of interventional covariance matrix (numeric matrices)
#'    as defined in Eq. 18b (p. 17)
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
calculate_jacobian_interventional_variances <- function(
    model = NULL, 
    intervention_names = NULL, 
    outcome_names = NULL, 
    use_model_values = FALSE,
    verbose = NULL){

  ######## TODO: REMOVE DEBUG
  # START DEBUG
  
  #model <- internal_list_no_outcome
  #use_model_values <- FALSE
  #intervention_names <- "x2" 
  #outcome_names <- "y3"
  
  # END DEBUG
  
  # function name
  fun_name <- "calculate_jacobian_interventional_variances"

  # function version
  fun_version <- "0.0.3 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")
  
  # CG 0.0.3 2023-02-23: include check of user-specified arguments model
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
  
  if(use_model_values == TRUE) {
    
    verbose <- model$control$verbose
    intervention_names <- model$info_interventions$intervention_names
    outcome_names <- model$info_interventions$outcome_names
    constant_matrices <- model$constant_matrices
    
  } else {
    
    #TODO: include argument check
    
    verbose <- handle_verbose_argument(verbose)
    intervention_names <- intervention_names
    outcome_names <- outcome_names
    
    # # Calculate zero one matrices
    constant_matrices <- calculate_constant_matrices(
      model = model,
      intervention_names = intervention_names,
      outcome_names = outcome_names)
    
  }

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ",
                               Sys.time(), "\n" ))


  # Prepare elements ----

  # Number of observed variables
  n <- model$info_model$n_ov

  # Identity matrices
  I_n <- diag(n)
  I_n2 <- diag(n^2)

  # Selection
  ONE_I <- constant_matrices$select_intervention
  I_N <- constant_matrices$eliminate_intervention

  # Elimination, duplication, and commutation matrices
  L_n <- constant_matrices$elimination_matrix
  D_n <- constant_matrices$duplication_matrix
  K_n <- constant_matrices$commutation_matrix

  # C and Psi matrices
  C <- model$info_model$C$values
  Psi <- model$info_model$Psi$values

  # Jacobian matrices
  jac_C <- model$info_model$C$derivative
  jac_Psi <- model$info_model$Psi$derivative

  # Compute transformation matrix
  C_trans <- solve(I_n - I_N %*% C)


  # Calculate Jacobian of the interventional variances ----

  G_2C <- (I_n2 + K_n) %*%
    kronecker(X = C_trans %*% I_N %*% Psi %*% I_N, Y = I_n) %*%
    kronecker(X = t(C_trans), Y = C_trans %*% I_N)

  G_2Psi <- kronecker(X = C_trans, Y = C_trans) %*% kronecker(X = I_N, Y = I_N)

  jac_g2 <- L_n %*% (G_2C %*% jac_C + G_2Psi %*% jac_Psi)

  jac_g2


}



