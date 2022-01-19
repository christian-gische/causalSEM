## Changelog:
# CG 0.0.3 2022-01-13:  changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.2 2021-11-19: function makes use of calculate_constant_matrices and
#                      other small changes. Should accept scalar and vector x 
#                      now.
# CG 0.0.1 2021-11-10: initial programming

## Documentation
#' @title Calculate Jacobian of the Interventional Mean Vector
#' @description Calculate Jacobian of the interventional mean Vector for a 
#' specific interventional level.
#' @param model internal_list or object of class causalSEM
#' @param x interventional level
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable
#' @param verbose verbosity of console outputs
#' @return The Jacobian of interventional mean vector (numeric matrices)
#'    as defined in Eq. 18a (p. 17).
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal

## Function definition
calculate_jacobian_interventional_means <- 
  function(model, x, intervention_names, outcome_names, verbose){

  # function name
  fun_name <- "calculate_jacobian_interventional_means"

  # function version
  fun_version <- "0.0.3 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ",
                               Sys.time(), "\n" ))


  # Prepare elements ----

  # Calculate zero one matrices
  constant_matrices <- calculate_constant_matrices(
    model = model,
    intervention_names = intervention_names,
    outcome_names = outcome_names,
    verbose = verbose
    )

  # Create identity matrix
  I_n <- diag(model$info_model$n_ov)

  # Selection
  ONE_I <- constant_matrices$select_intervention
  I_N <- constant_matrices$eliminate_intervention

  # C and Psi matrices
  C <- model$info_model$C$values

  # Jacobian matrices
  jac_C <- model$info_model$C$derivative

  # Asymptotic covariance matrix of the model parameters
  acov <- model$info_model$param$varcov_par_unique

  # Compute transformation matrix

  C_trans <- solve(I_n - I_N %*% C)


  # Calculate Jacobian g1 ----
  # TODO: allow x to be a vector

  jac_g1 <- kronecker(X = t(x) %*% t(ONE_I) %*% t(C_trans), 
                      Y = C_trans %*% I_N) %*% jac_C

  # return jacobian matrix
  jac_g1

}



