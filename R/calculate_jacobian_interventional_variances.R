## Changelog:
# MA 0.0.1 2021-11-19: Initial programming

## Documentation
#' @title Calculate Jacobian of the interventional variances for a specific
#' interventional level and a specific value in the range of the outcome variable
#' @description Internal function that calculates the Jacobian of the
#' interventional variances for a specific interventional level and a specific value.
#' in the range of the outcome variable
#' @param model internal_list or object of class causalSEM
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable
#' @param verbose verbosity of console outputs
#' @return \code{calculate_jacobian_interventional_variances} returns the
#'    Jacobian of interventional covariance matrix (numeric matrices)
#'    as defined in Eq. 18b (p. 17)
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
calculate_jacobian_interventional_variances <- function(
  model, intervention_names, outcome_names, verbose
  ) {

  # function name
  fun_name <- "calculate_jacobian_interventional_variances"

  # function version
  fun_version <- "0.0.1 2021-11-19"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ", Sys.time(), "\n" ))


  # Prepare elements ----

  # Number of observed variables
  n <- model$info_model$n_ov

  # Identity matrices
  I_n <- diag(n)
  I_n2 <- diag(n^2)

  # # Calculate zero one matrices
  zero_one_matrices <- calculate_zero_one_matrices(
    model = model,
    intervention_names = intervention_names,
    outcome_names = outcome_names,
    verbose = verbose
  )

  # Selection
  ONE_I <- zero_one_matrices$select_intervention
  I_N <- zero_one_matrices$eliminate_intervention

  # Elimination, duplication, and commutation matrices
  L_n <- zero_one_matrices$elimination_matrix
  D_n <- zero_one_matrices$duplication_matrix
  K_n <- zero_one_matrices$commutation_matrix

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

  # Should the output be vectorized or not?

}



