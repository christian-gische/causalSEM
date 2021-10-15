## Changelog:
# MA 0.0.1 2021-10-14: initial programming

## Documentation
#' @title Calculate asymptotic standard errors
#' @description Internal function that calculates the asymptotic
#' variance-covariance matrix, standard errors, and z-values of the
#' interventional means and variance-covariance matrix
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{calculate_ase} returns the inputted internal_list and adds
#' three slots to the interventional_distribution sublist.
#'    "acov" is a list containing the asymptotic variance-covariance matrices of
#'    the interventional means and covariances.
#'    "ase" is a list containing vectors with the asymptotic standard errors of
#'    the interventional means and variances.
#'    "z_values" is a list containing vectors with the asymptotic z-values of
#'    the interventional means and variances.
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords

calculate_ase <- function(internal_list) {

  # Verbosity and bug tracking ----

  # function name
  fun_name <- "calculate_ase"

  # function version
  fun_version <- "0.0.1 2021-10-14"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }


  # Prepare elements ----

  # Number of observed variables
  n <- internal_list$info_model$n_ov

  # Convert interventional level to column vector
  x <- as.matrix(internal_list$info_interventions$intervention_level)

  # Identity matrices
  I_n <- diag(n)
  I_n2 <- diag(n^2)

  # Selection
  ONE_I <- internal_list$interventional_distribution$zero_one_matrices$select_intervention
  I_N <- internal_list$interventional_distribution$zero_one_matrices$eliminate_interventio

  # Elimination, duplication, and commutation matrices
  L_n <- matrixcalc::elimination.matrix(n = n)
  D_n <- lavaan::lav_matrix_duplication(n = n)
  K_n <- lavaan::lav_matrix_commutation(m = n, n = n)

  # C and Psi matrices
  C <- internal_list$info_model$C$values
  Psi <- internal_list$info_model$Psi$values

  # Jacobian matrices
  jac_C <- internal_list$info_model$C$derivative
  jac_Psi <- internal_list$info_model$Psi$derivative

  # Asymptotic covariance matrix of the model parameters
  acov <- internal_list$info_model$param$varcov_par_unique

  # Interventional means
  ## This is not properly linked up
  # gamma_1 <- internal_list$info_interventions$means

  # Compute transformation matrix
  ## I cannot think of a better name. Feel free to change
  C_trans <- solve(I_n - I_N %*% C)


  # Calculate Jacobian g1 ----

  jac_g1 <- kronecker(X = t(x) %*% t(ONE_I) %*% t(C_trans), Y = C_trans %*% I_N) %*% jac_C


  # Calculate Jacobian g2 ----

  G_2C <- (I_n2 + K_n) %*%
    kronecker(X = C_trans %*% I_N %*% Psi %*% I_N, Y = I_n) %*%
    kronecker(X = t(C_trans), Y = C_trans %*% I_N)

  G_2Psi <- kronecker(X = C_trans, Y = C_trans) %*% kronecker(X = I_N, Y = I_N)

  jac_g2 <- L_n %*% (G_2C %*% jac_C + G_2Psi %*% jac_Psi)


  # Calculate asymptotic covariances ----

  AV_gamma_1 <- jac_g1 %*% acov %*% t(jac_g1)

  AV_gamma_2 <- jac_g2 %*% acov %*% t(jac_g2)


  # Calculate asymptotic standard errors ----

  ase_gamma_1 <- sqrt(diag(AV_gamma_1))

  ## Diagnonal values only?
  ase_gamma_2 <- sqrt(diag(AV_gamma_2)[1:n * n])


  # Calculate z-values

  z_gamma_1 <- gamma_1 / ase_gamma_1

  ## Are these the correct z-values
  z_gamma_2 <- diag(gamma_2) / ase_gamma_2


  # Prepare output ----

  # Console output
  if(verbose >= 2) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # Store results
  internal_list$interventional_distribution$acov <- list(
    acov_means = AV_gamma_1,
    ase_se = AV_gamma_2
  )

  internal_list$interventional_distribution$ase <- list(
    ase_means = ase_gamma_1,
    ase_covariance = ase_gamma_2
  )

  internal_list$interventional_distribution$z_values <- list(
    z_means = z_gamma_1,
    z_variance = z_gamma_2
  )

  internal_list

}
