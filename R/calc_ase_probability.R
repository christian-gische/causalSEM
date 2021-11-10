## Changelog:
# CG 0.0.3 2021-11-09: get elimination, duplication and commutation
# matrix from internal list, respectively.
# MA 0.0.2 2021-10-31: initial programming
# MA 0.0.1 2021-10-28: initial programming


## Documentation
#' @title Calculate asymptotic standard errors of the interventional
#' distribution
#' @description The function ase_probability() calculates the asysmptotic variance,
#' standard error and z-value of the interventional probability. Currently, the
#' function only accepts a single outcome variable.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{calc_ase_probability} returns the inputted internal.list and adds
#' three slots to the interventional_distribution$probability sublist.
#'    "acov" is a numeric with the asymptotic variance.
#'    "ase" is a numeric with the asymptotic standard error.
#'    "zvalue" is a numeric with the asymptotic z-value.
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}

calc_ase_probability <- function(internal_list) {

  # Verbosity and bug tracking ----

  # function name
  fun_name <- "ase_probability"

  # function version
  fun_version <- "0.0.3 2021-11-09"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }


  # Sort this later ----

  # Prepare elements ----

  # Intervention info
  outcome <- internal_list$info_interventions$outcome_name
  intervention <- internal_list$info_interventions$intervention_name
  intervention_level <- internal_list$info_interventions$intervention_level
  upper_bound <- internal_list$info_interventions$upper_bound
  lower_bound <- internal_list$info_interventions$lower_bound

  # Number of observed variables
  n <- internal_list$info_model$n_ov

  # Convert interventional level to column vector
  x <- as.matrix(internal_list$info_interventions$intervention_level)

  # Identity matrices
  I_n <- diag(n)
  I_n2 <- diag(n^2)

  # Selection matrices
  ONE_I <- internal_list$interventional_distribution$zero_one_matrices$select_intervention
  I_N <- internal_list$interventional_distribution$zero_one_matrices$eliminate_intervention
  ## Find the position of the outcome (only a single outcome is supported yet)
  j <- which(internal_list$info_model$var_names == internal_list$info_interventions$outcome_name)
  ## Select outcome
  i_j <- matrix(0, nrow = n, ncol = 1)
  i_j[j, 1] <- 1

  # Elimination, duplication, and commutation matrices
  # CG 0.0.3 2021-11-09: get elimination, duplication and commutation
  # matrix from internal list, respectively.
  L_n <- internal_list$interventional_distribution$zero_one_matrices$elimination_matrix
  D_n <- internal_list$interventional_distribution$zero_one_matrices$duplication_matrix
  K_n <- internal_list$interventional_distribution$zero_one_matrices$commutation_matrix
  
  ## Select outcome
  i_jDn <- matrix(0, nrow = n^2, ncol = 1)
  i_jDn[(j - 1)*n +j] <- 1

  # C and Psi matrices
  C <- internal_list$info_model$C$values
  Psi <- internal_list$info_model$Psi$values

  # Jacobian matrices
  jac_C <- internal_list$info_model$C$derivative
  jac_Psi <- internal_list$info_model$Psi$derivative

  # Asymptotic covariance matrix of the model parameters
  acov <- internal_list$info_model$param$varcov_par_unique

  # Interventional means
  gamma_1 <- internal_list$interventional_distribution$moments$mean_vector

  # Interventional variance-covariance matrix
  gamma_2 <- internal_list$interventional_distribution$moments$variance_matrix

  #
  gamma_4 <- internal_list$interventional_distribution$probability$p

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


  # Calculate Jacobian g4 ----

  # Select outcome mean and standard deviation
  outcome_mean <- gamma_1[outcome, 1]
  outcome_std <- sqrt(gamma_2[outcome, outcome])

  # Densities
  z_low <- (lower_bound - outcome_mean) / outcome_std
  z_up <- (upper_bound - outcome_mean) / outcome_std
  density_low <- stats::dnorm(lower_bound, mean = outcome_mean, sd = outcome_std)
  density_up <- stats::dnorm(upper_bound, mean = outcome_mean, sd = outcome_std)

  # G_matrices
  G_4mu <- 1 / outcome_std * (density_up - density_low)
  G_4sigma2 <- 1 / (2*outcome_std^2) * (density_up * z_up - density_low * z_low)

  # Jacobian
  jac_g4 <- cbind(G_4mu, G_4sigma2) %*% rbind(t(i_j) %*% jac_g1, t(i_jDn) %*% D_n %*% jac_g2)


  # Calculate asymptotic covariances ----

  AV_gamma_4 <- jac_g4 %*% acov %*% t(jac_g4)



  # Calculate asymptotic standard errors ----

  ase_gamma_4 <- sqrt(AV_gamma_4)


  # Calculate z-values

  zvalue_gamma_4 <- gamma_4 / ase_gamma_4


  # Prepare output ----

  # Console output
  if(verbose >= 2) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # Store results
  internal_list$interventional_distribution$probability$acov <- AV_gamma_4
  internal_list$interventional_distribution$probability$ase <- ase_gamma_4
  internal_list$interventional_distribution$probability$zvalue <- zvalue_gamma_4

  internal_list

}
