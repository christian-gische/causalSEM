## Changelog:
# MA 0.0.1 2021-10-28: initial programming

## Documentation
#' @title Calculate asymptotic standard errors of the interventional
#' distribution
#' @description The function ase_probability() calculates the asysmptotic variance,
#' standard error and z-value of the interventional probability. Currently, the
#' function only accepts a single outcome variable.
#' @param internal.list A list with various information extracted from the
#'    model.
#' @return \code{calc_ase_probability} returns the inputted internal.list and adds
#' three slots to the interventional_distribution$probability sublist.
#'    "acov" is a numeric with the asymptotic variance.
#'    "ase" is a numeric with the asymptotic standard error.
#'    "zvalue" is a numeric with the asymptotic z-value.
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords

calc_ase_probability <- function(internal.list) {

  # Verbosity and bug tracking ----

  # function name
  fun.name <- "ase_probability"

  # function version
  fun.version <- "0.0.1 2021-10-29"

  # function name+version
  fun.name.version <- paste0(fun.name, " (", fun.version, ")")

  # get verbose argument
  verbose <- internal.list$control$verbose

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun.name.version, " ", Sys.time(), "\n"))
  }


  # Sort this later ----

  # Prepare elements ----

  # Intervention info
  outcome <- internal.list$info_interventions$outcome_name
  intervention <- internal.list$info_interventions$intervention_name
  intervention.level <- internal.list$info_interventions$intervention_level
  upper.bound <- internal.list$info_interventions$upper_bound
  lower.bound <- internal.list$info_interventions$lower_bound

  # Number of observed variables
  n <- internal.list$info_model$n_ov

  # Convert interventional level to column vector
  x <- as.matrix(internal.list$info_interventions$intervention_level)

  # Identity matrices
  I_n <- diag(n)
  I_n2 <- diag(n^2)

  # Selection matrices
  ONE_I <- internal.list$interventional_distribution$zero_one_matrices$select_intervention
  I_N <- internal.list$interventional_distribution$zero_one_matrices$eliminate_intervention
  ## Find the position of the outcome (only a single outcome is supported yet)
  j <- which(internal.list$info_model$var_names == internal.list$info_interventions$outcome_name)
  ## Select outcome
  i_j <- matrix(0, nrow = n, ncol = 1)
  i_j[j, 1] <- 1

  # Elimination, duplication, and commutation matrices
  L_n <- matrixcalc::elimination.matrix(n = n)
  D_n <- lavaan::lav_matrix_duplication(n = n)
  K_n <- lavaan::lav_matrix_commutation(m = n, n = n)
  ## Select outcome
  i_jDn <- matrix(0, nrow = n^2, ncol = 1)
  i_jDn[(j - 1)*n +j] <- 1

  # C and Psi matrices
  C <- internal.list$info_model$C$values
  Psi <- internal.list$info_model$Psi$values

  # Jacobian matrices
  jac_C <- internal.list$info_model$C$derivative
  jac_Psi <- internal.list$info_model$Psi$derivative

  # Asymptotic covariance matrix of the model parameters
  acov <- internal.list$info_model$param$varcov_par_unique

  # Interventional means
  gamma_1 <- internal.list$interventional_distribution$moments$mean_vector

  # Interventional variance-covariance matrix
  gamma_2 <- internal.list$interventional_distribution$moments$variance_matrix

  #
  gamma_4 <- internal.list$interventional_distribution$probability$p

  # Compute transformation matrix
  ## I cannot think of a better name. Feel free to change
  C.trans <- solve(I_n - I_N %*% C)


  # Calculate Jacobian g1 ----

  jac_g1 <- kronecker(X = t(x) %*% t(ONE_I) %*% t(C.trans), Y = C.trans %*% I_N) %*% jac_C


  # Calculate Jacobian g2 ----

  G_2C <- (I_n2 + K_n) %*%
    kronecker(X = C.trans %*% I_N %*% Psi %*% I_N, Y = I_n) %*%
    kronecker(X = t(C.trans), Y = C.trans %*% I_N)

  G_2Psi <- kronecker(X = C.trans, Y = C.trans) %*% kronecker(X = I_N, Y = I_N)

  jac_g2 <- L_n %*% (G_2C %*% jac_C + G_2Psi %*% jac_Psi)


  # Calculate Jacobian g4 ----

  # Select outcome mean and standard deviation
  outcome.mean <- gamma_1[outcome, 1]
  outcome.std <- sqrt(gamma_2[outcome, outcome])

  # Densities
  z.low <- (lower.bound - outcome.mean) / outcome.std
  z.up <- (upper.bound - outcome.mean) / outcome.std
  density.low <- stats::dnorm(lower.bound, mean = outcome.mean, sd = outcome.std)
  density.up <- stats::dnorm(upper.bound, mean = outcome.mean, sd = outcome.std)

  # G_matrices
  G_4mu <- 1 / outcome.std * (density.up - density.low)
  G_4sigma2 <- 1 / (2*outcome.std^2) * (density.up * z.up - density.low * z.low)

  # Jacobian
  jac_g4 <- cbind(G_4mu, G_4sigma2) %*% rbind(t(i_j) %*% jac_g1, t(i_jDn) %*% D_n %*% jac_g2)


  # Calculate asymptotic covariances ----

  AV.gamma_4 <- jac_g4 %*% acov %*% t(jac_g4)



  # Calculate asymptotic standard errors ----

  ase.gamma_4 <- sqrt(AV.gamma_4)


  # Calculate z-values

  zvalue.gamma_4 <- gamma_4 / ase.gamma_4


  # Prepare output ----

  # Console output
  if(verbose >= 2) {
    cat(paste0("  end of function ", fun.name.version, " ", Sys.time(), "\n"))
  }

  # Store results
  internal.list$interventional_distribution$probability$acov <- AV.gamma_4
  internal.list$interventional_distribution$probability$ase <- ase.gamma_4
  internal.list$interventional_distribution$probability$zvalue <- zvalue.gamma_4

  internal.list

}
