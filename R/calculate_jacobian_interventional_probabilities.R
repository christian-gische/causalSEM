## Changelog:
# CG 0.0.3 2022-01-13:  changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# CG 0.0.2 2021-11-24: changed $variane to $covariance in internal list
# MA 0.0.1 2021-11-19: initial programming

## Documentation
#' @title Calculate Jacobian of the Probablity of an Interventional Event 
#' @description Calculates Jacobian of the probability of a univariate 
#' interventional evenent. The probability is calculated for (i) a univariate 
#' outcome variable, (ii) a specific interventional level, and (iii )specific 
#' values of the lower and upper bound of the critical range of the outcome 
#' variable.
#' @param model internal_list or object of class causalSEM
#' @param x interventional levels
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable
#' @param lower_bounds numeric with lower bound
#' @param upper_bounds numeric with upper bound
#' @param verbose verbosity of console outputs
#' @return \code{calculate_jacobian_interventional_probabilities} returns the
#'    Jacobian of interventional probabilities (numeric matrices)
#'    as defined in Eq. 18a (p. 17)
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
calculate_jacobian_interventional_probabilities <- function(
  model, x, intervention_names, outcome_names, lower_bounds, upper_bounds, 
  verbose
  ){

  # Verbosity and bug tracking ----

  # function name
  fun_name <- "calculate_jacobian_interventional_probabilities"

  # function version
  fun_version <- "0.0.3 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- model$control$verbose

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }


  # Prepare elements ----

  # Number of observed variables
  n <- model$info_model$n_ov

  # Calculate zero-one matrices
  constant_matrices <- calculate_constant_matrices(
    model = model,
    intervention_names = intervention_names,
    outcome_names = outcome_names,
    verbose = verbose
  )

  # Identity matrices
  I_n <- diag(n)
  I_n2 <- diag(n^2)

  # Selection matrices
  ONE_I <- constant_matrices$select_intervention
  I_N <- constant_matrices$eliminate_intervention
  i_j <- constant_matrices$select_outcome

  # Elimination, duplication, and commutation matrices
  L_n <- constant_matrices$elimination_matrix
  D_n <- constant_matrices$duplication_matrix
  K_n <- constant_matrices$commutation_matrix

  # Select outcomes
  i_jDn <- matrix(0, nrow = n^2, ncol = length(outcome_names))
  for (i in 1:length(outcome_names)) {
    j <- which(i_j[, i] == 1)
    i_jDn[(j - 1) * n + j, i] <- 1
  }

  # C and Psi matrices
  C <- model$info_model$C$values
  Psi <- model$info_model$Psi$values

  # Jacobian matrices
  jac_C <- model$info_model$C$derivative
  jac_Psi <- model$info_model$Psi$derivative

  # Interventional means
  gamma_1 <- model$interventional_distribution$means$values

  # Interventional variance-covariance matrix
  # CG 0.0.1 2021-11-24: changed $variane to $covariance in internal list
  gamma_2 <- model$interventional_distribution$covariance_matrix$values

  # Compute transformation matrix
  C_trans <- solve(I_n - I_N %*% C)

  # Jacobian of the interventional means
  jac_g1 <- calculate_jacobian_interventional_means(
    model = model,
    x = x,
    intervention_names = intervention_names,
    outcome_names = outcome_names,
    verbose = verbose)

  # Jacobian of the interventional variances
  jac_g2 <- calculate_jacobian_interventional_variances(
    model = model,
    intervention_names = intervention_names,
    outcome_names = outcome_names,
    verbose = verbose)


  # Calculate Jacobian g4 ----

  # Select outcome mean and standard deviation
  outcome_mean <- gamma_1[outcome_names, 1]
  outcome_std <- sqrt(gamma_2[outcome_names, outcome_names])

  # Densities
  z_low <- (lower_bounds - outcome_mean) / outcome_std
  z_up <- (upper_bounds - outcome_mean) / outcome_std
  density_low <- stats::dnorm(lower_bounds, mean = outcome_mean, 
                              sd = outcome_std)
  density_up <- stats::dnorm(upper_bounds, mean = outcome_mean, 
                             sd = outcome_std)

  # G_matrices
  G_4mu <- 1 / outcome_std * (density_up - density_low)
  G_4sigma2 <- 1 / (2*outcome_std^2) * (density_up * z_up - density_low * z_low)

  # Jacobian
  jac_g4 <- cbind(G_4mu, G_4sigma2) %*% rbind(t(i_j) %*% jac_g1, t(i_jDn) %*% 
                                                D_n %*% jac_g2)

  # Output
  jac_g4

}
