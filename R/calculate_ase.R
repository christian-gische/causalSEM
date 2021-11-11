## Changelog:
# CG 0.0.4 2021-11-10: added formula for jac_g2 for 
#                      vectorized variance-covariance matrix
#                      corrected formula for ase_variances
# CG 0.0.3 2021-11-09: get elimination, duplication and commutation
# matrix from internal list, respectively.
# MA 0.0.2 2021-10-31: - uncommented gamma_1 and gamma_2
#                      - some asymptotic standard errors are zero. This is
#                      - not correct
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

calculate_ase <- function(internal_list) {

  # Verbosity and bug tracking ----

  # function name
  fun_name <- "calculate_ase"

  # function version
  fun_version <- "0.0.2 2021-10-31"

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
  I_N <- internal_list$interventional_distribution$zero_one_matrices$eliminate_intervention

  # Elimination, duplication, and commutation matrices
  # CG 0.0.3 2021-11-09: get elimination, duplication and commutation
  # matrix from internal list, respectively.
  L_n <- internal_list$interventional_distribution$zero_one_matrices$elimination_matrix
  D_n <- internal_list$interventional_distribution$zero_one_matrices$duplication_matrix
  K_n <- internal_list$interventional_distribution$zero_one_matrices$commutation_matrix
  
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

  # Interventional covariance
  gamma_2 <- internal_list$interventional_distribution$moments$variance_matrix

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

  # CG 0.0.4 2021-11-10: added formula for jac_g2 for 
  # vectorized variance-covariance matrix
  jac_g2 <- L_n %*% (G_2C %*% jac_C + G_2Psi %*% jac_Psi)
  jac_g2_vec <-  (G_2C %*% jac_C + G_2Psi %*% jac_Psi)


  # Calculate asymptotic covariances ----

  AV_gamma_1 <- jac_g1 %*% acov %*% t(jac_g1)

  AV_gamma_2 <- jac_g2 %*% acov %*% t(jac_g2)
  AV_gamma_2_vec <- jac_g2_vec %*% acov %*% t(jac_g2_vec)


  # Calculate asymptotic standard errors ----

  ase_gamma_1 <- sqrt(diag(AV_gamma_1))

  # CG 0.0.4 2021-11-10: corrected formula for ase_variances
  labels_AV_gamma_2_vec <- expand.grid(internal_list$info_model$var_names,internal_list$info_model$var_names)
  labels_AV_gamma_2_vec <- paste0(labels_AV_gamma_2_vec$Var1,labels_AV_gamma_2_vec$Var2)
  
  AV_gamma_2_vec <- as.matrix(AV_gamma_2_vec)
  colnames(AV_gamma_2_vec) <- labels_AV_gamma_2_vec
  rownames(AV_gamma_2_vec) <- labels_AV_gamma_2_vec
  
  variance_labels <- paste0(internal_list$info_model$var_names,internal_list$info_model$var_names)
  
  AV_variances <- AV_gamma_2_vec[variance_labels,variance_labels]
  
  ase_variances <- sqrt(diag(AV_variances))

  # Calculate z-values

  z_gamma_1 <- gamma_1 / ase_gamma_1

  ## Are these the correct z-values
  z_variances <- diag(gamma_2) / ase_variances


  # Prepare output ----

  # Console output
  if(verbose >= 2) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # Store results
  internal_list$interventional_distribution$acov <- list(
    acov_means = AV_gamma_1,
    ase_se = AV_gamma_2
    # TODO Change names of slots in list
    # acov_interventional_means = AV_gamma_1,
    # acov_vec_intervenional_covariance_matrix = AV_gamma_2
  )

  internal_list$interventional_distribution$ase <- list(
    ase_means = ase_gamma_1,
    ase_covariance = ase_variances
    # TODO Change names of slots in list
    # ase_interventional_means = AV_gamma_1,
    # ase_interventional_variances = AV_gamma_2
  )

  internal_list$interventional_distribution$z_values <- list(
    z_means = z_gamma_1,
    z_variance = z_variances
    # TODO Change names of slots in list
    # z_interventional_means = AV_gamma_1,
    # z_interventional_variances = AV_gamma_2
  )

  internal_list

}
