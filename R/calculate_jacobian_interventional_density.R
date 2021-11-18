## Changelog:
# CG 0.0.2 2021-11-18: added the user specified argument model which CURRENTLY NEEDS 
#                      TO BE THE internal_list
#                      changed name of function from _pdf to _density
# CG 0.0.1 2021-11-11: initial programming

## Documentation
#' @title Function to calculate Jacobian of the interventional pdf for a specific
#' interventional level and a specific value in the range of the outcome variable
#' @description Internal function that calculates Jacobian of the interventional mean vector
#'    for a specific interventional level and a specific value in the range of the outcome variable
#' @param model internal_list or object of class causalSEM 
#' @param x interventional level 
#' @param y value in the range of the outcome variable
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable 
#' @param verbose verbosity of console outputs
#' @return \code{calculate_jacobian_interventional_density} returns the
#'    Jacobian of interventional probability density function (numeric value) 
#'    as defined in Eq. 18c (p. 17)
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
calculate_jacobian_interventional_density <- function( model, x, y, intervention_names, outcome_name, verbose ){
  
  # function name
  fun.name <- "calculate_jacobian_interventional_density"
  
  # function version
  fun.version <- "0.0.2 2021-11-18"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # TODO check if user argument model is the internal_list or
  # an object of class causalSEM
  # CURRENTLY, the function assumes that the input model is
  # of type internal_list. After allowing for objects of class causalSEM
  # the pathes starting with internal_list$ might need adjustment
  
 
  
  # get variable names of interventional variables
  if( is.character( intervention_names ) && all( intervention_names %in% internal_list$info_model$var_names ) ){
    x_labels <- intervention_names
  } else {
    x_labels <- internal_list$info_interventions$intervention_name
  }
  
  # get variable name of outcome variable
  if( is.character( outcome_name ) && outcome_name %in% setdiff(internal_list$info_model$var_names, x_labels) ){
    y_label <- outcome_name
  } else {
    stop( paste0( fun.name.version, ": Argument outcome_name needs to be the a character string with the name of a non-interventional variable."  ) )
  }
  
  # get interventional levels
  if( is.numeric( x ) && length ( x ) == length (intervention_names) ){
    # set in internal_list
    x_values <- x
  } else {
    x_values <- internal_list$info_interventions$intervention_level
  }
  
  # get value in the range of outcome variable
  if( is.numeric( y ) && length ( y ) == 1 ){
    # set in internal_list
    y_value <- y
  } else {
    y_value <- internal_list$interventional_distribution$moments$mean_vector[y_label,1]
  }
  
  
  # get total number of variables
  # get number of unique parameters 
  
  n <- internal_list$info_model$n_ov
  n_unique <- internal_list$info_model$param$n_par_unique
  
  # get intervential mean and variance
  # TODO: assign E by calling the function calculate_interventional_means
  E <- internal_list$interventional_distribution$moments$mean_vector[y_label,1]
  V <- internal_list$interventional_distribution$moments$variance_matrix[y_label,y_label]
  
  # compute value of pdf
  
  f <- dnorm( y_value, mean=E, sd=sqrt(V) )
  
  # compute G3mu and G3Sigma from Corollary 11 in Gische and Voelkle (2021)
  # stack together in a matrix of appropriate dimensions
  
  G3mu <- (y_value-E) * (1 / V)
  G3Sigma <- (1 / 2) * (1 / V) * ((y_value-E)^2 * (1 / V) - 1)
  
  G3 <- matrix(c(G3mu,G3Sigma), ncol = 2, byrow = T)
  
  # compute selection matrices 
  
  j <- which(internal_list$info_model$var_names == y_label)
  ONE_N <- matrix(0, nrow = n, ncol = 1)
  ONE_N[j] <- 1
  
  # get Jacobians of gamma1 and gamma2 and multiply by 
  # corresponding selection matrices 
  # TODO: use handle verbose function instead of typting zero
  
  jac_gamma_1 <- calculate_jacobian_interventional_means(x_values, 0)
  
  ############## 
  # TODO: get jac_gamma_2 from the list or by calling the respective functions 
  
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
  
  
  # Calculate Jacobian g2 ----
  
  G_2C <- (I_n2 + K_n) %*%
    kronecker(X = C_trans %*% I_N %*% Psi %*% I_N, Y = I_n) %*%
    kronecker(X = t(C_trans), Y = C_trans %*% I_N)
  
  G_2Psi <- kronecker(X = C_trans, Y = C_trans) %*% kronecker(X = I_N, Y = I_N)
  
  # CG 0.0.4 2021-11-10: added formula for jac_g2 for 
  # vectorized variance-covariance matrix
  
  jac_g2 <- L_n %*% (G_2C %*% jac_C + G_2Psi %*% jac_Psi)
  
  ## end of TODO section
  ##########################
  
  jac_gamma_2 <- jac_g2
  
  
  jac_gamma_1_selected <- t(ONE_N) %*% jac_gamma_1
  jac_gamma_2_selected <- ((kronecker(X = t(ONE_N), Y = t(ONE_N))) %*%
                            internal_list$interventional_distribution$zero_one_matrices$duplication_matrix %*%
                            jac_gamma_2)
  
  # stack selected elements from the Jacobians in a matrix
  # of proper dimensions
  
  J <- matrix(c(jac_gamma_1_selected, jac_gamma_2_selected), ncol = n_unique, nrow = 2, byrow = T)
  
  # Compute Jacobian of gamma_4
  
  jac_gamma_3 <- f * G3 %*% J
  
  # return Jacobian of gamma_4
  return( jac_gamma_3 )
  
  
}



