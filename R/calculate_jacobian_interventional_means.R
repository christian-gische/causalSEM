## Changelog:
# CG 0.0.1 2021-11-10: initial programming

## Documentation
#' @title Function to calculate Jacobian of the interventional mean vector for a specific
#' interventional level
#' @description Internal function that calculates Jacobian of the interventional mean vector
#'    for a specific interventional level
#' @param x interventional level  
#' @param verbose verbosity of console outputs
#' @return \code{calculate_jacobian_interventional_means} returns the
#'    Jacobian of interventional mean vector (numeric matrices) 
#'    as defined in Eq. 18a (p. 17)
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
calculate_jacobian_interventional_means <- function( x, verbose ){
  
  # function name
  fun.name <- "calculate_jacobian_interventional_means"
  
  # function version
  fun.version <- "0.0.1 2021-11-10"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  
  # Prepare elements ----
  
  # Number of observed variables
  n <- internal_list$info_model$n_ov
  
  # Identity matrices
  I_n <- diag(n)
 
  # Selection
  ONE_I <- internal_list$interventional_distribution$zero_one_matrices$select_intervention
  I_N <- internal_list$interventional_distribution$zero_one_matrices$eliminate_intervention
  
  # C and Psi matrices
  C <- internal_list$info_model$C$values
  
  # Jacobian matrices
  jac_C <- internal_list$info_model$C$derivative
 
  # Asymptotic covariance matrix of the model parameters
  acov <- internal_list$info_model$param$varcov_par_unique
  
  # Compute transformation matrix
  ## I cannot think of a better name. Feel free to change
  C_trans <- solve(I_n - I_N %*% C)
  
  
  # Calculate Jacobian g1 ----
  # TODO: allow x to be a vector
  
  jac_g1 <- kronecker(X = t(x) %*% t(ONE_I) %*% t(C_trans), Y = C_trans %*% I_N) %*% jac_C 
    
  # return p
  return( jac_g1 )
}
  


