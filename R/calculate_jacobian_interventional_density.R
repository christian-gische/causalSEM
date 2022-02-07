## Changelog:
# CG 0.0.5 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# CG 0.0.4 2021-11-24: changed $variance to $covariance in internal_list path
# CG 0.0.3 2021-11-22: get jacobian of gamma1 and gamma2 by calling 
# corresponding 
#                      calculate_jacobian function 
# CG 0.0.2 2021-11-18: added the user specified argument model which CURRENTLY 
#                      NEEDS TO BE THE internal_list
#                      changed name of function from _pdf to _density
# CG 0.0.1 2021-11-11: initial programming

## Documentation
#' @title Calculate the Jacobian of the Density Function of the Interventional 
#' Distribution
#' @description Calculate the Jacobian of the probability density function (pdf)
#' of the Interventional distributionfor a specific interventional level and a 
#' specific value in the range of the outcome variable.
#' @param model internal_list or object of class causalSEM 
#' @param x interventional level 
#' @param y value in the range of the outcome variable
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable 
#' @param verbose verbosity of console outputs
#' @return \code{calculate_jacobian_interventional_density} returns the
#'    Jacobian of interventional probability density function (numeric value) 
#'    as defined in Eq. 18c (p. 17)
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal
#' 
#' 
#' 
## Function definition
calculate_jacobian_interventional_density <- 
  function( model, x, y, intervention_names, outcome_names, verbose ){
  
  # function name
  fun.name <- "calculate_jacobian_interventional_density"
  
  # function version
  fun.version <- "0.0.5 2022-01-13"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", 
                                  Sys.time(), "\n" ) )
  
  # TODO check if user argument model is the internal_list or
  # an object of class causalSEM
  # CURRENTLY, the function assumes that the input model is
  # of type internal_list. After allowing for objects of class causalSEM
  # the pathes starting with internal_list$ might need adjustment
  
  # get variable names of interventional variables
  if( is.character( intervention_names ) && 
      all( intervention_names %in% model$info_model$var_names ) ){
    x_labels <- intervention_names
  } else {
    x_labels <- model$info_interventions$intervention_names
  }
  
  # get variable name of outcome variable
  # TODO allow calculation only for NON interventional variables 
#  if( is.character( outcome_names ) && 
#  outcome_names %in% setdiff(model$info_model$var_names, x_labels) ){
#    y_labels <- outcome_names
#  } else {
#    stop( paste0( fun.name.version, ": Argument outcome_names needs to be the a 
#                  character string with the name of a non-interventional 
#                  variable."  ) )
#  }
  
  if( is.character( outcome_names ) && 
    all ( outcome_names %in% model$info_model$var_names ) ){
    y_labels <- outcome_names
  } else {
    stop( paste0( fun.name.version, ": Argument outcome_names needs to be the a 
                  character string with the name of an observed variable."  ) )
  }
  
  # get interventional levels
  if( is.numeric( x ) && length ( x ) == length (intervention_names) ){
    x_values <- x
  } else {
    x_values <- model$info_interventions$intervention_levels
  }
  
  # get value in the range of outcome variable
  if( is.numeric( y ) && length ( y ) == 1 ){
    y_value <- y
  } else {
    y_value <- model$interventional_distribution$means$values[y_labels,1]
  }
  
  
  # get total number of variables
  # get number of unique parameters 
  
  n <- model$info_model$n_ov
  n_unique <- model$info_model$param$n_par_unique
  
  # get intervential mean and variance
  # TODO: assign E by calling the function calculate_interventional_means
  E <- model$interventional_distribution$means$values[y_labels,1]
  V <- 
    model$interventional_distribution$covariance_matrix$values[y_labels,
                                                               y_labels]
  
  # compute value of interventional density
  f <- stats::dnorm( y_value, mean=E, sd=sqrt(V) )
  
  # compute G3mu and G3Sigma from Corollary 11 in Gische and Voelkle (2021)
  # stack together in a matrix of appropriate dimensions
  
  G3mu <- (y_value-E) * (1 / V)
  G3Sigma <- (1 / 2) * (1 / V) * ((y_value-E)^2 * (1 / V) - 1)
  
  G3 <- matrix(c(G3mu,G3Sigma), ncol = 2, byrow = T)
  
  # compute selection matrices 
  
  j <- which(model$info_model$var_names == y_labels)
  ONE_N <- matrix(0, nrow = n, ncol = 1)
  ONE_N[j] <- 1
  
  # get Jacobians of gamma1 and gamma2 and multiply by 
  # corresponding selection matrices 
  
  jac_gamma_1 <- 
    calculate_jacobian_interventional_means(
      model = model, 
      x = x_values, 
      intervention_names = x_labels, 
      outcome_names = y_labels, 
      verbose = model$control$verbose )
  
  
  
  jac_gamma_2 <- 
    calculate_jacobian_interventional_variances(
      model = model, 
      intervention_names = x_labels, 
      outcome_names = y_labels, 
      verbose = model$control$verbose)
  
 
  jac_gamma_1_selected <- t(ONE_N) %*% jac_gamma_1
  jac_gamma_2_selected <- 
    ((kronecker(X = t(ONE_N), Y = t(ONE_N))) %*%
      model$constant_matrices$duplication_matrix %*%jac_gamma_2)
  
  # stack selected elements from the Jacobians in a matrix
  # of proper dimensions
  
  J <- matrix(c(jac_gamma_1_selected, jac_gamma_2_selected), 
              ncol = n_unique, nrow = 2, byrow = T)
  
  # Compute Jacobian of gamma_3
  
  jac_gamma_3 <- f * G3 %*% J
  
  # return Jacobian of gamma_3
  return( jac_gamma_3 )
  
  
}



