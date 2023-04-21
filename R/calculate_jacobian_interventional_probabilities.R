## Changelog:
# CG 0.0.5 2022-03-09:  set default values 
# CG 0.0.4 2022-03-08:  allow for multivariate lower and upper bounds
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
# TODO: we could remove the arguments x, and intervention_names
# or change the code to provide full flexibility (see TODOS below)

# CG 0.0.5 2022-03-09:  set default values 
calculate_jacobian_interventional_probabilities <- 
  function(model = NULL,
           x = NULL,
           intervention_names = NULL,
           outcome_names = NULL,
           lower_bounds = NULL,
           upper_bounds = NULL,
           verbose = NULL ){
  
  # Verbosity and bug tracking ----

  # function name
  fun_name <- "calculate_jacobian_interventional_probabilities"

  # function version
  fun_version <- "0.0.4 2022-03-08"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")
  
    # get verbose argument
  if ( is.null(verbose) ){
        verbose <- model$control$verbose
  } else {
  verbose <- verbose
  }

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }
  
  # TODO plausibility check of user argument "model": should be internal_list or
  # an object of class causalSEM
  
  # CG 0.0.5 2022-03-09:  set default values
  if ( is.null( c( x, intervention_names, outcome_names, lower_bounds, 
                   upper_bounds) ) ){
    
    x <- model$info_interventions$intervention_levels
    intervention_names <- model$info_interventions$intervention_names
    outcome_names <- model$info_interventions$outcome_names
    lower_bounds <- model$info_interventions$lower_bounds
    upper_bounds <- model$info_interventions$upper_bounds
    
    if( any( is.null( c( lower_bounds, upper_bounds ) ) ) ){
      jac_g4 <- NULL
      jac_g4 
    } else {
    
    # Number of observed variables
    n <- model$info_model$n_ov
    n_outcome <- length(outcome_names)
    
    # Identity matrices
    I_n <- diag(n)
    I_n2 <- diag(n^2)
    
    # Selection matrices
    ONE_I <- model$constant_matrices$select_intervention
    I_N <- model$constant_matrices$eliminate_intervention
    
    # TODO: Check if the selection matrix is correctly defined and will also 
    # work for multivariate inputs
    
    i_j <- model$constant_matrices$select_outcome
    
    # Elimination, duplication, and commutation matrices
    L_n <- model$constant_matrices$elimination_matrix
    D_n <- model$constant_matrices$duplication_matrix
    K_n <- model$constant_matrices$commutation_matrix
    
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
    gamma_2 <- model$interventional_distribution$covariance_matrix$values
    
    # Compute transformation matrix
    C_trans <- solve(I_n - I_N %*% C)
    
    # Jacobian of the interventional means
    jac_g1 <- model$interventional_distribution$means$jacobian
    
    # Jacobian of the interventional variances
    jac_g2 <- model$interventional_distribution$covariance_matrix$jacobian
    
    # Calculate Jacobian of interventional probabilities
    
    jac_g4_list <- vector("list", n_outcome)
    names(jac_g4_list) <- outcome_names
    
    for (i in 1:n_outcome){
      # Select outcome mean and standard deviation
      outcome_mean <- gamma_1[outcome_names[i], 1]
      outcome_std <- sqrt(gamma_2[outcome_names[i], outcome_names[i]])
      
      # Densities
      z_low <- (lower_bounds[i] - outcome_mean) / outcome_std
      z_up <- (upper_bounds[i] - outcome_mean) / outcome_std
      density_low <- stats::dnorm(lower_bounds[i], mean = outcome_mean, 
                                  sd = outcome_std)
      density_up <- stats::dnorm(upper_bounds[i], mean = outcome_mean, 
                                 sd = outcome_std)
      
      # G_matrices
      G_4mu <- 1 / outcome_std * (density_up - density_low)
      G_4sigma2 <- 1 / (2*outcome_std^2) * (density_up * z_up - 
                                              density_low * z_low)
      
      # Jacobian
      jac_g4_list[[i]]<- 
        cbind(G_4mu, G_4sigma2) %*% rbind(t(i_j[,i]) %*% jac_g1, 
                                          t(i_jDn[,i]) %*%  D_n %*% jac_g2)
    }
    
    jac_g4 <- jac_g4_list
    jac_g4
    }
    
  } else {
  
  # CG 0.0.4 2022-03-08:  allow for multivariate lower and upper bounds
  # get variable names of interventional variables
  if (is.character(intervention_names) && 
      all(intervention_names %in% model$info_model$var_names)) {
    intervention_names <- intervention_names
  } else {
    stop( paste0( fun.name.version, ": Calculation of asymptotics of 
    interventional probabilities failed. Argument intervention_names needs to 
    be a character vector of variable names."  ) )
  }
  
  # CG 0.0.4 2022-03-08:  allow for multivariate lower and upper bounds
  # get interventional levels
  if (is.numeric(x) && length (x) == length(intervention_names)) {
    x <- x
  } else {
    stop( paste0( fun.name.version, ": Calculation of asymptotics of 
    interventional probabilities failed. Argument x needs to be of same length
    as argument intervention_names."  ) )
  }
  
  # CG 0.0.4 2022-03-08:  allow for multivariate lower and upper bounds
  # plausibility check for argument outcome
  if( is.character( outcome_names ) && 
      all( outcome_names %in% model$info_model$var_names ) ){
    # set in internal_list
    outcome_names <- outcome_names
  } else if ( is.null( outcome_names ) ){
    outcome_names <- 
      setdiff(model$info_model$var_names, 
              model$info_interventions$intervention_names)
  } else {
    stop( paste0( fun.name.version, ": Calculation of Jacobian of interventional
    probabilities failed. Argument outcome_name needs to be a character vector 
    of variable names."  ) )
  }
  
  # get number of interventional variables 
  # set in internal_list
  n_outcome <- length(outcome_names)
  
  # get lower bound of outcome range 
  if( is.numeric( lower_bounds ) && 
      length ( lower_bounds ) == n_outcome)
  {
    # set in internal_list
    lower_bounds <- lower_bounds 
  } else {
    stop( paste0( fun.name.version, ": Calculation of Jacobian of interventional
    probabilities failed. Argument lower_bounds needs to be numeric and of 
    same length as the argument outcome_names."  ) )
  }
  
  # get upper bounds of outcome range 

  if( is.numeric( upper_bounds ) && 
      length ( upper_bounds ) == n_outcome)
  {
    # set in internal_list
    upper_bounds <- upper_bounds 
  } else {
    stop( paste0( fun.name.version, ": Calculation of Jacobian of interventional
    probabilities failed. Argument upper_bounds needs to be numeric and of 
    same length as the argument outcome_names."  ) )
  }

  if( any( is.null( c( lower_bounds, upper_bounds ) ) ) ){
    jac_g4 <- NULL
    jac_g4 
  } else {
  
  # Number of observed variables
  n <- model$info_model$n_ov

  #Calculate zero-one matrices
  constant_matrices <- calculate_constant_matrices(
    model = model,
    intervention_names = intervention_names,
    outcome_names = outcome_names)

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
  gamma_1 <- calculate_interventional_means( C = C, x = x, SI = ONE_I, n = n, 
                                             IN = I_N, verbose = verbose )
  
  # Interventional variance-covariance matrix
  gamma_2 <- calculate_interventional_covariance_matrix( C = C, Psi = Psi, 
                                                         x = x, SI = ONE_I, 
                                                         n = n, IN = I_N,
                                                         verbose = verbose )
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
  
  
  jac_g4_list <- vector("list", n_outcome)
  names(jac_g4_list) <- outcome_names
   
  for (i in 1:n_outcome){
  # Select outcome mean and standard deviation

  outcome_mean <- gamma_1[outcome_names[i], 1]
  outcome_std <- sqrt(gamma_2[outcome_names[i], outcome_names[i]])
  
  # Densities
  z_low <- (lower_bounds[i] - outcome_mean) / outcome_std
  z_up <- (upper_bounds[i] - outcome_mean) / outcome_std
  density_low <- stats::dnorm(lower_bounds[i], mean = outcome_mean, 
                              sd = outcome_std)
  density_up <- stats::dnorm(upper_bounds[i], mean = outcome_mean, 
                             sd = outcome_std)

  # G_matrices
  G_4mu <- 1 / outcome_std * (density_up - density_low)
  G_4sigma2 <- 1 / (2*outcome_std^2) * (density_up * z_up - density_low * z_low)

  # Jacobian
  jac_g4_list[[i]]<- 
    cbind(G_4mu, G_4sigma2) %*% rbind(t(i_j[,i]) %*% jac_g1, 
                                              t(i_jDn[,i]) %*%  D_n %*% jac_g2)
  }
  
  jac_g4 <- jac_g4_list
  jac_g4
  }
 }
}
