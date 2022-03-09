## Changelog:
# CG 0.0.3 2022-03-08:  allow for multivariate lower and upper bounds
# CG 0.0.2 2022-01-13:  changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.1 2021-11-19: initial programming

## Documentation
#' @title Calculate Asymptotic Standard Errors of Interventional Probabilities
#' @description Calculates the asysmptotic covariance matrix,
#' the aysmptotic standard errors, and the approximate z-values of the 
#' interventional probabilities for a specific interventional level and a 
#' specific range (lower_bounds, upper_bounds) of the outcome variables.
#' @param model internal_list or object of class causalSEM
#' @param x interventional levels
#' @param intervention_names names of interventional variables
#' @param outcome_names names of outcome variables
#' @param lower_bounds numeric with lower bounds
#' @param upper_bounds numeric with upper bounds
#' @param verbose verbosity of console outputs
#' @return Asysmptotic covariance matrix, the aysmptotic standard errors, and
#' the approximate z-values of the the interventional interventional  
#' probabilities for a specific interventional level and a specific range 
#' (lower_bounds, upper_bounds) of the outcome variables. 
#' (see Corollaries, 10, 11 in Gische and Voelkle, 2021).
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z

calculate_ase_interventional_probabilities <- 
  function(model, x, intervention_names, outcome_names, lower_bounds, 
           upper_bounds, verbose) {

  # function name
  fun_name <- "calculate_ase_interventional_probabilities"

  # function version
  fun_version <- "0.0.3 2022-03-08"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- model$control$verbose

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # TODO plausibility check of user argument "model": should be internal_list or
  # an object of class causalSEM
  # CURRENTLY, the function assumes that the input model is
  # of type internal_list. After allowing for objects of class causalSEM
  # the pathes starting with internal_list$ might need adjustment
  
  # get variable names of interventional variables
  if (is.character(intervention_names) && 
      all(intervention_names %in% model$info_model$var_names)) {
    intervention_names <- intervention_names
  } else {
    stop( paste0( fun.name.version, ": Calculation of asymptotics of 
    interventional probabilities failed. Argument intervention_names needs to 
    be a character vector of variable names."  ) )
  }
  
  # get interventional levels
  if (is.numeric(x) && length (x) == length(intervention_names)) {
    x <- x
  } else {
    stop( paste0( fun.name.version, ": Calculation of asymptotics of 
    interventional probabilities failed. Argument x needs to be of same length
    as argument intervention_names."  ) )
  }
  
  
  # CG 0.0.3 2022-03-08:  allow for multivariate lower and upper bounds
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
  
  # get number of outcome variables 
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

  # get total number of variables
  # get number of unique parameters
  
  n <- model$info_model$n_ov
  n_unique <- model$info_model$param$n_par_unique

  # get intervential probability
  # TODO: To be consistent we would have to call the function 
  # calcualte interventional means here
  gamma_4 <- model$interventional_distribution$probabilities$values

  # compute jacobian of the pdf
  jac_g4 <- calculate_jacobian_interventional_probabilities(
    model = model,
    x = x,
    intervention_names = intervention_names,
    outcome_names = outcome_names,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    verbose = verbose
  )

  # get AV of parameter vector
  acov <- model$info_model$param$varcov_par_unique

  # compute asymptotic variance
  acov_gamma_4 <- numeric(n_outcome)
  
  for (i in 1:n_outcome) {
    
    acov_gamma_4[i] <- jac_g4[[i]]%*% acov %*% t(jac_g4[[i]])
    
  }
  
   # compute asymptotic standard errors
  ase_gamma_4 <- sqrt(acov_gamma_4)

  # compute approximate z-value
  z_gamma_4 <- gamma_4 / ase_gamma_4

  # Prepare output ----

  # Console output
  if (verbose >= 2) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # Output
  list(gamma_4 = gamma_4,
       acov_gamma_4 = acov_gamma_4,
       ase_gamma_4 = ase_gamma_4,
       z_gamma_4 = z_gamma_4)

}
