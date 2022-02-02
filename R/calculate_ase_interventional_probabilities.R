## Changelog:
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
  fun_version <- "0.0.2 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- model$control$verbose

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # TODO check if user argument model is the internal_list or
  # an object of class causalSEM
  # CURRENTLY, the function assumes that the input model is
  # of type internal_list. After allowing for objects of class causalSEM
  # the pathes starting with internal_list$ might need adjustment

  # get variable names of interventional variables
  if (is.character(intervention_names) && 
      all(intervention_names %in% model$info_model$var_names)) {
    x_labels <- intervention_names
  } else {
    x_labels <- model$info_interventions$intervention_names
  }
  
  # get interventional levels
  if (is.numeric(x) && length (x) == length(intervention_names)) {
    x_values <- x
  } else {
    x_values <- model$info_interventions$intervention_levels
  }
  
  # get variable names of outcome variables
  if( is.character( outcome_names ) && 
      outcome_names %in% setdiff(model$info_model$var_names, 
                                 x_labels) ){
    y_labels <- outcome_names
  } else {
    stop( paste0( fun.name.version, ": Argument outcome_names needs to be the 
                  a character string with the name of a non-interventional 
                  variable."  ) )
  }
  
  # get lower bound of outcome range 
  # TODO: allow lower bounds to be multivariate
  if( is.numeric( lower_bounds ) && length ( lower_bounds ) == 1 &&
      model$info_interventions$n_outcome == 1 ){
  
    lower_bounds <- lower_bounds 
    
  } else if ( is.null( lower_bounds ) ){
    # TODO: give warning / error if model$info_interventions$lower_bounds
    # has no entry
    
    lower_bounds <- model$info_interventions$lower_bounds
    
  } else {
    
    stop( paste0( fun.name.version, ": setting lower_bounds failed. Argument 
                  lower_bounds needs to be a numeric scalar."  ) )
  }
  
  
  # TODO: option to provide same number of upper and lower bounds as 
  # verbose: provide lower and upper bound in the same order as outcome 
  # variable or as named vector; if no outcome variable is provided as argument 
  # force user to name upper and lower bound which have to be the same 
  # dimension as vector of non interventional variables 
  # internally: always name upper and lower bound in internal list and bring in 
  # same order as outcome names
  # outcome variables of interest 
  # caution: muliple upper bounds need to be in the same order as multivariate 
  # outcome variable
  # CAUTION: order of upper bounds in case outcome variable is not user 
  # specified 
  
  # get upper bounds of outcome range 
  # TODO: allow lower bounds to be multivariate
  
  if( is.numeric(upper_bounds ) && length ( upper_bounds ) == 1 &&
      model$info_interventions$n_outcome == 1 ){
    
    upper_bounds <- upper_bounds
    
  } else if ( is.null( upper_bounds ) ){
    # TODO: give warning / error if model$info_interventions$lower_bounds
    # has no entry
    upper_bounds <- model$info_interventions$upper_bounds 
    
  } else {
    stop( paste0( fun.name.version, ": setting upper_bounds failed. Argument 
                  lower_bounds needs to be a numeric scalar." ) )
  }
  

  # get total number of variables
  # get number of unique parameters
  
  n <- model$info_model$n_ov
  n_unique <- model$info_model$param$n_par_unique

  # get intervential probability
  
  gamma_4 <- model$interventional_distribution$probabilities$values

  # compute jacobian of the pdf
  jac_g4 <- calculate_jacobian_interventional_probabilities(
    model = model,
    x = x_values,
    intervention_names = x_labels,
    outcome_names = y_labels,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    verbose = verbose
  )

  # get AV of parameter vector
  acov <- model$info_model$param$varcov_par_unique

  # compute asymptotic variance
  acov_gamma_4 <- jac_g4 %*% acov %*% t(jac_g4)

  # compute asymptotic standard errors
  ase_gamma_4 <- sqrt(diag(acov_gamma_4))

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
