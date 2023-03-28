## Changelog:
# CG 0.0.3 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# CG 0.0.3 2021-11-24: changed $variance to $covariance in internal_list path
# CG 0.0.2 2021-11-18: adding the user specified argument model which CURRENTLY  
#                      NEEDS TO BE THE internal_list
#                      changed name of function from _pdf to _density
# CG 0.0.1 2021-11-11: initial programming

## Documentation
#' @title Calculate Asymptotics of the Interventional pdf 
#' @description Calculates the asysmptotic variance,
#' the aysmptotic standard error, and the approximate z-value of the  
#' interventional probability density function (pdf) for a specific 
#' interventional level and a specific value in the range of a univariate 
#' outcome variable. 
#' @param model An object of class causalSEM (e.g., the output of the 
#' intervention_effect() function)
#' @param x interventional levels 
#' @param y values within the range of the outcome variable
#' @param intervention_names names of interventional variables
#' @param outcome_names names of outcome variables 
#' @param verbose verbosity of console outputs
#' @return A list containing the asysmptotic covariance matrix, 
#' the aysmptotic standard errors, and the approximate z-value of the 
#' the interventional pdf for a specific interventional level and a specific 
#' value in the range of the outcome variable.
#' (see Corollaries, 10, 11 in Gische and Voelkle, 2021)
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z

# TODO: currently the function is written to handle a single, scalar-valued
# outcome variable. Function needs adjustment to be able to handle several
# outcome variables as arguments.

calculate_ase_interventional_density <- function(model, x, y, 
                                intervention_names, outcome_names, verbose) {
  
  # function name
  fun_name <- "calculate_ase_interventional_density"
  
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
  
  # TODO check if user argument model is the internal_list or
  # an object of class causalSEM
  # CURRENTLY, the function assumes that the input model is
  # of type internal_list. After allowing for objects of class causalSEM
  # the pathes starting with internal_list$ might need adjustment
  
  
  # model_aux <- model
  
  
  # get variable names of interventional variables
  # TODO: Rethink setting a default here
  if( is.character( intervention_names ) &&
      all( intervention_names %in% model$info_model$var_names ) ){
    x_labels <- intervention_names
  } else {
    x_labels <- model$info_interventions$intervention_names
  }
  
  # get variable name of outcome variable
  # TODO allow calculation only for NON interventional variables 
  #  if( is.character( outcome_names ) && 
  #  all(outcome_names %in% setdiff(model$info_model$var_names, x_labels) )){
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
        stop( paste0( fun.name.version, ": Argument outcome_names needs to be a 
                  character string with the name of an observed variable."  ) )
      }
  
  # get interventional levels
  # TODO: Rethink setting a default here
  if( is.numeric( x ) && length ( x ) == length (intervention_names) ){
    x_values <- x
  } else {
    x_values <- model$info_interventions$intervention_levels
  }
  
  # get single value in the range of outcome variable
  # TODO: Rethink setting a default here
  if( is.numeric( y ) && length ( y ) == 1 ){
    y_values <- y
  } else {
    y_values <- 
      model$interventional_distribution$means$values[y_labels,1]
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
  
  # compute value of pdf
  gamma_3 <- stats::dnorm( y_values, mean=E, sd=sqrt(V) )
  
  # compute jacobian of the pdf
  jac_gamma_3 <- 
    calculate_jacobian_interventional_density( model = model,
                                               x = x_values,
                                               y = y_values,
                                               intervention_names = x_labels, 
                                               outcome_names = y_labels, 
                                               verbose = verbose)
  
  # get AV of parameter vector 
  acov <- model$info_model$param$varcov_par_unique
  
  # compute asymptotic variance
  acov_gamma_3 <- jac_gamma_3 %*%  acov %*% t(jac_gamma_3) 
  
  # compute asymptotic standard errors
  ase_gamma_3 <- sqrt(acov_gamma_3)
  
  
  # compute approximate z-value
  z_gamma_3 <- gamma_3 / ase_gamma_3
  
  # Prepare output ----
  
  # Console output
  if(verbose >= 2) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n"))
  }
  
  # Store results
  
  return(list(gamma_3 = gamma_3,
              jac_gamma_3 = jac_gamma_3,
              acov_gamma_3 = acov_gamma_3,
              ase_gamma_3 = ase_gamma_3,
              z_gamma_3 = z_gamma_3)
         )
   
}
