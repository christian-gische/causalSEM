## Changelog:
# CG 0.0.2 2021-11-18: adding the user specified argument model which CURRENTLY NEEDS 
#                      TO BE THE internal_list
#                      changed name of function from _pdf to _density
# CG 0.0.1 2021-11-11: initial programming

## Documentation
#' @title Calculate asymptotic standard errors of the interventional pdf for a specific
#' interventional level and a specific value in the range of the outcome variable
#' @description The function calculate_ase_interventional_density() calculates the asysmptotic 
#' covariance matrix, the aysmptotic standard errors, and the z-value of the 
#' the interventional pdf for a specific interventional level and a specific value 
#' in the range of the outcome variable. Currently, the function only accepts a 
#' single scalar outcome variable.
#' @param model internal_list or object of class causalSEM 
#' @param x interventional level 
#' @param y value in the range of the outcome variable
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable 
#' @param verbose verbosity of console outputs
#' @return \code{calculate_ase_interventional_density} returns the 
#' asysmptotic covariance matrix, the aysmptotic standard errors, and the approximate z-value of the 
#' the interventional pdf for a specific interventional level and a specific value 
#' in the range of the outcome variable (see Corollaries, 10, 11 in Gische and Voelkle, 2021)
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}

calculate_ase_interventional_density <- function(model, x, y, intervention_names, outcome_name, verbose) {
  
  # function name
  fun_name <- "calculate_ase_interventional_density"
  
  # function version
  fun_version <- "0.0.2 2021-11-18"
  
  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
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
    x_values <- x
  } else {
    x_values <- internal_list$info_interventions$intervention_level
  }
  
  # get value in the range of outcome variable
  if( is.numeric( y ) && length ( y ) == 1 ){
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
  gamma_3 <- dnorm( y_value, mean=E, sd=sqrt(V) )
  
  # compute jacobian of the pdf
  jac_gamma_3 <- calculate_jacobian_interventional_density( x = x_values,
                                                        y = y_value,
                                                        intervention_names = "x2", 
                                                        outcome_name = "y3", 
                                                        verbose = verbose)
  
  # get AV of parameter vector 
  acov <- internal_list$info_model$param$varcov_par_unique
  
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
              acov_gamma_3 = acov_gamma_3,
              ase_gamma_3 = ase_gamma_3,
              z_gamma_3 = z_gamma_3)
         )
   
}
