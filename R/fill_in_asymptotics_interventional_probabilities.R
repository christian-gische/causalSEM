## Changelog:
# CG 0.0.3 2022-03-08:  allow for multivariate lower and upper bounds
# CG 0.0.2 2022-01-13:  changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.1 2021-11-22:  initial programming

## Documentation
#' @title Fill in Asymptotic Quantities of Interventional Probabilities to 
#' Internal List
#' @description Fills in the Jacobian, the asymptotic covariance matrix, 
#' the asymptotic standard errors, and asymptotic z-values of the
#' interventional probabilities into the internal list.
#' @param internal_list A list with various information extracted from the 
#' model.
#' @return The inputted internal_list with several slots in 
#' ..$interventional_distribution$probabilities filled.
#'    ..$jacobian
#'    ..$ase
#'    ..$z_values
#' @seealso \code{\link{}}
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_asymptotics_interventional_probabilities <- function(internal_list){

  # function name
  fun_name <- "fill_in_asymptotics_interventional_probabilities"

  # function version
  fun_version <- "0.0.3 2022-03-08"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ", 
                               Sys.time(), "\n" ))

  
  # get upper lower bound
  y_low <- internal_list$info_interventions$lower_bounds
  y_up <- internal_list$info_interventions$upper_bounds
  
  # CG 0.0.3 2022-03-08:  allow for multivariate lower and upper bounds
  if( any( is.null( c( y_low, y_up ) ) ) ){
    
    internal_list$interventional_distribution$probabilities$jacobian <- NULL
    internal_list$interventional_distribution$probabilities$acov <- NULL
    internal_list$interventional_distribution$probabilities$ase <- NULL
    internal_list$interventional_distribution$probabilities$z_values <- NULL
      
  } else {
  
  # calculate jacobian
  jacobian <- calculate_jacobian_interventional_probabilities(
    model = internal_list,
    x = internal_list$info_interventions$intervention_levels,
    intervention_names = internal_list$info_interventions$intervention_names,
    outcome_names = internal_list$info_interventions$outcome_names,
    lower_bounds = internal_list$info_interventions$lower_bounds,
    upper_bounds = internal_list$info_interventions$upper_bounds,
    verbose = verbose
  )

  # calculate asymptotics
  ase <- calculate_ase_interventional_probabilities(
    model = internal_list,
    x = internal_list$info_interventions$intervention_levels,
    intervention_names = internal_list$info_interventions$intervention_names,
    outcome_names = internal_list$info_interventions$outcome_names,
    lower_bounds = internal_list$info_interventions$lower_bounds,
    upper_bounds = internal_list$info_interventions$upper_bounds,
    verbose = verbose
  )

  # fill in slots of ...$interventional_distribution
  # CG 0.0.2: changed names of slots to be filled in to
  # $interventional_distribution$probabilities
  #'    ..$jacobian
  #'    ..$acov
  #'    ..$ase
  #'    ..$z_values
  internal_list$interventional_distribution$probabilities$jacobian <- jacobian
  internal_list$interventional_distribution$probabilities$acov <- 
   ase$acov_gamma_4
  internal_list$interventional_distribution$probabilities$ase <- ase$ase_gamma_4
  internal_list$interventional_distribution$probabilities$z_values <- 
   ase$z_gamma_4
  
  }
  
  # console output
  if(verbose >= 2) cat( paste0("  end of function ", fun_name_version, " ", 
                               Sys.time(), "\n" ))
  
  # return internal list
  internal_list
}
