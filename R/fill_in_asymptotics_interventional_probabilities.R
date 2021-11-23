## Changelog:
# MA 0.0.1 2021-11-22: initial programming

## Documentation
#' @title Function to fill in asymptotic quantities of the interventional probabilities
#' @description Internal function that fills in the jacobian, asymptotic
#' covariance matrix, asymptotic standard errors, and asymptotic z-values of the
#' interventional probabilities into the internal list.
#' @param internal_list A list with various information extracted from the model.
#' @return \code{fill_in_asymptotics_interventional_probabilities} returns the inputted
#' internal_list with several slots in ..$interventional_distribution filled.
#'    ..$jacobian
#'    ..$acov
#'    ..$ase
#'    ..$z_value
#' @seealso \code{\link{}}
#' @references
#' Gische, C. & Voelkle, M. C. (2021). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. Psychometrika.
#' @keywords internal

## Function definition
fill_in_asymptotics_interventional_probabilities <- function(internal_list){

  # function name
  fun_name <- "fill_in_asymptotics_interventional_probabilities"

  # function version
  fun_version <- "0.0.1 2021-11-22"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ", Sys.time(), "\n" ))

  # calculate jacobian
  jacobian <- calculate_jacobian_interventional_probabilities(
    model = internal_list,
    x = internal_list$info_interventions$intervention_level,
    intervention_names = internal_list$info_interventions$intervention_name,
    outcome_names = internal_list$info_interventions$outcome_name,
    lower_bound = internal_list$info_interventions$lower_bound,
    upper_bound = internal_list$info_interventions$upper_bound,
    verbose = verbose
  )

  # calculate asym
  ase <- calculate_ase_interventional_probabilities(
    model = internal_list,
    x = internal_list$info_interventions$intervention_level,
    intervention_names = internal_list$info_interventions$intervention_name,
    outcome_names = internal_list$info_interventions$outcome_name,
    lower_bound = internal_list$info_interventions$lower_bound,
    upper_bound = internal_list$info_interventions$upper_bound,
    verbose = verbose
  )

  # fill in slots of ...$interventional_distribution
  internal_list$interventional_distribution$jacobian$probabilities <- jacobian
  internal_list$interventional_distribution$acov$probabilities <- ase$acov_gamma_4
  internal_list$interventional_distribution$ase$probabilities <- ase$ase_gamma_4
  internal_list$interventional_distribution$z_values$probabilities <- ase$z_gamma_4

  # console output
  if(verbose >= 2) cat( paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n" ))

  # return internal list
  internal_list

}
