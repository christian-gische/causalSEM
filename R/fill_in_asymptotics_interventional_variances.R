## Changelog:
# MA 0.0.1 2021-11-22: initial programming

## Documentation
#' @title Function to fill in asymptotic quantities of the interventional variances
#' @description Internal function that fills in the jacobian, asymptotic
#' covariance matrix, asymptotic standard errors, and asymptotic z-values of the
#' interventional variances into the internal list.
#' @param internal_list A list with various information extracted from the model.
#' @return \code{fill_in_asymptotics_interventional_variances} returns the inputted
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
fill_in_asymptotics_interventional_variances <- function(internal_list){

  # function name
  fun_name <- "fill_in_asymptotics_interventional_variances"

  # function version
  fun_version <- "0.0.1 2021-11-22"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ", Sys.time(), "\n" ))

  # calculate jacobian
  jacobian <- calculate_jacobian_interventional_variances(
    model = internal_list,
    intervention_names = internal_list$info_interventions$intervention_name,
    outcome_names = internal_list$info_interventions$outcome_name,
    verbose = verbose
  )

  # calculate asym
  ase <- calculate_ase_interventional_variances(
    model = internal_list,
    x = internal_list$info_interventions$intervention_level,
    intervention_names = internal_list$info_interventions$intervention_name,
    outcome_names = internal_list$info_interventions$outcome_name,
    verbose = verbose
  )

  # fill in slots of ...$interventional_distribution
  internal_list$interventional_distribution$jacobian$variances <- jacobian
  internal_list$interventional_distribution$acov$variances <- ase$acov_gamma_2
  internal_list$interventional_distribution$ase$variances <- ase$ase_gamma_2
  internal_list$interventional_distribution$z_values$variances <- ase$z_gamma_2

  # console output
  if(verbose >= 2) cat( paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n" ))

  # return internal list
  internal_list

}
