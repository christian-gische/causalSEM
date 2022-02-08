## Changelog:
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.1 2021-11-22: initial programming

## Documentation
#' @title Fill in Asymptotics for the Mean Vector of the Interventional
#'  Distribution
#' @description Fills in the Jacobian, the asymptotic
#' covariance matrix, the asymptotic standard errors, and asymptotic z-values 
#' of the mean vector of the interventional distribution into the internal list.
#' @param internal_list A list with various information extracted from the 
#' model.
#' @return The inputted internal_list with several slots 
#' in ..$interventional_distribution filled in.
#'    ..$jacobian
#'    ..$acov
#'    ..$ase
#'    ..$z_value
#' @seealso \code{\link{}}
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_asymptotics_interventional_means <- function(internal_list){

  # function name
  fun_name <- "fill_in_asymptotics_interventional_means"

  # function version
  fun_version <- "0.0.2 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ",
                               Sys.time(), "\n" ))

  # calculate jacobian
  jacobian <- calculate_jacobian_interventional_means(
    model = internal_list,
    x = internal_list$info_interventions$intervention_levels,
    intervention_names = internal_list$info_interventions$intervention_names,
    outcome_names = internal_list$info_interventions$outcome_names,
    verbose = verbose
  )

  # calculate asymptotic standard errors
  ase <- calculate_ase_interventional_means(
    model = internal_list,
    x = internal_list$info_interventions$intervention_levels,
    intervention_names = internal_list$info_interventions$intervention_names,
    outcome_names = internal_list$info_interventions$outcome_names,
    verbose = verbose
  )

  # fill in slots of ...$interventional_distribution
  internal_list$interventional_distribution$means$jacobian <- jacobian
  internal_list$interventional_distribution$means$acov <- ase$acov_gamma_1
  internal_list$interventional_distribution$means$ase <- ase$ase_gamma_1
  internal_list$interventional_distribution$means$z_values <- ase$z_gamma_1

  # console output
  if(verbose >= 2) cat( paste0("  end of function ", fun_name_version, " ",
                               Sys.time(), "\n" ))

  # return internal list
  internal_list

}
