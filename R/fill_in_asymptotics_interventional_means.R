## Changelog:
# CG 0.0.4 2023-02-28: check if argument is of class causalSEM
# MH 0.0.3 2022-03-17: removed "seealso", solves NOTE in package checking
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.1 2021-11-22: initial programming

## Documentation
#' @title Fill in Asymptotics for the Mean to List
#' @description Fills in the Jacobian, the asymptotic
#' covariance matrix, the asymptotic standard errors, and asymptotic z-values 
#' of the mean vector of the interventional distribution into the internal list.
#' See, for example, Theorem 8 and Corollary 11 in Gische and Voelkle (2022).
#' @param internal_list A list with various information extracted from the 
#' model.
#' @return The inputted list with several slots 
#' in \code{..$interventional_distribution$means} filled in.\cr
#' \tabular{lll}{
#' \tab   \code{..$jacobian} \tab The Jacobian matrix. \cr
#' \tab   \code{..$acov} \tab The asymptotic covariance matrix. \cr
#' \tab   \code{..$ase} \tab Asymptotic standard errors. \cr
#' \tab   \code{..$z_value} \tab Asymptotic z-values.} 
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_asymptotics_interventional_means <- function(internal_list = NULL){

  # function name
  fun_name <- "fill_in_asymptotics_interventional_means"

  # function version
  fun_version <- "0.0.4 2023-02-28"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")
  
  # CG 0.0.3 2023-02-28: check if argument is of class causalSEM 
  # check function arguments 
  ## get class of model object
  model_class <- class(internal_list)
  
  ## set supported classes of model objects
  supported_model_classes <- c( "causalSEM" )
  
  ## check if argument model is supported
  if(!any(model_class %in% supported_model_classes)) stop(
    paste0(
      fun.name.version, ": model of class ", model_class,
      " not supported. Supported fit objects are: ",
      paste(supported_model_classes, collapse = ", ")
    )
  )

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

  # fill in slots of ...$interventional_distribution$means
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
