## Changelog:
# CG 0.0.9 2021-11-11: replaced add_derivative function by the functions
#                      calculate_jacobian_C and calculate_jacobian_Psi
# MA 0.0.8 2021-10-31: addes calculate_ase, calc_se_proability, and class
# CG 0.0.7 2021-10-28: added @exprt to preamble
# MH 0.0.6 2021-10-25:
#    -- changed call "interventional_moment()" to "interventional_moments()" (with "s")
#    -- disabled calculate_ase (crashes)
# CG 0.0.5 2021-10-03: add populate intervention_info
#                      add build_zero_one_matrix
# CG 0.0.4 2021-09-24: add verbose output when start function
#                      added add_derivative function
# CG/MH 0.0.3 2021-09-10: update first run
# CG 0.0.2 2021-09-04: add documentation
# CG 0.0.1 2021-07-20: initial programming

## Documentation
#' @title Calculate estimates of causal effects defined based on the interventional distribution.
#' @description Calculate estimates of causal effects defined based on the interventional distribution.
#' @param model Fitted model. The fitted model can be of class lavaan.
#' @param intervention Character vector. Variable names of interventional variables. Default: empty vector (no intervention).
#' @param intervention_level Numeric vector. Interventional levels. Same length and order as argument intervention. Default: vector of ones.
#' @param outcome Character vector. Variable names of outcome variables. Default: all non-interventional variables.
#' @param effect.type Character vector. Parts of the interventional distribution the user is interested in. Admissible values are "mean", "variance", "density", and "probability". Default: "mean". Argument "probability" only admissible if outcome is univariate.
#' @param lower.bound Single number, numeric. Lower bound of critical range of univariate outcome variable.
#' @param upper.bound Single number, numeric. Upper bound of critical range of univariate outcome variable.
#' @param verbose A single number, integer. 0...no output (default), 1...user messages, 2...debugging-relevant messages.
#' @return An object of class causalSEM, for which several methods are available, including a summary method.
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords extenal
#' @export

intervention_effect <- function(model, intervention, outcome = NULL, intervention_level,
                                effect.type = NULL , lower.bound = NULL,
                                upper.bound = NULL, verbose = 0, ...){


  # function name
  fun.name <- "intervention_effect"

  # function version
  fun.version <- "0.0.8 2021-10-31"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # TODO
  # check if arguments are well-specified FORMALLY;
  # give warnings, error, etc.
  # later task
  # Check of supported class
  # Check if all variables are "manifest"
  # Check of verbosity argument
  # using (or merging) the verbose_argument_handling function
  # todo: allow object of class causalSEM as input to the
  # intervention effect function
  # check_arguments(model, intervention, outcome, levels, effect.type,
                # lower.bound, upper.bound, verbose ...)

  # creates empty list
  internal_list <- make_empty_list( verbose=verbose )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

  # populate model info
  # fills (some) slots in info_model and fitted_object/class
  internal_list <- populate_model_info( internal_list = internal_list,
                                        model = model )

  # fills (some) slots in info_intervention
  internal_list <-  populate_intervention_info(internal_list = internal_list,
                                               intervention = intervention,
                                               outcome = outcome,
                                               intervention_level = intervention_level,
                                               effect.type = effect.type,
                                               lower.bound = lower.bound,
                                               upper.bound = upper.bound)

  # build zero-one matrices to compute interventional distribution
  internal_list <- build_zero_one_matrix( internal_list = internal_list )

  # build matrix of structural coefficients
  internal_list <- build_C( internal_list = internal_list )

  # build variance-covariance matrix of error terms
  internal_list <- build_Psi( internal_list = internal_list )

  # build vector of distinct, functionally independent parameters
  internal_list <- build_theta( internal_list = internal_list )
  
  # CG 0.0.9 2021-11-11: replaced add_derivative function by the functions
  #                      calculate_jacobian_C and calculate_jacobian_Psi

  # get Jacobian of C with respect to parameters
  internal_list <- calculate_jacobian_C( internal_list = internal_list )
  
  # get Jacobian of Psi with respect to parameters
  internal_list <- calculate_jacobian_Psi( internal_list = internal_list )

  # Calculates interventional moments
  # MH 0.0.6 2021-10-25: changed call "interventional_moment()" to "interventional_moments()" (with "s")
  internal_list <- interventional_moments( internal_list = internal_list )

  # calculates interventional density
  internal_list <- interventional_density( internal_list = internal_list )

  # Calculates interventional probability
  internal_list <- interventional_probability( internal_list = internal_list )

  # Calculate asymptotic standard errors of the interventional mean and covariance matrix
  internal_list <- calculate_ase(internal_list = internal_list)

  # Calculate asymptotic standard errors of the interventional
  internal_list <- calc_ase_probability(internal_list = internal_list)

  # Implement this when calc_ase_density is done
  #internal_list <- calc_ase_density(internal_list = internal_list)

  # Assign class to list
  ## This needs to be discussed
  class(internal_list) <- "causalSEM"


  # create output
  internal_list

  }

