## Changelog:
# MH 0.0.17 2022-01-29: disabled fill_in_asymptotics_interventional_
#                       probabilities (crashes)
# CG 0.0.16 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.15 2022-01-13: added a slot "table containing information accessed
#                       with generic functions
#                       added information for the print function
# MH 0.0.14 2021-11-30: disabled fill_in_asymptotics_interventional_
#                       probabilities, crashes
#                       added call fill_in_interventional_density
#                       added create_causalSEM_s3_object
#                       changed return to causalSEM_object
# MA 0.0.13 2021-11-26, call changed from build_Psi to fill_in_Psi
# CG 0.0.12 2021-11-24: add fill_in_asymptotics functions
# MH 0.0.11 2021-11-22:
#    -- disabled calc_ase_probability (crashes)
#    -- disabled interventional_density, not needed anymore?
#    -- interventional_moments splitted into
#       fill_in_interventional_means / calculate_interventional_means
#       fill_in_interventional_covariance_matrix / 
#       calculate_interventional_covariance_matrix
#    -- call of interventional_probability changed 
#       to fill_in_interventional_probabilities
#    -- call of populate_model_info changed to fill_in_info_model
#    -- call of make_empty_list changed to create_empty_list
# CG 0.0.10 2021-11-22: replaced build_constant_matrix function by
#                       fill_in_constant_matrices
# CG 0.0.9 2021-11-11: replaced add_derivative function by the functions
#                      calculate_jacobian_C and calculate_jacobian_Psi
# MA 0.0.8 2021-10-31: addes calculate_ase, calc_se_proability, and class
# CG 0.0.7 2021-10-28: added @exprt to preamble
# MH 0.0.6 2021-10-25:
#    -- changed call "interventional_moment()" to "interventional_moments()" 
#       (with "s")
#    -- disabled calculate_ase (crashes)
# CG 0.0.5 2021-10-03: add populate intervention_info
#                      add build_constant_matrix
# CG 0.0.4 2021-09-24: add verbose output when start function
#                      added add_derivative function
# CG/MH 0.0.3 2021-09-10: update first run
# CG 0.0.2 2021-09-04: add documentation
# CG 0.0.1 2021-07-20: initial programming

## Documentation
#' @title Calculate Estimates of Causal Effects Based on the Interventional 
#' Distribution.
#' @description Calculate estimates of causal effects defined based on the 
#' interventional distribution.
#' @param model Fitted model. The fitted model can be of class lavaan.
#' @param intervention Character vector. Variable names of interventional 
#' variables. Default: empty vector (no intervention).
#' @param intervention_level Numeric vector. Interventional levels. 
#' Same length and order as argument intervention. Default: vector of ones.
#' @param outcome Character vector. Variable names of outcome variables.
#' Default: all non-interventional variables.
#' @param effect_type Character vector. Parts of the interventional 
#' distribution the user is interested in. Admissible values are 
#' "mean", "variance", "density", and "probability". 
#' Default: "mean". Argument "probability" only admissible if outcome 
#' is univariate.
#' @param lower_bound Single number, numeric. Lower bound of critical range 
#' of univariate outcome variable.
#' @param upper_bound Single number, numeric. Upper bound of critical range 
#' of univariate outcome variable.
#' @param verbose A single number, integer. 0...no output (default), 
#' 1...user messages, 2...debugging-relevant messages.
#' @return An object of class causalSEM, for which several methods 
#' are available, including a summary method.
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords extenal
#' @export

intervention_effect <- function(model, intervention, intervention_level, 
                                outcome = NULL, effect_type = NULL, 
                                lower_bound = NULL, upper_bound = NULL, 
                                verbose = 0, ...){


  # function name
  fun.name <- "intervention_effect"

  # function version
  fun.version <- "0.0.16 2022-01-13"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # TODO
  # check if arguments are well-specified FORMALLY;
  # give warnings, error, etc.
  # later task
  # Check of supported class
  # Check if all variables are "manifest"
  # Check of verbosity argument
  # using (or merging) the handle_verbose_argument function
  # allow object of class causalSEM as input to the
  # intervention_effect function
  # check_arguments(model, intervention, outcome, levels, effect_type,
  # lower_bound, upper_bound, verbose ...)

  # creates empty list
  # internal_list <- make_empty_list( verbose=verbose )
  # MH 0.0.11 2021-11-22, changed to create_empty_list
  internal_list <- create_empty_list( verbose=verbose )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, "
                                  ", Sys.time(), "\n" ) )

  # populate model info
  # fills (some) slots in info_model and fitted_object/class
  # internal_list <- populate_model_info( internal_list = internal_list,
                                        # model = model )
  # MH 0.0.11 2021-11-22, call of populate_model_info 
  # changed to fill_in_info_model
  internal_list <- fill_in_info_model( internal_list = internal_list,
                                               model = model )

  # fills (some) slots in info_intervention
  internal_list <-  fill_in_info_interventions(
    internal_list = internal_list,
    intervention = intervention,
    outcome = outcome,
    intervention_level = intervention_level,
    effect_type = effect_type,
    lower_bound = lower_bound,
    upper_bound = upper_bound)

  # fill in zero one matrices to compute interventional distribution
  # CG 0.0.10 2021-11-22: replaced build_constant_matrix function by
  #                       fill_in_constant_matrices
  internal_list <- fill_in_constant_matrices( internal_list = internal_list )

  # build matrix of structural coefficients
  # MH 0.0.11 2021-11-22, call changed from build_C to fill_in_C
  internal_list <- fill_in_C( internal_list = internal_list )

  # build variance-covariance matrix of error terms
  # MA 0.0.13 2021-11-26, call changed from build_Psi to fill_in_Psi
  internal_list <- fill_in_Psi( internal_list = internal_list )

  # build vector of distinct, functionally independent parameters
  internal_list <- fill_in_theta( internal_list = internal_list )

  # CG 0.0.9 2021-11-11: replaced add_derivative function by the functions
  #                      calculate_jacobian_C and calculate_jacobian_Psi

  # get Jacobian of C with respect to parameters
  internal_list <- calculate_jacobian_C( internal_list = internal_list )

  # get Jacobian of Psi with respect to parameters
  internal_list <- calculate_jacobian_Psi( internal_list = internal_list )

  # Calculates interventional moments
  # MH 0.0.6 2021-10-25: changed call "interventional_moment()" to 
  # "interventional_moments()" (with "s")
  # internal_list <- interventional_moments( internal_list = internal_list )
  # MH 0.0.11 2021-11-22: interventional_moments splitted into
  #   fill_in_interventional_means / calculate_interventional_means
  #   fill_in_interventional_covariance_matrix / 
  #   calculate_interventional_covariance_matrix
  internal_list <- fill_in_interventional_means( internal_list = internal_list )
  internal_list <- fill_in_interventional_covariance_matrix( internal_list = 
                                                             internal_list )

  # calculates interventional density
  # MH 0.0.11 2021-11-22 disabled, not needed anymore?
  # internal_list <- interventional_density( internal_list = internal_list )
  # MH 0.0.14 2021-11-30, call of fill_in_interventional_density
   internal_list <- fill_in_interventional_density( internal_list = 
                                                    internal_list )

  # Calculates interventional probability
  # internal_list <- interventional_probability( internal_list = internal_list )
  # MH 0.0.11 2021-11-22, call changed from interventional_probability to
  #   fill_in_interventional_probabilities
  internal_list <- fill_in_interventional_probabilities( internal_list = 
                                                        internal_list )

  #
  # CG 0.0.12 2021-11-24: add fill_in_asymptotics functions
  internal_list <- fill_in_asymptotics_interventional_means( internal_list = 
                                                             internal_list )
  # MH 0.0.14 2021-11-30, disabled/crashes
  # internal_list <- fill_in_asymptotics_interventional_probabilities( 
  # internal_list = internal_list )
  internal_list <- 
    fill_in_asymptotics_interventional_variances( internal_list = 
                                                  internal_list )

  # Calculate asymptotic standard errors of the interventional mean and 
  # covariance matrix
  # internal_list <- calculate_ase(internal_list = internal_list)

  # Calculate asymptotic standard errors of the interventional
  # MH 0.0.11 2021-11-22 disabled/crashes
  # internal_list <- calc_ase_probability(internal_list = internal_list)
  
  # Implement this when calc_ase_density is done
  #internal_list <- calc_ase_density(internal_list = internal_list)
  
 internal_list <- fill_in_asymptotics_interventional_density( internal_list = 
                                                              internal_list )
  


  # MH 0.0.17 2022-01-29 disabled, crashes with call:
  #    intervention_effect( model=o00_lavaan_test_object,intervention="x2",
  #                         intervention_level=2)
  # error message:
  #   Fehler in stats::dnorm(lower_bounds, mean = outcome_mean,
  #                        sd = outcome_std) : 
  #   Nicht-numerisches Argument für mathematische Funktion  
  # traceback:
  #   4: stats::dnorm(lower_bounds, mean = outcome_mean, sd = outcome_std)
               # at calculate_jacobian_interventional_probabilities.R#133
  #   3: calculate_jacobian_interventional_probabilities(model = internal_list, 
       # x = internal_list$info_interventions$intervention_levels, 
       # intervention_names = internal_list$info_interventions$
	   #                                                   intervention_names, 
       # outcome_names = internal_list$info_interventions$outcome_names, 
       # lower_bounds = internal_list$info_interventions$lower_bounds, 
       # upper_bounds = internal_list$info_interventions$upper_bounds, 
       # verbose = verbose) at fill_in_asymptotics_interventional_
	   #                                                    probabilities.R#46
  #   2: fill_in_asymptotics_interventional_probabilities(internal_list =
       #                            internal_list) at intervention_effect.R#210
  #   1: intervention_effect(model = o00_lavaan_test_object, intervention =
       #                                                                  "x2", 
       # intervention_level = 2)

 # internal_list <- 
   # fill_in_asymptotics_interventional_probabilities( internal_list = 
                                                      # internal_list )
  
  

  # Add a slot containing output for generic functions
  # MA
  internal_list$tables <- fill_in_print_table(internal_list = internal_list)

  # Assign class causalSEM to internal list
  # MH 0.0.14 2021-11-30
  causalSEM_object <- create_causalSEM_s3_object( internal_list )


  # create output
  # internal list
  # MH 0.0.14 2021-11-30, changed return to causalSEM_object
  causalSEM_object

  }


### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "intervention_effect.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

## test object 00_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
# object00 <- intervention_effect( model=o00_lavaan_test_object,intervention="x2",intervention_levels=2)

