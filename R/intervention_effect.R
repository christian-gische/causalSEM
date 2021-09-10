## Changelog:
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

intervention_effect <- function(model, intervention, outcome = NULL, intervention_level,
                                effect.type = NULL , lower.bound = NULL,
                                upper.bound = NULL, verbose = 0, ...){


  # function name
  fun.name <- "intervention_effect"
  
  # function version
  fun.version <- "0.0.3 2021-09-10"
  
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

  # populate model info
  # fills (some) slots in info_model and fitted_object/class
  internal_list <- populate_model_info( internal_list = internal_list, 
                                        model = model )

  # build matrix of structural coefficients
  internal_list <- build_C( internal_list = internal_list )
  
  # build variance-covariance matrix of error terms
  internal_list <- build_Psi( internal_list = internal_list )
  
  # build vector of distinct, functionally independent parameters 
  internal_list <- build_theta( internal_list = internal_list )


# TODO fills intervention info into list
# internal_list <- populate_intervention_info(internal_list = internal_list, 
                                            # intervention = intervention,
                                            # outcome = outcome, 
                                            # intervention_level = intervention_level,
                                            # effect.type = effect.type, 
                                            # lower.bound = lower.bound,
                                            # upper.bound = upper.bound )

#
# add other functions from the flow chart analogously
#
#

# prepare output 
# internal_list <- prepare_output(internal_list = internal_list,)

# assign class to list
# class(internal_list) <- "causalSEM"

  # create output
  return(internal_list)
}

# development
# source( "verbose_argument_handling.R" )
# source( "make_empty_list.R" )
# source( "populate_model_info.R" )
# source( "lav_parTable_fill_labels.R" )
# source( "build_C.R" )
# source( "build_Psi.R" )
# source( "build_theta.R" )

# ( load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) ) )

# internal_list <- intervention_effect( model=o00_lavaan_test_object,
                     # intervention=NULL,
					 # intervention_level=NULL)

