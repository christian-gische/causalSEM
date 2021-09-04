## Changelog:
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
  fun.version <- "0.0.2 2021-09-04"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  
  
  # check if arguments are well-specified FORMALLY; 
  # give warnings, error, etc.
  # later task
  # Check of supported class
  # Check if all variables are "manifest"
  # Check of verbosity argument 
  # using (or merging) the verbose_argument_handling function
  # todo: allow object of class causalSEM as input to the 
  # intervention effect function
  
  
check_arguments(model, intervention, outcome, levels, effect.type, 
                lower.bound, upper.bound, verbose ...)  
  

# creates empty list
internal_list <- make_empty_list()
  
# fills model info into list
# todo: fills two slots of internal list: 
# 1. fitted_object
# 2. fitted_object_class


internal_list <- populate_model_info(internal_list = internal_list, 
                                     model = model)
                                     
                                     
  
# fills intervention info into list
internal_list <- populate_intervention_info(internal_list = internal_list, 
                                            intervention = intervention,
                                            outcome = outcome, 
                                            intervention_level = intervention_level,
                                            effect.type = effect.type, 
                                            lower.bound = lower.bound,
                                            upper.bound = upper.bound )


# build matrix of structural coefficients
internal_list <- build_C(internal_list = internal_list) 

# build variance-covariance matrix of error terms
internal_list <- build_Psi(internal_list = internal_list) 

# build vector of distinct, functionally independent parameters 
internal_list <- build_theta(internal_list = internal_list) 



#
# add other functions from the flow chart analogously
#
#

# prepare output 
internal_list <- prepare_output(internal_list = internal_list,)

# assign class to list
class(internal list) <- "causalSEM"

# create output
return(internal_list)
}