## Changelog:
# CG 0.0.3 2021-11-18: changed name from populate_intervention_info to fill_in_info_interventions
# CG 0.0.2 2021-10-28: added comments / todos before lower.bounds
# CG 0.0.1 2021-10-01: initial programming

## Documentation
#' @title Extracts interventional information from the arguments of the intervention_effect function. 
#' @description Internal function that Extracts interventional information from the arguments 
#'    of the intervention_effect function.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{fill_in_info_interventions} returns the inputted internal_list with slots
#'    \code{n_intervention}, \code{intervention_name}, \code{intervention_level},
#'    \code{effect_type}, \code{n_outcome}, \code{outcome_name},  
#'    \code{lower_bound}, and \code{upper_bound} populated.
#' @seealso \code{\link{populate_model_info}}
#' @references
#' Gische, C. & Voelkle, M. C. (2022). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. Psychometrika.
#' @keywords internal

## Function definition
fill_in_info_interventions <- function(internal_list, intervention, outcome, intervention_level,
                                       effect.type, lower.bound, upper.bound){
  
  # function name
  fun.name <- "fill_in_info_interventions"
  
  # function version
  fun.version <- "0.0.3 2021-11-18"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # get variable names of interventional variables
  if( is.character( intervention ) && all( intervention %in% internal_list$info_model$var_names ) ){
    # set in internal_list
    internal_list$info_interventions$intervention_name <- intervention
  } else {
    stop( paste0( fun.name.version, ": setting intervention_name in internal list failed. Argument intervention needs to be a character vector of variable names.")  )
  }
  
  # get number of interventional variables 
  # set in internal_list
  internal_list$info_interventions$n_intervention <- length( internal_list$info_interventions$intervention_name )
  
  # get numeric values of interventional levels
  if( is.numeric( intervention_level ) && length ( intervention_level ) == length( internal_list$info_interventions$intervention_name )){
    # set in internal_list
    internal_list$info_interventions$intervention_level <- intervention_level
  } else if ( is.null( intervention_level ) ){
    internal_list$info_interventions$intervention_level <- rep( 1 , internal_list$info_interventions$n_intervention )
  } else {
    stop( paste0( fun.name.version, ": setting intervention_level in internal list failed. Argument intervention_level needs to be a numeric vector of same length as intervention."  ) )
  }
  
  # get effect type from input of intervention_effect function 
  
  supported_effect_types <- c( "mean" , "variance" , "density" , "probability" )
  
  if( is.character( effect.type ) && all( effect.type %in% supported_effect_types ) ){
    # set in internal_list
    internal_list$info_interventions$effect_type <- effect.type
  } else if ( is.null( effect.type ) ){
    internal_list$info_interventions$effect_type <- c("mean")
  } else {
    stop( paste0( fun.name.version, ": setting effect_type in internal list failed. Argument effect.type needs to be a character vector with entries 'mean', 'variance', 'density', or 'probability'." )  )
  }
  
  
  # get variable names of outcome variables
  if( is.character( outcome ) && all( outcome %in% internal_list$info_model$var_names ) ){
    # set in internal_list
    internal_list$info_interventions$outcome_name <- outcome
  } else if ( is.null( outcome ) ){
    internal_list$info_interventions$outcome_name <- setdiff(internal_list$info_model$var_names, internal_list$info_interventions$intervention_name)
  } else {
    stop( paste0( fun.name.version, ": setting outcome_name in internal list failed. Argument outcome needs to be a character vector of variable names."  ) )
  }
  
  # get number of interventional variables 
  # set in internal_list
  internal_list$info_interventions$n_outcome <- length(internal_list$info_interventions$outcome_name)
  
  # get lower bound of outcome range 
  if( is.numeric( lower.bound ) && length ( lower.bound ) == 1 && internal_list$info_interventions$n_outcome == 1 ){
    # set in internal_list
    internal_list$info_interventions$lower_bound <- lower.bound 
  } else if ( is.null( lower.bound ) ){
    internal_list$info_interventions$lower_bound <- NULL 
  } else {
    stop( paste0( fun.name.version, ": setting lower_bound in internal list failed. Argument lower_bound needs to be a numeric scalar."  ) )
  }
  
  
  # get lower bound of outcome range 
  # todo: option to provide same number of upper and lower bounds as 
  # verbose: provide lower and upper bound in the same order as outcome variable or
  # as named vector; if no outcome variable is provided as argument force user to name
  # upper and lower bound which have to be the same dimension as vector of non interventional
  # variables 
  # internally: always name upper and lower bound in internal list and bring in same order 
  # as outcome names
  # outcome variables of interest 
  # caution: muliple upper bounds need to be in the same order as multivariate outcome variable
  # CAUTION: order of upper bounds in case outcome variable is not user specified 
  
  if( is.numeric(upper.bound ) && length ( upper.bound ) == 1 && internal_list$info_interventions$n_outcome == 1 ){
    # set in internal_list
    internal_list$info_interventions$upper_bound <- upper.bound
  } else if ( is.null( upper.bound ) ){
    internal_list$info_interventions$upper_bound <- NULL 
  } else {
    stop( paste0( fun.name.version, ": setting upper_bound in internal list failed. Argument lower_bound needs to be a numeric scalar."  ) )
  }
  
  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # return internal list
  return( internal_list )
}

## test/development
#source( "C:/Users/Christian/Dropbox/causalSEM_R_Package/causalSEM/R/verbose_argument_handling.R" )
#source( "C:/Users/Christian/Dropbox/causalSEM_R_Package/causalSEM/R/make_empty_list.R" )
#source( "C:/Users/Christian/Dropbox/causalSEM_R_Package/causalSEM/R/populate_model_info.R" )

#load( "C:/Users/Christian/Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" )
#load( "C:/Users/Christian/Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" )
#load( "C:/Users/Christian/Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" )
#load( "C:/Users/Christian/Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" )


#internal_list <- make_empty_list(verbose = 2)
#internal_list <- populate_model_info(internal_list, o00_lavaan_test_object)
#populate_intervention_info(internal_list, c("x2"), c("y3"), 11.48,
#                           c("mean"), -40, 80)

#internal_list <- make_empty_list(verbose = 2)
#internal_list <- populate_model_info(internal_list, o01_lavaan_test_object)
#populate_intervention_info(internal_list, c("x2"), c("y3"), 11.48,
#                           c("mean"), -40, 80)

#internal_list <- make_empty_list(verbose = 2)
#internal_list <- populate_model_info(internal_list, o02_lavaan_test_object)
#populate_intervention_info(internal_list, c("x2"), c("y3"), 11.48,
#                           c("mean"), -40, 80)

#internal_list <- make_empty_list(verbose = 2)
#internal_list <- populate_model_info(internal_list, o03_lavaan_test_object)
#populate_intervention_info(internal_list, c("x2"), c("y3"), 11.48,
#                           c("mean"), -40, 80)