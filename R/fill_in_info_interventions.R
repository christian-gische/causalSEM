## Changelog:
# CG 0.0.4 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# CG 0.0.3 2021-11-18: changed name from populate_intervention_info to 
#                      fill_in_info_interventions
# CG 0.0.2 2021-10-28: added comments / todos before lower_boundss
# CG 0.0.1 2021-10-01: initial programming

## Documentation
#' @title Extracts Interventional Information From the Arguments of the 
#' intervention_effect() function. 
#' @description Extract interventional information from the arguments 
#'    of the intervention_effect() function.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return Returns the inputted internal_list with slots
#'    \code{n_intervention}, \code{intervention_names},
#'    \code{intervention_levels},
#'    \code{effect_type}, \code{n_outcome}, \code{outcome_names},  
#'    \code{lower_bounds}, and \code{upper_bounds} filled in.
#' @seealso \code{\link{populate_model_info}}
#' @references
#' Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible framework for
#' studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_info_interventions <- function(internal_list, 
                                       intervention, 
                                       outcome, 
                                       intervention_level,
                                       effect_type, 
                                       lower_bound, 
                                       upper_bound){
  
  # function name
  fun.name <- "fill_in_info_interventions"
  
  # function version
  fun.version <- "0.0.4 2022-01-13"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  
  # get variable names of interventional variables
  if( is.character( intervention ) && 
      all( intervention %in% internal_list$info_model$var_names ) ){
    # set in internal_list
    internal_list$info_interventions$intervention_names <- intervention
  } else {
    stop( paste0( fun.name.version, ": setting intervention_names in internal 
                  list failed. Argument intervention needs to be a character 
                  vector of variable names.")  )
  }
  
  # get number of interventional variables 
  # set in internal_list
  internal_list$info_interventions$n_intervention <- 
    length( internal_list$info_interventions$intervention_names )
  
  # get numeric values of interventional levels
  if( is.numeric( intervention_level ) && 
      length ( intervention_level ) == 
      length( internal_list$info_interventions$intervention_names )){
    # set in internal_list
    internal_list$info_interventions$intervention_levels <- intervention_level
  } else if ( is.null( intervention_level ) ){
    internal_list$info_interventions$intervention_levels <- 
      rep( 1 , internal_list$info_interventions$n_intervention )
  } else {
    stop( paste0( fun.name.version, ": setting intervention_level in internal 
                  list failed. Argument intervention_level needs to be a 
                  numeric vector of same length as intervention."  ) )
  }
  
  # get effect type from input of intervention_effect function 
  
  supported_effect_types <- c( "mean" , "variance" , "density" , "probability" )
  
  if( is.character( effect_type ) && 
      all( effect_type %in% supported_effect_types ) ){
    # set in internal_list
    internal_list$info_interventions$effect_type <- effect_type
  } else if ( is.null( effect_type ) ){
    internal_list$info_interventions$effect_type <- c("mean")
  } else {
    stop( paste0( fun.name.version, ": setting effect_type in internal list
                  failed. Argument effect_type needs to be a character vector 
                  with entries 'mean', 'variance', 'density', 
                  or 'probability'." )  )
  }
  
  
  # get variable names of outcome variables
  if( is.character( outcome ) && 
      all( outcome %in% internal_list$info_model$var_names ) ){
    # set in internal_list
    internal_list$info_interventions$outcome_names <- outcome
  } else if ( is.null( outcome ) ){
    internal_list$info_interventions$outcome_names <- 
      setdiff(internal_list$info_model$var_names, 
              internal_list$info_interventions$intervention_names)
  } else {
    stop( paste0( fun.name.version, ": setting outcome_names in internal list 
                  failed. Argument outcome needs to be a character vector of 
                  variable names."  ) )
  }
  
  # get number of interventional variables 
  # set in internal_list
  internal_list$info_interventions$n_outcome <- 
    length(internal_list$info_interventions$outcome_names)
  
  # get lower bound of outcome range 
  # TO DO: allow lower bounds to be multivariate
  if( is.numeric( lower_bound ) && 
      length ( lower_bound ) == 1 && 
      internal_list$info_interventions$n_outcome == 1 ){
    # set in internal_list
    internal_list$info_interventions$lower_bounds <- lower_bound 
  } else if ( is.null( lower_bound ) ){
    internal_list$info_interventions$lower_bounds <- NULL 
  } else {
    stop( paste0( fun.name.version, ": setting lower_bound in internal list 
                  failed. Argument lower_bound needs to be a numeric 
                  scalar."  ) )
  }
  
  
  # TODO: option to provide same number of upper and lower bounds as 
  # verbose: provide lower and upper bound in the same order as outcome variable
  # or as named vector; if no outcome variable is provided as argument force
  # user to name upper and lower bound which have to be the same dimension as 
  # vector of non interventional variables 
  # internally: always name upper and lower bound in internal list and bring in
  # same order as outcome names outcome variables of interest 
  # caution: muliple upper bounds need to be in the same order as multivariate 
  # outcome variable
  # CAUTION: order of upper bounds in case outcome variable is not user 
  # specified 
  
# get upper bounds of outcome range 
# TO DO: allow lower bounds to be multivariate
  
  if( is.numeric(upper_bound ) &&
      length ( upper_bound ) == 1 && 
      internal_list$info_interventions$n_outcome == 1 ){
    # set in internal_list
    internal_list$info_interventions$upper_bounds <- upper_bound
  } else if ( is.null( upper_bound ) ){
    internal_list$info_interventions$upper_bounds <- NULL 
  } else {
    stop( paste0( fun.name.version, ": setting upper_bound in internal list 
                  failed. Argument lower_bound needs to be a numeric 
                  scalar."  ) )
  }
  
  # console output
  if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  
  # return internal list
  return( internal_list )
}

## test/development
#source( "C:/Users/Christian/Dropbox/causalSEM_R_Package/causalSEM/R/handle_verbose_argument.R" )
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