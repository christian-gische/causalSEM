## Changelog:
# CG 0.0.6 2023-02-21: changes in preamble and comments
# CG 0.0.5 2022-03-08: changed argument check for upper and lower bound
# CG 0.0.4 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# CG 0.0.3 2021-11-18: changed name from populate_intervention_info to
#                      fill_in_info_interventions
# CG 0.0.2 2021-10-28: added comments / todos before lower_boundss
# CG 0.0.1 2021-10-01: initial programming

## Documentation
#' @title Process Arguments of the \code{intervention_effect}() Function
#' @description Extract information about the intervention from the arguments
#'  of the \code{intervention_effect}() function and check if they are 
#'  admissible.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted list with the following slots filled in:
#' \tabular{ll}{
#' List of 7 \tab \cr
#' \tab   \code{n_intervention} \cr
#' \tab   \code{intervention_names} \cr
#' \tab   \code{intervention_levels} \cr
#' \tab   \code{effect_type} \cr
#' \tab   \code{n_outcome} \cr
#' \tab   \code{outcome_names} \cr
#' \tab   \code{lower_bounds}\cr
#' \tab   \code{upper_bounds}}
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_info_interventions <- function(internal_list = NULL,
                                       intervention = NULL,
                                       outcome = NULL,
                                       intervention_level = NULL,
                                       effect_type = NULL,
                                       lower_bound = NULL,
                                       upper_bound = NULL){

  # function name
  fun.name <- "fill_in_info_interventions"

  # function version
  fun.version <- "0.0.6 2023-02-21"

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
    stop( paste0( fun.name.version, ": Setting outcome_names in internal list
                  failed. Argument outcome needs to be a character vector of
                  variable names of variables that are not subject to 
                  intervention."  ) )
  }

  # get number of interventional variables
  # set in internal_list
  internal_list$info_interventions$n_outcome <-
    length(internal_list$info_interventions$outcome_names)

  # get lower bound of outcome range
  # CG 0.0.5: allow lower bounds to be multivariate
  if( is.numeric( lower_bound ) &&
      length ( lower_bound ) == internal_list$info_interventions$n_outcome)
    {
    # set in internal_list
    internal_list$info_interventions$lower_bounds <- lower_bound
  } else if ( is.null( lower_bound ) ){
    internal_list$info_interventions$lower_bounds <- NULL
  } else {
    stop( paste0( fun.name.version, ": setting lower_bound in internal list
                  failed. Argument lower_bound needs to be numeric and of
                  same length as the argument outcome."  ) )
  }

# get upper bounds of outcome range
# CG 0.0.5: allow upper bounds to be multivariate

  if( is.numeric(upper_bound ) &&
      length ( upper_bound ) == internal_list$info_interventions$n_outcome){
    # set in internal_list
    internal_list$info_interventions$upper_bounds <- upper_bound
  } else if ( is.null( upper_bound ) ){
    internal_list$info_interventions$upper_bounds <- NULL
  } else {
    stop( paste0( fun.name.version, ": setting upper_bound in internal list
                  failed. Argument upper_bound needs to be numeric and of
                  same length as the argument outcome."  ) )
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
