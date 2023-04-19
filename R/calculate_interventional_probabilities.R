## Changelog:
# CG 0.0.7 2023-04-19: allow for arguments model and use_model_values
#                      include if statements to check which arguments to use
#                      check if argument model is of admissible class
# CG 0.0.6 2022-03-08: changed argument check for upper and lower bound
# CG 0.0.5 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MH 0.0.3 2021-11-22: renamed from calc_interventional_probability to 
# calculate_interventional_probabilities
# MH 0.0.2 2021-10-14: added loop over values
# MH 0.0.1 2021-09-27: initial programming

## Documentation
#' @title Calculate Probabilities of Interventional Events
#' @description Calculates probabilities of interventional events, for example,
#' the probability that the outcome variable realizes within a critical range of
#' values given an intervention on the exposure. See, for example, Eqs. 10, 15, 
#' and 22d in Gische and Voelkle (2022).
#' @param mean Numeric vector of means.
#' @param sd Numeric vector of standard deviations.
#' @param y_low Numeric vector of lower bounds.
#' @param y_up Numeric vector of upper bounds.
#' @param verbose Integer number setting verbosity of console outputs.
#' @return List of numeric vectors of probabilities interventional events.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
calculate_interventional_probabilities <- function(mean = NULL,
                                                   sd = NULL,
                                                   y_low = NULL,
                                                   y_up = NULL,
                                                   model = NULL,
                                                   use_model_values = FALSE,
                                                   verbose = NULL){

	# function name
	fun.name <- "calculate_interventional_probabilities"

	# function version
	fun.version <- "0.0.7 2023-04-19"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
	
	# if model is provided, check if it of admissible class 
	
	if ( !is.null(model)){
	  # get class of model object
	  model_class <- class(model)
	  
	  # set supported classes of model objects
	  supported_model_classes <- c( "causalSEM" )
	  
	  # check if argument model is supported
	  if(!any(model_class %in% supported_model_classes)) stop(
	    paste0(
	      fun.name.version, ": model of class ", model_class,
	      " not supported. Supported fit objects are: ",
	      paste(supported_model_classes, collapse = ", ")
	    )
	  )
	  
	  verbose <- model$control$verbose
	  
	}
	
	
	
	if (!is.null(model) && use_model_values == TRUE){
	  
	  # get upper lower bound
	  y_low <- model$info_interventions$lower_bounds
	  y_up <- model$info_interventions$upper_bounds
	  
	  # if y_low/y_up NULL then return NULL
	  if( any( is.null( c( y_low, y_up ) ) ) ){
	    p <- NULL
	  } else {
	    # get intervential mean and variance
	    E <- model$interventional_distribution$means$values
	    V <- model$interventional_distribution$covariance_matrix$values
	    
	    # standard deviations (sqrt of diagonal elements of V)
	    sds <- sqrt( diag( V ) )
	    
	    # select only outcome variables
	    outcomes <- model$info_interventions$outcome_names
	    means <- E[outcomes, 1]
	    sds <- sds[outcomes]
	    }
	  } else if (use_model_values == FALSE) {
	  
	  # TODO: include argument check for mean and sd and maybe elaborate 
	  # argument check for y_low and y_up
	  
	  verbose <- handle_verbose_argument(verbose)
	  
	  # CG 0.0.6 2022-03-08: argument check for upper and lower bound
	  
	  if( length(y_low) == length(y_up) && length(y_low) == length(sd) ){
	    
	    y_low <- y_low
	    y_up <- y_up
	    mean <- mean
	    sd <- sd 
	    
	    } else {
	    stop( paste0( fun.name.version, ": calculation of interventional 
	  probabilities failed. Arguments y_low, y_up, sd, and mean need to be of 
	  same length.") )
	  } 
	  
	}

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", 
	                                Sys.time(), "\n" ) )

	  # Eq. 22d, p. 22 in Gische and Voelkle (2022)
	  p <- mapply( function( mean, sd, y_low, y_up ) { 
	    stats::pnorm( ( y_up - mean ) / sd ) - 
	      stats::pnorm( ( y_low - mean ) / sd ) }, 
	    mean, sd, y_low, y_up, SIMPLIFY = TRUE )
	   
	  
	# return p
	return( p )
	
}
