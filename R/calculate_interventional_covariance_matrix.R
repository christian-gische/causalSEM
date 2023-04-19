## Changelog:
# CG 0.0.5 2023-04-19: allow for arguments model and use_model_values
#                      include if statements to check which arguments to use
#                      check if argument model is of admissible class
# CG 0.0.4 2023-02-28: change preamble for documentation
# MA 0.0.3 2022-02-07: Added Psi to @param
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title Calculate Covariance Matrix of the Interventional Distribution
#' @description Calculate Covariance Matrix of the Interventional Distribution.
#' (see, for example, Eq. 6b in Gische and Voelkle, 2022). The arguments of 
#' the \code{\link{calculate_interventional_covariance_matrix}} function are 
#' described in Definition 1 in Gische and Voelkle (2022).
#' @param C Numeric matrix of structural coefficients.
#' @param Psi Numeric covariance matrix (Psi-matrix).
#' @param x Numeric vector of interventional levels.
#' @param SI Numeric selection matrix.
#' @param n Integer number of observed variables.
#' @param IN Numeric zero-one matrix.
#' @param verbose Integer number describing the verbosity of console output.
#' Admissible values: 0: no output (default), 1: user messages, 
#' 2: debugging-relevant messages.
#' @return The covariance matrix of the interventional distribution.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
calculate_interventional_covariance_matrix <- 
  function(C = NULL,
           Psi= NULL,
           x = NULL,
           SI = NULL,
           n = NULL,
           IN = NULL,
           verbose = NULL,
           model = NULL,
           use_model_values = FALSE){

	# function name
	fun.name <- "calculate_interventional_covariance_matrix"

	# function version
	fun.version <- "0.0.5 2023-04-19"

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
	  
	  # get/define terms
	  C <- model$info_model$C$values
	  
	  # interventional level
	  x <- model$info_interventions$intervention_levels
	  
	  # covariance matrix
	  
	  Psi <- model$info_model$Psi$values
	  
	  # selection matrix 1_I
	  SI <- model$constant_matrices$select_intervention
	  
	  # number of observed variables
	  n <- model$info_model$n_ov
	  
	  # I_N matrix
	  IN <- model$constant_matrices$eliminate_intervention	
	  
	} else if (use_model_values == FALSE) {
	  
	  # TODO: include argument check
	  
	  verbose <- handle_verbose_argument(verbose)
	  
	  # get/define terms
	  C <- C
	  
	  # interventional level
	  x <- x
	  
	  # covariance matrix
	  Psi <- Psi
	  
	  # selection matrix 1_I
	  SI <- SI
	  
	  # number of observed variables
	  n <- n
	  
	  # I_N matrix
	  IN <- IN
	  
	}
	
	
	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# identity matrix
	In <- diag( n )

	# calculate interventional covariance matrix, Eq. 6b in Gische and
	# Voelkle (2022)
	V <-
	  solve( In - IN %*% C ) %*% IN %*% Psi %*% IN %*% t( solve( In - IN %*% C ) )

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( V )
}
