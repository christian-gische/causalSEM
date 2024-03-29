## Changelog:
# CG 0.0.4 2023-04-19: inlcude the n_grid argument
#                      remove check of var_names towards the end of the script
# CG 0.0.3 2023-04-19: allow for arguments model, use_model_values, 
#                      include if statements to check which arguments to use
#                      check if argument model is of admissible class
# CG 0.0.2 2022-01-13: function now computes the values of the pdf 
#                      over a grid of -3*SD ; +3*SD
#                      changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# MH 0.0.1 2021-11-30: chunked from interventional_density() 0.0.5 2021-11-24

## Documentation
#' @title Calculate PDF of Interventional Distribution
#' @description Calculate the probability density function (pdf) of the 
#' interventional distribution. The values are computed pointwise over a grid of
#' values for each univariate variable separately (i.e., the pdfs of the 
#'  marginal distributions). See, for example, Eqs. 9, 14, and 22c in Gische and
#'   Voelkle (2022).
#' @param E Numeric vector of mean values.
#' @param V Numeric vector of variances.
#' @param var_names Character vector of variable names.
#' @param n_grid Integer number indicating the number of values (in the grid) 
#' for which the pdf should be evaluated.
#' @param model Object of class \code{causalSEM}.
#' @param use_model_values Logical value indicating if model values should be 
#' used (TRUE) in calculation. Default: FALSE.
#' @param verbose Integer number describing the verbosity of console output.
#' Admissible values: 0: no output (default), 1: user messages, 
#' 2: debugging-relevant messages.
#' @return List of numeric vectors (named if \code{var_names} are supplied) 
#' containing the values of the marginal pdf of each outcome variable (over 
#' a grid of values). 
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z

## Function definition
calculate_interventional_density <- function(E = NULL,
                                             V = NULL,
                                             var_names = NULL,
                                             n_grid = NULL,
                                             model = NULL,
                                             use_model_values = FALSE,
                                             verbose = NULL){

	# function name
	fun.name <- "calculate_interventional_density"

	# function version
	fun.version <- "0.0.4 2023-04-21"

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
	  
	  
	  E <- model$interventional_distribution$means$values[,1]
	  V <- 
	    diag(model$interventional_distribution$covariance_matrix$values)
	  var_names <- model$info_model$var_names
	  n_grid <- NULL
	  
	  } else if (use_model_values == FALSE) {
	  
	  # TODO: include argument check
	  # TODO: require var_names as an obligatory argument!
	  
	  verbose <- handle_verbose_argument(verbose)
	  E <- E
	  V <- V
	  var_names <- var_names
	  n_grid <- n_grid
	  
	}

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# standard deviations (sqrt of diagonal elements of V)
	sds <- sqrt( V )

	# calculate pdfs for each variable
	# CG 0.0.2 2022-01-13 commented out this part 
	# pdfs <- mapply( function( mean, sd ){ stats::dnorm( mean, mean=mean, sd=sd ) }, E,
	                # sds, SIMPLIFY=TRUE )
	
	# CG 0.0.2 2022-01-13 introduced this part
	# generate x values and calculate pdfs for each variable, return list
	# TODO: use the n_grid argument here when defining the x sequence
	pdfs <- mapply( function( mean, sd ){
	  
	  # generate x-axis values
	  
	  if (is.null(n_grid)){
	    x <- seq( -3*sd, 3*sd, length.out = 5 ) + mean
	  } else {
	    x <- seq( -3*sd, 3*sd, length.out = n_grid ) + mean  
	  }
	  
	  # get pdf values
	  pdf.values <- stats::dnorm( x, mean=mean, sd=sd )
	  
	  # return
	  as.matrix( data.frame( "x"=x, "pdf.values"=pdf.values ) )
	  
	}, E, sds, SIMPLIFY=FALSE )
	

	# set variable names for list elements 
	# CG 0.0.4 2023-04-19: remove check of var_names towards the end of the script
	#if( !is.null( var_names ) ){
		names( pdfs ) <- var_names
	#}

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( pdfs )
}


### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R", full.names = TRUE )
# Rfiles <- Rfiles[ ! grepl("calculate_interventional_density.R", Rfiles) ]
# for( Rfile in Rfiles ){
	# cat( paste0( Rfile, "\n" ) ); flush.console()
	# source( Rfile )
# }

# calculate_interventional_density( 1, 2, "var1", verbose=2 )
# calculate_interventional_density( 1, 2, "var1", verbose=0 )
# calculate_interventional_density( 1, 2, NULL, verbose=0 )
# calculate_interventional_density( c(1,1), c(2,2), c("var1","var2"), verbose=0 )


### test
# require( testthat )
# test_file("../tests/testthat/XXXXXXX.R")
