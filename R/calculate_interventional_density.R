## Changelog:
# CG 0.0.2 2022-01-13: function now computes the values of the pdf 
#                      over a grid of -3*SD ; +3*SD
#                      changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# MH 0.0.1 2021-11-30: chunked from interventional_density() 0.0.5 2021-11-24

## Documentation
#' @title Calculate the Density Function of the Interventional Distribution
#' @description Calculate the density function of the interventional 
#' distribution. The values are computed pointwise over a grid of 200 
#' values in the range [-3*SD, 3*SD] of each univariate variable separately.
#' @param E vector of means
#' @param V vector of variances
#' @param var_names optional, vector of variable names
#' @return vector (named if var_names are supplied) of density values
#'  [ stats::pnorm( q=E, mean=E, sd=sqrt(V) ) ]
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z

## Function definition
calculate_interventional_density <- function( E, V, var_names=NULL, verbose ){

	# function name
	fun.name <- "calculate_interventional_density"

	# function version
	fun.version <- "0.0.2 2022-01-13"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

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
	pdfs <- mapply( function( mean, sd ){
	  
	  # generate x-axis values
	  x <- seq( -3*sd, 3*sd, length.out=200 ) + mean
	  
	  # get pdf values
	  pdf.values <- stats::dnorm( x, mean=mean, sd=sd )
	  
	  # return
	  as.matrix( data.frame( "x"=x, "pdf.values"=pdf.values ) )
	  
	}, E, sds, SIMPLIFY=FALSE )
	

	# set variable names for list elements 
	if( !is.null( var_names ) ){
		names( pdfs ) <- var_names
	}

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
