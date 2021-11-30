## Changelog:
# MH 0.0.1 2021-11-30: chunked from interventional_density() 0.0.5 2021-11-24

## Documentation
#' @title Interventional density
#' @description Calculates interventional density
#' @param E vector of means
#' @param V vector of variances
#' @param var_names optional, vector of variable names
#' @return vector (named if var_names are supplied) of density values [ pnorm( q=E, mean=E, sd=sqrt(V) ) ]
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
calculate_interventional_density <- function( E, V, var_names=NULL, verbose ){

	# function name
	fun.name <- "calculate_interventional_density"

	# function version
	fun.version <- "0.0.1 2021-11-30"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# standard deviations (sqrt of diagonal elements of V)
	sds <- sqrt( V )

	# calculate pdfs for each variable
	pdfs <- mapply( function( mean, sd ){ dnorm( mean, mean=mean, sd=sd ) }, E, sds, SIMPLIFY=TRUE )

	# set variable names for list elements 
	if( !is.null( var_names ) ){
		names( pdfs ) <- var_names
	}

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

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
