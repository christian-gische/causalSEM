# TODO: Use chunks of this function to define function according to the general structuring.
# According to our current structuring, there is 
# - one function that calculates the values (point estimates) (calculate_interventional_density)
# - one function that calculates the Jacobian (calculate_jacobian_interventional_density)
# - one function that calculates the ASE (calculate_ase_interventional_density)
# - one function that fills in point estimate into internal list (fill_in_interventional_density.R)
# - one function that fills in asymptotic inference into internal list (fill_in_asymptotics_interventional_density.R)
# --> My guess would be that chunks of the current version of the function interventional_density
# can be used in several of the functions suggested above

## Changelog:
# CG 0.0.3 2021-11-24: changed $variance to $covariance in internal_list path
# CG 0.0.4 2021-11-23: Added TODO above
# MH 0.0.3 2021-10-31: test dummy se added
# MH 0.0.2 2021-10-14: functional update
# MH 0.0.1 2021-09-27: initial programming

## Documentation
#' @title Calculates interventional density
#' @description Internal function that calculates interventional density
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{interventional_density} returns the inputted internal_list with slot
#'    interventional_distribution$density$pdf populated with a list (one named entry for each variable),
#'    each list element contains a matrix with two columns and 200 rows:
#'    [1] "x": generated x values (-3*SD ... 3*SD) 
#'    [2] "pdf.values": values from dnorm
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
interventional_density <- function( internal_list ){

	# function name
	fun.name <- "calculate_interventional_density"

	# function version
	fun.version <- "0.0.4 2021-11-23"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# get intervential mean and variance
	E <- internal_list$interventional_distribution$moments$mean_vector
	V <- internal_list$interventional_distribution$moments$covariance_matrix
	
	# standard deviations (sqrt of diagonal elements of V)
	sds <- sqrt( diag( V ) )

	# generate x values and calculate pdfs for each variable, return list
	l <- mapply( function( mean, sd ){

							# generate x-axis values
							x <- seq( -3*sd, 3*sd, length.out=200 ) + mean

							# get pdf values
							pdf.values <- dnorm( x, mean=mean, sd=sd )

							# get standard errors
							# TODO call se function
							# TEST DUMMY
							se <- rep( 0.001, length( x ) )

							# 95% CI
							LL95 <- pdf.values - qnorm(0.975)*se
							UL95 <- pdf.values + qnorm(0.975)*se

							# return
							as.matrix( data.frame( "x"=x, "pdf.values"=pdf.values, "se"=se, "LL95"=LL95, "UL95"=UL95 ) )

					}, E[,1], sds, SIMPLIFY=FALSE )

	# set variable names for list elements 
	names( l ) <- internal_list$info_model$var_names

	# populate slot
	internal_list$interventional_distribution$density$pdf <- l

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}


### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "interventional_density.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

## test object 00_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
# o00_internal_list <- intervention_effect( model=o00_lavaan_test_object,intervention="x2",intervention_level=2)
# str( o00_internal_list$interventional_distribution$density$pdf )

## test object 01_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
# o01_internal_list <- intervention_effect( model=o01_lavaan_test_object,intervention="x2",intervention_level=2)
# str( o01_internal_list$interventional_distribution$density$pdf )

## test object 02_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
# o02_internal_list <- intervention_effect( model=o02_lavaan_test_object,intervention="x2",intervention_level=2)
# str( o02_internal_list$interventional_distribution$density$pdf )

## test object 03_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
# o03_internal_list <- intervention_effect( model=o03_lavaan_test_object,intervention="x2",intervention_level=2)
# str( o03_internal_list$interventional_distribution$density$pdf )


### test
# require( testthat )
# test_file("../tests/testthat/XXXXXXX.R")
