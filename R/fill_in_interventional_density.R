## Changelog:
# MH 0.0.1 2021-11-30: chunked from interventional_density() 0.0.5 2021-11-24

## Documentation
#' @title Fill in interventional density
#' @description Fills in slot internal_list$interventional_distribution$density$pdfs
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{fill_in_interventional_density} returns the inputted internal_list with slot
#'    internal_list$interventional_distribution$density$pdfs populated
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
fill_in_interventional_density <- function( internal_list ){

	# function name
	fun.name <- "fill_in_interventional_density"

	# function version
	fun.version <- "0.0.1 2021-11-30"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# get intervential mean and variance and variable names
	E <- internal_list$interventional_distribution$moments$mean_vector[,1]
	V <- diag( internal_list$interventional_distribution$moments$covariance_matrix )
	var_names <- internal_list$info_model$var_names

	# calculate interventional density
	pdfs <- calculate_interventional_density( E=E, V=V, var_names=var_names, verbose=verbose )

	# populate slot
	internal_list$interventional_distribution$density$pdfs <- pdfs

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}


### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R", full.names = TRUE )
# Rfiles <- Rfiles[ ! grepl("fill_in_interventional_density.R", Rfiles) ]
# for( Rfile in Rfiles ){
	# cat( paste0( Rfile, "\n" ) ); flush.console()
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
