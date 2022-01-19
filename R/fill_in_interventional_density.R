## Changelog:
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# MH 0.0.1 2021-11-30: chunked from interventional_density() 0.0.5 2021-11-24

## Documentation
#' @title Fill in Density Function of the Interventional Distribution
#' @description Fills in slot internal_list$interventional_distribution$
#' density_function$values in the internal_list.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted internal_list with slot
#' internal_list$interventional_distribution$density_function$values filled in.
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal

## Function definition
fill_in_interventional_density <- function( internal_list ){

	# function name
	fun.name <- "fill_in_interventional_density"

	# function version
	fun.version <- "0.0.2 2022-01-13"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# get intervential mean and variance and variable names
	E <- internal_list$interventional_distribution$means$values[,1]
	V <- 
	  diag( internal_list$interventional_distribution$covariance_matrix$values )
	var_names <- internal_list$info_model$var_names

	# calculate interventional density
	pdfs <- calculate_interventional_density( E=E, V=V, var_names=var_names, 
	                                          verbose=verbose )

	# populate slot
	internal_list$interventional_distribution$density_function$values <- pdfs

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

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
# o00_internal_list <- intervention_effect( model=o00_lavaan_test_object,intervention="x2",intervention_levels=2)
# str( o00_internal_list$interventional_distribution$density$pdf )

## test object 01_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
# o01_internal_list <- intervention_effect( model=o01_lavaan_test_object,intervention="x2",intervention_levels=2)
# str( o01_internal_list$interventional_distribution$density$pdf )

## test object 02_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
# o02_internal_list <- intervention_effect( model=o02_lavaan_test_object,intervention="x2",intervention_levels=2)
# str( o02_internal_list$interventional_distribution$density$pdf )

## test object 03_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
# o03_internal_list <- intervention_effect( model=o03_lavaan_test_object,intervention="x2",intervention_levels=2)
# str( o03_internal_list$interventional_distribution$density$pdf )


### test
# require( testthat )
# test_file("../tests/testthat/XXXXXXX.R")
