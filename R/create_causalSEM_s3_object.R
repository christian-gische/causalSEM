## Changelog:
# CG 0.0.2 2023-02-21: changes in preamble and comments
# MH 0.0.1 2021-11-30: initial programming

## Documentation
#' @title causalSEM S3 object
#' @description Assign class causalSEM to internal list.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return Object of class causalSEM.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
create_causalSEM_s3_object <- function( internal_list ){

	# function name
	fun.name <- "create_causalSEM_s3_object"

	# function version
	fun.version <- "0.0.2 2023-02-21"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# assign class causalSEM
	causalSEM <- structure( internal_list, class = "causalSEM" )
	
	# class( causalSEM )
	# inherits( causalSEM, "causalSEM" )
	
	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return internal list
	return( causalSEM )
}


## development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R", full.names = TRUE )
# Rfiles <- Rfiles[ ! grepl("create_causalSEM_s3_object.R", Rfiles) ]
# for( Rfile in Rfiles ){
	# cat( paste0( Rfile, "\n" ) ); flush.console()
	# source( Rfile )
# }

## test object 00_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
# o00_internal_list <- intervention_effect( model=o00_lavaan_test_object,intervention="x2",intervention_levels=2 )
# class( o00_internal_list )

## test object 01_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
# o01_internal_list <- intervention_effect( model=o01_lavaan_test_object,intervention="x2",intervention_levels=2)
# class( o01_internal_list )

## test object 02_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
# o02_internal_list <- intervention_effect( model=o02_lavaan_test_object,intervention="x2",intervention_levels=2)
# class( o02_internal_list )

## test object 03_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
# o03_internal_list <- intervention_effect( model=o03_lavaan_test_object,intervention="x2",intervention_levels=2)
# class( o03_internal_list )

### test
# require( testthat )
# test_file("../tests/testthat/XXXXXXX.R")
