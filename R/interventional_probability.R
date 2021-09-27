## Changelog:
# MH 0.0.1 2021-09-27: initial programming

## Documentation
#' @title Calculates interventional probability
#' @description Internal function that 
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{interventional_probability} returns the inputted internal_list with slot
#'    interventional_distribution$probability$p populated with a numeric value
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
interventional_probability <- function( internal_list ){

	# function name
	fun.name <- "interventional_probability"

	# function version
	fun.version <- "0.0.1 2021-09-27"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# get outcome, z.B. Y3
	# internal_list$info_interventions

# browser()
	# get intervential mean and variance
	E <- internal_list$interventional_distribution$moments$mean_vector
	V <- internal_list$interventional_distribution$moments$variance_matrix

	# get upper lower bound
	y_low <- internal_list$info_interventions$lower_bound <- 3 # TEST
	y_up <- internal_list$info_interventions$upper_bound <- 6  # TEST

	# calculate interventional probability
	p <- calc_interventional_probability( E=E, V=V, y_low=y_low, y_up=y_up, verbose=verbose )
	
	# populate slot
	internal_list$interventional_distribution$probability$p <- p
	
	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}

### development
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/verbose_argument_handling.R" )
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/make_empty_list.R" )
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/populate_model_info.R" )
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/lav_parTable_fill_labels.R" )
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/build_C.R" )
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/build_Psi.R" )
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/build_theta.R" )
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/interventional_moments.R" )
source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/calc_interventional_probability.R" )

## test object 00_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
# o00_internal_list <- make_empty_list()
# o00_internal_list <- populate_model_info( o00_internal_list, o00_lavaan_test_object )
# o00_internal_list <- build_C( o00_internal_list )
# o00_internal_list <- build_Psi( o00_internal_list )
# o00_internal_list <- build_theta( o00_internal_list )


## test object 01_lavaan_test_object
load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
o01_internal_list <- make_empty_list()
o01_internal_list <- populate_model_info( o01_internal_list, o01_lavaan_test_object )
o01_internal_list <- build_C( o01_internal_list )
o01_internal_list <- build_Psi( o01_internal_list )
o01_internal_list <- build_theta( o01_internal_list )
o01_internal_list <- interventional_moments( o01_internal_list )

o01_internal_list <- interventional_probability( o01_internal_list )

o01_internal_list$interventional_distribution$probability$p



## test object 02_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
# o02_internal_list <- make_empty_list()
# o02_internal_list <- populate_model_info( o02_internal_list, o02_lavaan_test_object )
# o02_internal_list <- build_C( o02_internal_list )
# o02_internal_list$info_model$C

## test object 03_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
# o03_internal_list <- make_empty_list()
# o03_internal_list <- populate_model_info( o03_internal_list, o03_lavaan_test_object )
# o03_internal_list <- build_C( o03_internal_list )
# o03_internal_list$info_model$C

### test
# require( testthat )
# test_file("../tests/testthat/test_interventional_moments.R")
