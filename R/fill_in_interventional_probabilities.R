## Changelog:
# CG 0.0.8 2023-04-19: changed arguments in call of 
#                      calculate_interventional_probability
# CG 0.0.7 2023-02-28: check if argument is of class causalSEM
# CG 0.0.6 2022-03-08: allow for multivariate lower and upper bounds
# CG 0.0.5 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# CG 0.0.4 2021-11-24: changed $variance to $covariance in internal_list path
# MH 0.0.3 2021-11-22: renamed from interventional_probability to 
# fill_in_interventional_probabilities
# MH 0.0.2 2021-10-14: functional update
# MH 0.0.1 2021-09-27: initial programming

## Documentation
#' @title Fill in Interventional Probabilities to List
#' @description Fills in probabilities of interventional events to internal 
#' list. See, for example, Eqs. 10, 15, and 22d in Gische and Voelkle (2022).
#' @param internal_list A list with information extracted from the model.
#' @return The inputted list with slot
#' \code{..$interventional_distribution$probabilities$values} 
#' filled in.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_interventional_probabilities <- function(internal_list = NULL){

	# function name
	fun.name <- "fill_in_interventional_probabilities"

	# function version
	fun.version <- "0.0.7 2023-02-28"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
	
	# CG 0.0.7 2023-02-28: check if argument is of class causalSEM 
	# check function arguments 
	## get class of model object
	model_class <- class(internal_list)
	
	## set supported classes of model objects
	supported_model_classes <- c( "causalSEM" )
	
	## check if argument model is supported
	if(!any(model_class %in% supported_model_classes)) stop(
	  paste0(
	    fun.name.version, ": model of class ", model_class,
	    " not supported. Supported fit objects are: ",
	    paste(supported_model_classes, collapse = ", ")
	  )
	)

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", 
	                                Sys.time(), "\n" ) )

	
		
		# calculate interventional probability
		p <- calculate_interventional_probabilities(model = internal_list,
		                                            use_model_values = TRUE)
	
	
	# populate slot
	internal_list$interventional_distribution$probabilities$values <- p
	
	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}

### development
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/handle_verbose_argument.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/make_empty_list.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/populate_model_info.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/lav_parTable_fill_labels.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/build_C.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/build_Psi.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/build_theta.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/populate_intervention_info.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/build_constant_matrix.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/interventional_moments.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/calc_interventional_probability.R" )

## test object 00_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
# o00_internal_list <- make_empty_list()
# o00_internal_list <- populate_model_info( o00_internal_list, o00_lavaan_test_object )
# o00_internal_list <- build_C( o00_internal_list )
# o00_internal_list <- build_Psi( o00_internal_list )
# o00_internal_list <- build_theta( o00_internal_list )
# o00_internal_list <- populate_intervention_info( o00_internal_list, c("x2"), c("y3"), 11.48, c("mean"), -40, 80 )
# o00_internal_list <- build_constant_matrix( o00_internal_list )
# o00_internal_list <- interventional_moments( o00_internal_list )
# o00_internal_list <- interventional_probability( o00_internal_list )
# o00_internal_list$interventional_distribution$probabilities$values


## test object 01_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
# o01_internal_list <- make_empty_list()
# o01_internal_list <- populate_model_info( o01_internal_list, o01_lavaan_test_object )
# o01_internal_list <- build_C( o01_internal_list )
# o01_internal_list <- build_Psi( o01_internal_list )
# o01_internal_list <- build_theta( o01_internal_list )
# o01_internal_list <- populate_intervention_info( o01_internal_list, c("x2"), c("y3"), 11.48, c("mean"), -40, 80 )
# o01_internal_list <- build_constant_matrix( o01_internal_list )
# o01_internal_list <- interventional_moments( o01_internal_list )
# o01_internal_list <- interventional_probability( o01_internal_list )
# o01_internal_list$interventional_distribution$probabilities$values


## test object 02_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
# o02_internal_list <- make_empty_list()
# o02_internal_list <- populate_model_info( o02_internal_list, o02_lavaan_test_object )
# o02_internal_list <- build_C( o02_internal_list )
# o02_internal_list <- build_Psi( o02_internal_list )
# o02_internal_list <- build_theta( o02_internal_list )
# o02_internal_list <- populate_intervention_info( o02_internal_list, c("x2"), c("y3"), 11.48, c("mean"), -40, 80 )
# o02_internal_list <- build_constant_matrix( o02_internal_list )
# o02_internal_list <- interventional_moments( o02_internal_list )
# o02_internal_list <- interventional_probability( o02_internal_list )
# o02_internal_list$interventional_distribution$probabilities$values


## test object 03_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
# o03_internal_list <- make_empty_list()
# o03_internal_list <- populate_model_info( o03_internal_list, o03_lavaan_test_object )
# o03_internal_list <- build_C( o03_internal_list )
# o03_internal_list <- build_Psi( o03_internal_list )
# o03_internal_list <- build_theta( o03_internal_list )
# o03_internal_list <- populate_intervention_info( o03_internal_list, c("x2"), c("y3"), 11.48, c("mean"), -40, 80 )
# o03_internal_list <- build_constant_matrix( o03_internal_list )
# o03_internal_list <- interventional_moments( o03_internal_list )
# o03_internal_list <- interventional_probability( o03_internal_list )
# o03_internal_list$interventional_distribution$probabilities$values


### test
# require( testthat )
# test_file("../tests/testthat/test_interventional_moments.R")
