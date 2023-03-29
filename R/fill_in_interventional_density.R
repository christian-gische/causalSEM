## Changelog:
# CG 0.0.3 2023-02-28: check if argument is of class causalSEM 
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# MH 0.0.1 2021-11-30: chunked from interventional_density() 0.0.5 2021-11-24

## Documentation
#' @title Fill in pdf of Interventional Distribution to List
#' Fill in the probability density function (pdf) of the 
#' interventional distribution to the internal list. The values are computed 
#' pointwise over a grid of values for each univariate variable separately 
#' (i.e., the pdfs of the marginal distributions). See, for example, 
#' Eqs. 9, 14, and 22c in Gische and Voelkle (2022).
#' @param internal_list A list with information extracted from the model.
#' @return The inputted list with slot
#' \code{..$interventional_distribution$density_function$values} 
#' filled in.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z

## Function definition
fill_in_interventional_density <- function(internal_list = NULL){

	# function name
	fun.name <- "fill_in_interventional_density"

	# function version
	fun.version <- "0.0.3 2023-02-28"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
	
	# CG 0.0.3 2023-02-28: check if argument is of class causalSEM 
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
