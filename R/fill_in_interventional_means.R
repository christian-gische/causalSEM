## Changelog:
# CG 0.0.3 2023-02-28: check if argument is of class causalSEM 
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title Fills in Interventional Mean to List
#' @description Fills in the mean vector of the interventional distribution into
#' the internal list (see, for example, Eq. 6a in Gische and Voelkle, 
#' 2022).
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted list with slot
#'    \code{..interventional_distribution$means$values} filled in.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_interventional_means <- function(internal_list = NULL){

	# function name
	fun.name <- "fill_in_interventional_means"

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
	
	# get/define terms
	C <- internal_list$info_model$C$values
	
	# interventional level
	x <- internal_list$info_interventions$intervention_levels
	
	# selection matrix 1_I
	SI <- internal_list$constant_matrices$select_intervention
	
	# number of observed variables
	n <- internal_list$info_model$n_ov
	
	# I_N matrix
	IN <- internal_list$constant_matrices$eliminate_intervention	
	
	# calculate interventional means
	E <- calculate_interventional_means( C=C, x=x, SI=SI, n=n, IN=IN,
	                                     verbose=verbose )
	
	# populate slots
	internal_list$interventional_distribution$means$values <- E

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}
