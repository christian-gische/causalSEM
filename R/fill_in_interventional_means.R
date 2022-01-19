## Changelog:
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title Fills in Mean Vector of the Interventional Distribution to Internal 
#' List
#' @description Fills in the mean vector of the interventional distribution to
#' the slot internal_list$interventional_distribution$means$values 
#' in the internal_list.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted internal_list with slot
#'    interventional_distribution$means$values filled in.
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal

## Function definition
fill_in_interventional_means <- function( internal_list ){

	# function name
	fun.name <- "fill_in_interventional_means"

	# function version
	fun.version <- "0.0.2 2022-01-13"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

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
