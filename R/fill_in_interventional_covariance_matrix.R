## Changelog:
# CG 0.0.3 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# CG 0.0.2 2021-11-24: changed $variance to $covariance in internal_list path
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title Fill in the Interventional Covariance Matrix to Internal List
#' @description Fills in slot internal_list$interventional_distribution$
#' covariance_matrix$values
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted internal_list with slot
#'    interventional_distribution$moments$variance_matrix filled in.
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_interventional_covariance_matrix <- function( internal_list ){

	# function name
	fun.name <- "fill_in_interventional_covariance_matrix"

	# function version
	fun.version <- "0.0.3 2022-01-13"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# get/define terms
	C <- internal_list$info_model$C$values
	Psi <- internal_list$info_model$Psi$values
	
	# interventional level
	x <- internal_list$info_interventions$intervention_levels
	
	# selection matrix 1_I
	SI <- internal_list$constant_matrices$select_intervention
	
	# number of observed variables
	n <- internal_list$info_model$n_ov
	
	# identity matrix
	In <- diag( n )
	
	# I_N matrix
	IN <- internal_list$constant_matrices$eliminate_intervention
	
	# calculate interventional covariance matrix
	V <- calculate_interventional_covariance_matrix( C=C, Psi=Psi, x=x, SI=SI,
	                                                 n=n, IN=IN, verbose=verbose )
	
	# populate slots
	internal_list$interventional_distribution$covariance_matrix$values <- V

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}
