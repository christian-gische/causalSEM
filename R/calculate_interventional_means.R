## Changelog:
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title Calculate Mean Vector of the Interventional Distribution
#' @description Calculate Mean Vector of the Interventional Distribution.
#' @param C C matrix
#' @param x interventional level
#' @param SI selection matrix 1_I
#' @param n number of observed variables
#' @param IN I_N matrix
#' @param verbose
#' @return The Mean Vector of the Interventional Distribution.
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal

## Function definition
calculate_interventional_means <- function( C, x, SI, n, IN, verbose ){

	# function name
	fun.name <- "calculate_interventional_means"

	# function version
	fun.version <- "0.0.2 2022-01-13"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# identity matrix
	In <- diag( n )

	# calculate interventional means, Eq. 6a in Gische and Voelkle (2021)
	E <- solve( In - IN %*% C ) %*% SI %*% x

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", 
	                                Sys.time(), "\n" ) )

	# return internal list
	return( E )
}
