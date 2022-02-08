## Changelog:
# MA 0.0.3 2022-02-07: Added Psi to @param
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title Calculate Covariance Matrix of the Interventional Distribution
#' @description Calculate Covariance Matrix of the Interventional Distribution.
#' @param C C matrix
#' @param Psi Psi matrix
#' @param x interventional level
#' @param SI selection matrix 1_I
#' @param n number of observed variables
#' @param IN I_N matrix
#' @param verbose A single number, integer. 0...no output (default),
#' 1...user messages, 2...debugging-relevant messages.
#' @return The covariance matrix of the interventional distribution.
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible
#' framework for studying causal effects using linear models. Psychometrika
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
calculate_interventional_covariance_matrix <- function( C, Psi, x, SI, n, IN,
                                                        verbose ){

	# function name
	fun.name <- "calculate_interventional_covariance_matrix"

	# function version
	fun.version <- "0.0.2 2022-01-13"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# identity matrix
	In <- diag( n )

	# calculate interventional covariance matrix, Eq. 6b in Gische and
	# Voelkle (2021)
	V <-
	  solve( In - IN %*% C ) %*% IN %*% Psi %*% IN %*% t( solve( In - IN %*% C ) )

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( V )
}
