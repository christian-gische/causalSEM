## Changelog:
# CG 0.0.4 2023-02-28: change preamble for documentation
# MA 0.0.3 2022-02-07: Added Psi to @param
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title Calculate Interventional Mean
#' @description Calculate mean vector of the interventional distribution 
#' (see, for example, Eqs. 6a, 12a, and 22a in Gische and Voelkle, 2022). 
#' The arguments SI and IN (zero-one matrices) of the function are 
#' described in Definition 1 in Gische and Voelkle (2022).
#' @param C Numeric matrix of structural coefficients.
#' @param x Numeric vector of interventional levels.
#' @param SI Numeric selection matrix.
#' @param n Integer number of observed variables.
#' @param IN Numeric zero-one matrix.
#' @param verbose Integer number describing the verbosity of console output.
#' Admissible values: 0: no output (default), 1: user messages, 
#' 2: debugging-relevant messages.
#' @return The numeric mean vector of the interventional distribution.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z

## Function definition
calculate_interventional_means <- function(C = NULL,
                                           x = NULL,
                                           SI = NULL,
                                           n = NULL,
                                           IN = NULL,
                                           verbose = NULL){

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

	# calculate interventional means, Eq. 6a in Gische and Voelkle (2022)
	E <- solve( In - IN %*% C ) %*% SI %*% x

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( E )
}
