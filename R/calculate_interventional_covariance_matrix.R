## Changelog:
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title Calculates interventional covariance matrix
#' @description Internal function that calculates interventional covariance matrix
#' @param C C matrix
#' @param x interventional level
#' @param SI selection matrix 1_I
#' @param n number of observed variables
#' @param IN I_N matrix
#' @param verbose
#' @return \code{calculate_interventional_covariance_matrix} returns the interventional covariance matrix
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
calculate_interventional_covariance_matrix <- function( C, Psi, x, SI, n, IN, verbose ){

	# function name
	fun.name <- "calculate_interventional_covariance_matrix"

	# function version
	fun.version <- "0.0.1 2021-11-22"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# identity matrix
	In <- diag( n )
	
	# calculate interventional covariance matrix, Eq. 6b in Gische/Voelkle
	V <- solve( In - IN %*% C ) %*% IN %*% Psi %*% IN %*% t( solve( In - IN %*% C ) )
	
	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return internal list
	return( V )
}
