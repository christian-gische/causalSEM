## Changelog:
# MH 0.0.1 2021-09-27: initial programming

## Documentation
#' @title Function to calculate interventional probability
#' @description Internal function that 
#' @param 
#' @return \code{calc_interventional_probability} returns the
#'    interventional probability (numeric value) as defined in Eq. 22d (p. 22)
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
calc_interventional_probability <- function( E, V, y_low, y_up, verbose ){

	# function name
	fun.name <- "calc_interventional_probability"

	# function version
	fun.version <- "0.0.1 2021-09-27"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# Eq. 22d, p. 22
	p <- pnorm( ( y_up - E ) / sqrt( V ) ) - pnorm( ( y_low - E ) / sqrt( V ) )

	# return p
	return( p )
}
