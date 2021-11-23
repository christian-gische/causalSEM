## Changelog:
# MH 0.0.3 2021-11-22: renamed from calc_interventional_probability to calculate_interventional_probabilities
# MH 0.0.2 2021-10-14: added loop over values
# MH 0.0.1 2021-09-27: initial programming

## Documentation
#' @title Function to calculate interventional probability
#' @description Internal function that calculates interventional probability
#'    (can be used with scalars or vectors as inputs)
#' @param mean mean(s)
#' @param sd standard deviation(s)
#' @param y_low lower bound(s)
#' @param y_up upper bound(s)
#' @param verbose verbosity of console outputs
#' @return \code{calculate_interventional_probabilities} returns the
#'    interventional probability/ies (numeric value/s) as defined in Eq. 22d (p. 22)
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
calculate_interventional_probabilities <- function( mean, sd, y_low, y_up, verbose ){

	# function name
	fun.name <- "calculate_interventional_probabilities"

	# function version
	fun.version <- "0.0.3 2021-11-22"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# Eq. 22d, p. 22
	p <- mapply( function( mean, sd, y_low, y_up ) { pnorm( ( y_up - mean ) / sd ) - pnorm( ( y_low - mean ) / sd ) }, mean, sd, y_low, y_up, SIMPLIFY=TRUE )

	# return p
	return( p )
}
