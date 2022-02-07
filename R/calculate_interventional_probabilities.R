## Changelog:
# CG 0.0.5 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MH 0.0.3 2021-11-22: renamed from calc_interventional_probability to 
# calculate_interventional_probabilities
# MH 0.0.2 2021-10-14: added loop over values
# MH 0.0.1 2021-09-27: initial programming

## Documentation
#' @title Calculate Probabilites of Interventional Events
#' @description Calculates probabilities of interventional events, for example,
#' the probabily that the outcome variable realizes within a critical range of
#' values given an intervention on the exposure. 
#' @param mean mean(s)
#' @param sd standard deviation(s)
#' @param y_low lower bound(s)
#' @param y_up upper bound(s)
#' @param verbose verbosity of console outputs
#' @return The interventional probability/ies (numeric value/s) as defined in
#'  Eq. 22d (p. 22) of Gische and Voelkle (2021).
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal

## Function definition
calculate_interventional_probabilities <- function( mean, sd, y_low, y_up,
                                                    verbose ){

	# function name
	fun.name <- "calculate_interventional_probabilities"

	# function version
	fun.version <- "0.0.3 2021-11-22"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", 
	                                Sys.time(), "\n" ) )

	# Eq. 22d, p. 22
	p <- mapply( function( mean, sd, y_low, y_up ) { 
	  stats::pnorm( ( y_up - mean ) / sd ) - stats::pnorm( ( y_low - mean ) / sd ) }, 
	  mean, sd, y_low, y_up, SIMPLIFY=TRUE )

	# return p
	return( p )
}
