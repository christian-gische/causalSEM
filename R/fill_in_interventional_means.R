## Changelog:
# MH 0.0.1 2021-11-22: chunked from interventional_moments() 0.0.2 2021-10-14

## Documentation
#' @title fill_in_interventional_means
#' @description Fills in slot internal_list$interventional_distribution$moments$mean_vector
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{fill_in_interventional_means} returns the inputted internal_list with slot
#'    interventional_distribution$moments$mean_vector populated
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
fill_in_interventional_means <- function( internal_list ){

	# function name
	fun.name <- "fill_in_interventional_means"

	# function version
	fun.version <- "0.0.1 2021-11-22"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
	
	# get/define terms
	C <- internal_list$info_model$C$values
	
	# interventional level
	x <- internal_list$info_interventions$intervention_level
	
	# selection matrix 1_I
	SI <- internal_list$interventional_distribution$zero_one_matrices$select_intervention
	
	# number of observed variables
	n <- internal_list$info_model$n_ov
	
	# I_N matrix
	IN <- internal_list$interventional_distribution$zero_one_matrices$eliminate_intervention	
	
	# calculate interventional means
	E <- calculate_interventional_means( C=C, x=x, SI=SI, n=n, IN=IN, verbose=verbose )
	
	# populate slots
	internal_list$interventional_distribution$moments$mean_vector <- E

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}
