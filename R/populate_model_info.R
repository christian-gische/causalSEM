## Changelog:
# MH 0.0.3 2021-10-14: updated documentation
# MA 0.0.2 2021-09-09: this function stores the model in the designated slot
# MH 0.0.1 2021-09-01: initial programming

## Documentation
#' @title Extracts number of observations, number of manifest variables,
#'    and names of observed variables from a fitted SEM object
#' @description Internal function that extracts number of observations,
#'    number of manifest variables, and names of observed variables from a fitted
#'    structural equation model. Supported fitted objects: lavaan.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @param model Fitted model. The fitted model can be of class lavaan.
#' @return \code{populate_model_info} returns the inputted internal_list with slots
#'    \code{n_obs}, \code{n_ov}, and \code{var_names} populated.
#' @seealso \code{\link{build_C}} \code{\link{build_Psi}}
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
populate_model_info <- function(internal_list, model){

	# function name
	fun.name <- "populate_model_info"

	# function version
	fun.version <- "0.0.3 2021-10-14"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# get fit object from internal_list
	internal_list$fitted_object <- fit <- model

	# get class of fit object
	internal_list$fitted_object_class <- class(model)

	# supported fit objects
	supported_fit_objects <- fit_class <- c( "lavaan" )

	# check if supported
	if(!any(supported_fit_objects %in% supported_fit_objects)) stop(
	  paste0(
	    fun.name.version, ": fit object of class ", fit_class,
	    " not supported. Supported fit objects are: ",
	    paste(supported_fit_objects, collapse = ", ")
	    )
	  )

	# require package
	if( fit_class %in% "lavaan" ) require( lavaan )

	# model representation must be "LISREL"
	model.rep <- fit@Model@representation
	if( !model.rep %in% "LISREL" ) stop( paste0( fun.name.version, ": model representation as defined in fit@Model@representation must be LISREL, but it is ", paste( model.rep, collapse=", " ) ) )

	# function to check if is integer
	# https://stackoverflow.com/questions/3476782/check-if-the-number-is-integer
	is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

	# number of observations
	n_obs <- lavInspect( fit, "ntotal" )
	if( is.numeric( n_obs ) && !n_obs<0 && is.wholenumber( n_obs ) ){
		# set in internal_list
		internal_list$info_model$"n_obs" <- as.integer( n_obs )
	} else {
		stop( paste0( fun.name.version, ": setting n_obs in internal list failed; lavInspect( fit, 'ntotal' ) returned ", n_obs ) )
	}

	# variable names of observed variables
	var_names <- lavNames( fit )
	if( is.character( var_names ) ){
		# set in internal_list
		internal_list$info_model$"var_names" <- var_names
	} else {
		stop( paste0( fun.name.version, ": setting var_names in internal list failed; lavNames( fit ) returned ", var_names ) )
	}

	# number of manifest variables
	n_ov <- length( internal_list$info_model$"var_names" )
	if( is.numeric( n_ov ) && !n_ov<0 && is.wholenumber( n_ov ) ){
		# set in internal_list
		internal_list$info_model$"n_ov" <- as.integer( n_ov )
	} else {
		stop( paste0( fun.name.version, ": setting n_ov in internal list failed; length( internal_list$info_model$'var_names' ) returned ", n_ov ) )
	}

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}

## test/development
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/verbose_argument_handling.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/make_empty_list.R" )
## load( "c:/Users/martin/Dropbox/68_causalSEM/91_zeug/fit.lcs2.Rdata" )
# load( "c:/Users/martin/Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" )
# internal_list <- make_empty_list()
# internal_list$fitted_object <- o01_lavaan_test_object
# internal_list$fitted_object_class <- class( o01_lavaan_test_object )
# ( internal_list <- populate_model_info( internal_list ) )
# internal_list$info_model
