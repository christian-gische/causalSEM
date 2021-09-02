## Changelog:
# MH 0.0.3 2021-09-01: C list is now correctly inputted into internal list
# MH 0.0.2 2021-07-30: added documentation, changed input and output argument to internal_list
# MH 0.0.1 2021-07-20: initial programming

## Documentation
#' @title Extracts the structural coefficient matrix from a fitted SEM object
#' @description Internal function that extracts the structural coefficient matrix from a fitted
#'    structural equation model. Supported fitted objects: lavaan.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{build_C} returns the inputted internal_list with slot
#'    C populated with a two-entry list: "values" is a numeric matrix
#'    containing the structural coefficients (if available with row- and
#'    column names); "labels" is a character matrix containing parameter
#'    labels (NA for unlabeled parameters), NULL if labels are not extractable.
#' @seealso \code{\link{build_Psi}}
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
build_C <- function( internal_list ){

	# function name
	fun.name <- "build_C"

	# function version
	fun.version <- "0.0.3 2021-09-01"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
	
	# get fit object from internal_list
	fit <- internal_list$fitted_object

	# get class of fit object
	fit.class <- internal_list$fitted_object_class
	
	# supported fit objects
	supported.fit.objects <- c( "lavaan" )

	# check if supported
	if( !any( supported.fit.objects %in% fit.class ) ) stop( paste0( fun.name.version, ": fit object of class ", fit.class, " not supported. Supported fit objects are: ", paste( supported.fit.objects, collapse=", " ) ) )

	# require package
	if( fit.class %in% "lavaan" ) require( lavaan )

	# model representation must be "LISREL"
	model.rep <- fit@Model@representation
	if( !model.rep %in% "LISREL" ) stop( paste0( fun.name.version, ": model representation as defined in fit@Model@representation must be LISREL, but it is ", paste( model.rep, collapse=", " ) ) )

	# check whether beta is present in fit object
	GLIST.names <- names( fit@Model@GLIST )
	if( !any( GLIST.names %in% "beta" ) ) stop( paste0( fun.name.version, ": fit@Model@GLIST does not contain beta, but only ", paste( GLIST.names, collapse=", " ) ) )

	# get beta matrix from fit object
	C <- fit@Model@GLIST$beta

	# check dimensions
	C.dim <- dim( C )
	C.dim.n <- length( C.dim )
	if( !C.dim.n ) stop( fun.name.version, ": number of dimensions is not 2, but ", C.dim.n )
	if( !( all( length( C.dim ) >= 1 ) & all( C.dim %in% C.dim[1] ) ) ) stop( paste0( fun.name.version, ": not all dimensions are of same length or at least one dimension is of length < 1: ", paste( C.dim, collapse=", " ) ) )

	# get dimension names
	beta.dimNames <- fit@Model@dimNames[ names( fit@Model@GLIST ) %in% "beta" ][[1]]

	# set dimension names
	rownames( C ) <- beta.dimNames[[1]]
	colnames( C ) <- beta.dimNames[[2]]

	## labels of parameters in C matrix
	# initialization of C.lab
	C.lab <- NULL
	
	# if parameter labels exist (row/colnames of C) then try to extract parameter labels from partab
	if( !is.null( rownames( C ) ) & !is.null( colnames( C ) ) ){

		# empty matrix (consistent with C)
		C.lab <- C
		C.lab[] <- as.character(NA)
		
		# parameter table
		partab <- parTable( fit )
		
		# loop over elements of C.lab matrix
		for ( r in 1:nrow( C.lab ) ){
			for ( s in 1:ncol( C.lab ) ){
				# dependent variable
				lhs <- rownames( C.lab )[r]
				# predictor variable
				rhs <- colnames( C.lab )[s]
				# get label from partab
				lab <- partab[ partab$lhs %in% lhs & partab$op %in% "~" & partab$rhs %in% rhs, "label" ]
				# if not available or empty string, then NA
				if( length( lab ) == 0 | identical( lab, "" ) ) lab <- NA
				# set label
				C.lab[r,s] <- lab
			}
		}
	} else {
		# console output
		if( verbose >= 2 ) cat( paste0( fun.name.version, " ", Sys.time(), ": unable to extract parameter labels", "\n" ) )
	}
	
	# create list, first entry: values (=C matrix), labels (=labels of the parameters in the C matrix)
	C.list <- list( "values" = C, "labels" = C.lab )
	
	# populate slot C of internal_list
	internal_list$info_model$C <- C.list
	
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
# ( internal_list <- build_C( internal_list ) )
# internal_list$info_model
