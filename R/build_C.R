## Changelog:
# MH 0.0.1 2021-07-20: initial programming

# function definition
build_C <- function( fit, verbose=c(0,1,2) ){

	# function name
	fun.name <- "build_C"

	# function version
	fun.version <- "0.0.1 2021-07-20"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# handle verbose argument
	verbose <- verbose_argument_handling( verbose )

	# console output
	if( verbose >= 1 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# supported fit objects
	supported.fit.objects <- c( "lavaan" )

	# get class of fit object
	fit.class <- class( fit )

	# check if supported
	if( !any( supported.fit.objects %in% fit.class ) ) stop( fun.name.version, ": fit object of class ", fit.class, " not supported. Supported fit objects are: ", paste( supported.fit.objects, collapse=", " ) )

	# require package
	if( fit.class %in% "lavaan" ) require( lavaan )

	# model representation must be "LISREL"
	model.rep <- fit@Model@representation
	if( !model.rep %in% "LISREL" ) stop( fun.name.version, ": model representation as defined in fit@Model@representation must be LISREL, but it is ", paste( model.rep, collapse=", " ) )

	# check whether beta is present in fit object
	GLIST.names <- names( fit@Model@GLIST )
	if( !any( GLIST.names %in% "beta" ) ) stop( fun.name.version, ": fit@Model@GLIST does not contain beta, but only ", paste( GLIST.names, collapse=", " ) )

	# get beta matrix from fit object
	C <- fit@Model@GLIST$beta

	# check dimensions
	C.dim <- dim( C )
	C.dim.n <- length( C.dim )
	if( !C.dim.n ) stop( fun.name.version, ": number of dimensions is not 2, but ", C.dim.n )
	if( !( all( length( C.dim ) >= 1 ) & all( C.dim %in% C.dim[1] ) ) ) stop( fun.name.version, ": not all dimensions are of same length or at least one dimension is of length < 1: ", paste( C.dim, collapse=", " ) )

	# get dimension names
	beta.dimNames <- fit@Model@dimNames[ names( fit@Model@GLIST ) %in% "beta" ][[1]]

	# set dimension names
	rownames( C ) <- beta.dimNames[[1]]
	colnames( C ) <- beta.dimNames[[2]]

	# console output
	if( verbose >= 1 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return C matrix
	return( C )
}

# test/development
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/verbose_argument_handling.R" )
# load( "c:/Users/martin/Dropbox/68_causalSEM/91_zeug/fit.lcs2.Rdata" )
# build_C( fit, verbose=2 )
# build_C( fit )
