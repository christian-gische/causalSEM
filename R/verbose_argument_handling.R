## Changelog:
# MH 0.0.1 2021-07-20: initial programming

# function definition
verbose_argument_handling <- function( verbose ){

	# function name
	fun.name <- "verbose_argument_handling"

	# function version
	fun.version <- "0.0.1 2021-07-20"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# default
	verbose.default <- c(0,1,2) # 0...no output, 1...user messages, 2...debugging-relevant messages

	# if not numeric, try to convert to numeric
	if( !class( verbose ) %in% class( verbose.default ) ) verbose <- suppressWarnings( try( as.numeric( verbose ) ) )
	if( inherits( verbose, "try-error" ) ) verbose <- verbose.default

	# if not yet numeric, set default
	if( !class( verbose ) %in% class( verbose.default ) ) verbose <- verbose.default

	# if numeric(0), set default
	if( identical( verbose, numeric(0) ) ) verbose <- verbose.default

	# if verbose contains not supported values, set default
	if( !any( verbose.default %in% verbose ) ) verbose <- verbose.default

	# if length of verbose is greater 1, set first value
	if( length( verbose ) > 1 ) verbose <- verbose.default[1]

	# console output
	if( verbose >= 1 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return verbose object
	return( verbose )	
}
