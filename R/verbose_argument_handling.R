## Changelog:
# MH 0.0.2 2021-07-30: added documentation
# MH 0.0.1 2021-07-20: initial programming

## Documentation
#' @title Checks and potentially "repairs" the verbose argument
#' @description Internal function, checks whether the user-inputted verbose
#'    argument is valid
#' @param verbose A number, 0...no output (default), 1...user messages,
#'    2...debugging-relevant messages.
#' @return returns either the valid user input or 0 (no output) as default
#' @keywords internal

# Function definition
verbose_argument_handling <- function( verbose=c(0,1,2) ){

	# function name
	fun.name <- "verbose_argument_handling"

	# function version
	fun.version <- "0.0.2 2021-07-30"

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
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

	# return verbose object
	return( verbose )	
}

## test/development
# verbose_argument_handling()   # 0
# verbose_argument_handling(0)  # 0
# verbose_argument_handling(1)  # 1
# verbose_argument_handling(2)  # 2
# verbose_argument_handling(3)  # 0
# verbose_argument_handling(NA) # 0
# verbose_argument_handling(-1) # 0
