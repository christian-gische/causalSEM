## Changelog:
# CG 0.0.4 2023-02-20: changes to preamble to print documentation
#                      changed argument to verbose = NULL
# MH 0.0.3 2021-11-22: renamed from verbose_argument_handling to 
#                      handle_verbose_argument
# MH 0.0.2 2021-07-30: added documentation
# MH 0.0.1 2021-07-20: initial programming

## Documentation
#' @title Handling of Verbose Argument
#' @description Reads the user-inputted verbose
#'    argument checks if it is valid.
#' @param verbose A single number, integer. 
#' 0...no output (default); 
#' 1...user messages;
#' 2...debugging-relevant messages. 
#' @return Returns verbose setting.


# Function definition
handle_verbose_argument <- function( verbose = NULL ){

	# function name
	fun.name <- "handle_verbose_argument"

	# function version
	fun.version <- "0.0.4 2023-02-20"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# default
	verbose.default <- c(0,1,2) 
	# 0...no output, 1...user messages, 2...debugging-relevant messages

	# if not numeric, try to convert to numeric
	if( !class( verbose ) %in% class( verbose.default ) ) verbose <- 
	  suppressWarnings( try( as.numeric( verbose ) ) )
	if( inherits( verbose, "try-error" ) ) verbose <- verbose.default

	# if not yet numeric, set default
	if( !class( verbose ) %in% class( verbose.default ) ) verbose <- 
	  verbose.default

	# if numeric(0), set default
	if( identical( verbose, numeric(0) ) ) verbose <- verbose.default

	# if verbose contains not supported values, set default
	if( !any( verbose.default %in% verbose ) ) verbose <- verbose.default

	# if length of verbose is greater 1, set first value
	if( length( verbose ) > 1 ) verbose <- verbose.default[1]

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", 
	                                fun.name.version, " ", Sys.time(), "\n" ) )

	# return verbose object
	return( verbose )	
}

## test/development
# handle_verbose_argument()   # 0
# handle_verbose_argument(0)  # 0
# handle_verbose_argument(1)  # 1
# handle_verbose_argument(2)  # 2
# handle_verbose_argument(3)  # 0
# handle_verbose_argument(NA) # 0
# handle_verbose_argument(-1) # 0
