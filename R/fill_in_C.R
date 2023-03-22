## Changelog:
# CG 0.0.11 2023-03-22: removed check if representation is of type LISREL
# MH 0.0.10 2022-03-17: removed "require", solves NOTE in package checking
# CG 0.0.9 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MH 0.0.8 2021-11-22: renamed build_C to fill_in_C
# CG 0.0.7 2021-11-18: changed name of called funtion to 
#                      add_labels_in_lavaan_parTable
# MH 0.0.6 2021-09-21: "development" section updated
# MA 0.0.5 2021-09-09:
#    -- fill_in_C only alters the 'values' and 'labels' slots without 
#       overwriting the complete 'C' list
# MH 0.0.4 2021-09-08:
#    -- lav_parTable_fill_labels integrated
#    -- reduction of C matrix for 1:1 mapped models
#    -- tests with package testthat added: ..\tests\testthat\test_fill_in_C.R
# MH 0.0.3 2021-09-01: C list is now correctly inputted into internal list
# MH 0.0.2 2021-07-30:
#    -- added documentation
#    -- changed input and output argument to internal_list
# MH 0.0.1 2021-07-20: initial programming

## Documentation
#' @title Extract Matrix of Structural Coefficients 
#' @description Extracts the structural coefficient matrix from a fitted
#'    structural equation model. Supported objects types: lavaan.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted internal_list with slots in the sublist 
#'    ..$C filled in:
#' \tabular{lll}{
#'      .. ..$values: num[0, 0]   \tab \tab numeric matrix containing 
#'      parameter values of matrix of structural coefficients\cr
#'      .. ..$labels: chr[0, 0]   \tab \tab character matrix containing 
#'      parameter labels of matrix of structural coefficients\cr
#'      \tab \tab (NA for unlabeled parameters, NULL if labels can not 
#'      be extracted.)}
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_C <- function( internal_list ){

	# function name
	fun.name <- "fill_in_C"

	# function version
	fun.version <- "0.0.10 2022-03-17"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", 
	                                Sys.time(), "\n" ) )

	# get fit object from internal_list
	fit <- internal_list$fitted_object

	# get class of fit object
	fit.class <- internal_list$fitted_object_class

	# supported fit objects
	supported.fit.objects <- c( "lavaan" )

	# check if supported
	if( !any( supported.fit.objects %in% fit.class ) ) 
	  stop( paste0( fun.name.version, ": fit object of class ", fit.class, " not 
	                supported. Supported fit objects are: ",
	                paste( supported.fit.objects, collapse=", " ) ) )

	# require package
	# if( fit.class %in% "lavaan" ) requireNamespace( "lavaan" )
	
	# CG 0.0.11 2023-03-22: removed check if representation is of type LISREL
	# the LISREL check is redundant, since the input to the fill_in_C function is 
	# the internal list and it has already been checked by the fill_in_model_info
	# function

	# model representation must be "LISREL"
	# model.rep <- fit@Model@representation
	# if( !model.rep %in% "LISREL" ) 
	#  stop( paste0( fun.name.version, ": model representation as defined in 
	#                fit@Model@representation must be LISREL, 
	#                but it is ", paste( model.rep, collapse=", " ) ) )

	# check whether beta is present in fit object
	GLIST.names <- names( fit@Model@GLIST )
	if( !any( GLIST.names %in% "beta" ) ) 
	  stop( paste0( fun.name.version, ": fit@Model@GLIST does not contain beta, 
	                but only ", paste( GLIST.names, collapse=", " ) ) )

	# get beta matrix from fit object
	C <- fit@Model@GLIST$beta

	# check dimensions
	C.dim <- dim( C )
	C.dim.n <- length( C.dim )
	if( !C.dim.n ) 
	  stop( fun.name.version, ": number of dimensions is not 2, but ", C.dim.n )
	if( !( all( length( C.dim ) >= 1 ) & all( C.dim %in% C.dim[1] ) ) ) 
	  stop( paste0( fun.name.version, ": not all dimensions are of same length 
	                or at least one dimension is of length < 1:", 
	                paste( C.dim, collapse=", " ) ) )

	# get dimension names
	beta.dimNames <- 
	  fit@Model@dimNames[ names( fit@Model@GLIST ) %in% "beta" ][[1]]

	# set dimension names
	rownames( C ) <- beta.dimNames[[1]]
	colnames( C ) <- beta.dimNames[[2]]

	## labels of parameters in C matrix
	# initialization of C.lab
	C.lab <- NULL

	# if variable labels exist (row/colnames of C) then try to extract 
	# parameter labels from partab
	if( !is.null( rownames( C ) ) & !is.null( colnames( C ) ) ){

		# empty matrix (consistent with C)
		C.lab <- C
		C.lab[] <- as.character(NA)

		# parameter table
		# partab <- parTable( fit )
		# MH 0.0.4 2021-09-08 call of lav_parTable_fill_labels
		# CG 0.0.7 2021-11-18: changed name of called function 
		# to add_labels_in_lavaan_parTable 
		partab <- add_labels_in_lavaan_parTable( internal_list )

		# loop over elements of C.lab matrix
		for ( r in 1:nrow( C.lab ) ){
			for ( s in 1:ncol( C.lab ) ){
				# dependent variable
				lhs <- rownames( C.lab )[r]
				# predictor variable
				rhs <- colnames( C.lab )[s]
				# get label from partab
				lab <- partab[ partab$lhs %in% lhs & 
				                 partab$op %in% "~" & partab$rhs %in% rhs, "label" ]
				# if not available or empty string, then NA
				if( length( lab ) == 0 | identical( lab, "" ) ) lab <- NA
				# set label
				C.lab[r,s] <- lab
			}
		}

		## MH 0.0.4 2021-09-08
		# C matrix of models with pseudo measurement model (1:1 mapping of manifest 
		# variables onto latent variables)
		# is reduced to manifest variables
		# test object o00_lavaan_test_object

		# if current C matrix is twice the size n_ov * n_ov
		if( all( dim(C) == 2 * rep( internal_list$info_model$n_ov, 2 ) ) ){

			# console output
			if( verbose >= 2 ) cat( paste0( fun.name.version, " ", Sys.time(), 
			                                ": trying to reduce C matrix to observed 
			                                variables", "\n" ) )

			# C matrix of observed/non-observed
			# (observed / latent probably must be blockwise arranged and ordered)
			C.onov <- C[ internal_list$info_model$var_names , 
			             !colnames( C ) %in% internal_list$info_model$var_names ]

			# C matrix of non-observed
			C.nov <- C[ !rownames( C ) %in% internal_list$info_model$var_names ,
			            !colnames( C ) %in% internal_list$info_model$var_names ]

			# C matrix of non-observed/observed
			C.noov <- C[ !rownames( C ) %in% internal_list$info_model$var_names ,
			             internal_list$info_model$var_names ]

			# Checks
			checks <- rep( as.logical(NA), 3 )
			# C.onov must be identity matrix
			checks[1] <- identical( unname( C.onov ), diag( dim( C.onov )[1] ) )
			# C.nov must be null matrix
			checks[2] <- identical( unname( C.nov ), array( 0, dim=dim( C.nov ) ) )
			# C.noov must be null matrix
			checks[3] <- identical( unname( C.noov ), array( 0, dim=dim( C.noov ) ) )

			# if all checks are true, reduce C and C.lab
			if( all( checks ) ){
				C <- C[ internal_list$info_model$var_names, 
				        internal_list$info_model$var_names ]
				C.lab <- C.lab[ rownames(C), colnames(C) ]
				# console output
				if( verbose >= 2 ) cat( 
				  paste0( fun.name.version, " ", Sys.time(), ": 
				          reduction of C matrix to observed variables successful, 
				          new dimensions of C: ", paste(dim(C),collapse=" "), "\n" ) )
			} else {
				# console output
				if( verbose >= 2 ) cat( 
				  paste0( fun.name.version, " ", Sys.time(), ": reduction of C matrix 
				          to observed variables failed, checks: ",
				          paste(as.character(checks),collapse=" "), "\n" ) )
			}
		}

	} else {
		# console output
		if( verbose >= 2 ) cat( 
		  paste0( fun.name.version, " ", Sys.time(), 
		          ": C has no row and/or column variable names, 
		          extraction of parameter labels not possible", "\n" ) )
	}

	# populate slots of C
	internal_list$info_model$C$values <- C
	internal_list$info_model$C$labels <- C.lab

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}

### development
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/handle_verbose_argument.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/create_empty_list.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/fill_in_info_model.R" )
# source( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R/add_labels_in_lavaan_parTable.R" )

## test object 00_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
# o00_internal_list <- create_empty_list()
# o00_internal_list <- fill_in_info_model( o00_internal_list, o00_lavaan_test_object )
# o00_internal_list <- fill_in_C( o00_internal_list )
# o00_internal_list$info_model$C

## test object 01_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
# o01_internal_list <- create_empty_list()
# o01_internal_list <- fill_in_info_model( o01_internal_list, o01_lavaan_test_object )
# o01_internal_list <- fill_in_C( o01_internal_list )
# o01_internal_list$info_model$C

## test object 02_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
# o02_internal_list <- create_empty_list()
# o02_internal_list <- fill_in_info_model( o02_internal_list, o02_lavaan_test_object )
# o02_internal_list <- fill_in_C( o02_internal_list )
# o02_internal_list$info_model$C

## test object 03_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
# o03_internal_list <- create_empty_list()
# o03_internal_list <- fill_in_info_model( o03_internal_list, o03_lavaan_test_object )
# o03_internal_list <- fill_in_C( o03_internal_list )
# o03_internal_list$info_model$C

### test
# require( testthat )
# test_file("../tests/testthat/test_fill_in_C.R")
