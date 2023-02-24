## Changelog:
# CG 0.0.6 2022-01-13: changed structure of internal_list
#                      cleaned up code (documentation, 80 char per line)
#                      changed dot-case to snake-case
# CG 0.0.5 2021-11-24: changed $variance to $covariance in internal_list path
# CG 0.0.4 2021-11-23: Added TODO above
# MH 0.0.3 2021-10-31: test dummy se added
# MH 0.0.2 2021-10-14: functional update
# MH 0.0.1 2021-09-27: initial programming

## Documentation
#' @title Fill in Asymptotics for Density Function of Interventional 
#' Distributions
#' @description Fill in asymptotics for density function of interventional 
#' distributions. Asymptotic values are computed pointwise over a grid of 200 
#' values in the range [-3*SD, 3*SD] of the univariate outcome variable. The 
#' asysmptotic standard errors and the approximate z-values of the  
#' interventional density function for a specific interventional level are 
#' computed pointwise over the grid. 
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted internal_list with slot 
#' interventional_distribution$density_function
#' ..$ase
#' ..$z-values
#' filled in with a list (one named entry for each variable). 
#' Each list element in ..$ase contains a matrix with two columns and 200 rows:
#'    [1] "x": generated x values (-3*SD ... 3*SD) 
#'    [2] "ase": asymptotic standard errors
#' Each list element in ..$z_values contains a matrix with two columns and 200 
#' rows:
#'    [1] "x": generated x values (-3*SD ... 3*SD) 
#'    [2] "z_values": asymptotic standard errors
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_asymptotics_interventional_density <- function( internal_list ){

	# function name
	fun.name <- "fill_in_asymptotics_interventional_density"

	# function version
	fun.version <- "0.0.6 2022-01-13"

	# function name+version
	fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

	# get verbose argument
	verbose <- internal_list$control$verbose

	# console output
	if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", 
	                                Sys.time(), "\n" ) )

	# get number of observed variables and names of observed variables
	
	n <- internal_list$info_model$n_ov
	var_names <- internal_list$info_model$var_names

	l <- vector(mode = "list", length = n)
	names(l) <- var_names
	
	
	
	# fill list with the values of the pdfs for each non-interventional 
	# variable (grid of 200 values for each variable)
	
	for (j in 1 : n) {
	
	pdf <- internal_list$interventional_distribution$density_function$values[[j]]
	ase_col <-matrix( nrow = nrow(pdf) , ncol = 2 ) 
	colnames(ase_col) <- c("ase","z_value")
	pdf <- cbind(pdf, ase_col)
	l[[j]] <- pdf
	
	}
	
	# add the pointwise (over the grid of 200 values) ase and the (pointwise) 
	# z-values to the list for each non-interventional variable  
	  
	for (j in 1 : n) {
	for (i in 1:nrow(pdf)) {
	  
	  ase_pdf <- calculate_ase_interventional_density(
	    model = internal_list,
	    x = internal_list$info_interventions$intervention_levels,
	    y = l[[j]][i, "x"],				                                                       
	    intervention_names = internal_list$info_interventions$intervention_names, 
	    outcome_names = var_names[j] , 
	    verbose = internal_list$control$verbose)
	    
	  l[[j]][i, "ase"] <- ase_pdf$ase_gamma_3
	  l[[j]][i, "z_value"] <- ase_pdf$z_gamma_3
	  
	}
	}
	
	# reorganize the lists 
	
	slot_ase <- vector(mode = "list", length = n)
	names(slot_ase) <- var_names
	
	for (j in 1 : n) {
	
	slot_ase[[j]] <- l[[j]][ , c("x", "ase")]
	  
	}
	
	slot_z_values <- vector(mode = "list", length = n)
	names(slot_z_values) <- var_names
	
	for (j in 1 : n) {
	  
	  slot_z_values[[j]] <- l[[j]][ , c("x", "z_value")]
	  
	}

	# fill in slots
	
	internal_list$interventional_distribution$density_function$ase <- slot_ase
	  
	internal_list$interventional_distribution$density_function$z_values <-
	  slot_z_values 

	# console output
	if( verbose >= 2 ) cat( paste0( "  end of function ", fun.name.version, " ",
	                                Sys.time(), "\n" ) )

	# return internal list
	return( internal_list )
}


### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "interventional_density.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

## test object 00_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/00_lavaan_test_object.Rdata" ) )
# o00_internal_list <- intervention_effect( model=o00_lavaan_test_object,intervention="x2",intervention_levels=2)
# str( o00_internal_list$interventional_distribution$density$pdf )

## test object 01_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/01_lavaan_test_object.Rdata" ) )
# o01_internal_list <- intervention_effect( model=o01_lavaan_test_object,intervention="x2",intervention_levels=2)
# str( o01_internal_list$interventional_distribution$density$pdf )

## test object 02_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/02_lavaan_test_object.Rdata" ) )
# o02_internal_list <- intervention_effect( model=o02_lavaan_test_object,intervention="x2",intervention_levels=2)
# str( o02_internal_list$interventional_distribution$density$pdf )

## test object 03_lavaan_test_object
# load( file.path( shell( "echo %USERPROFILE%", intern=TRUE ), "Dropbox/causalSEM_R_Package/test_object/03_lavaan_test_object.Rdata" ) )
# o03_internal_list <- intervention_effect( model=o03_lavaan_test_object,intervention="x2",intervention_levels=2)
# str( o03_internal_list$interventional_distribution$density$pdf )


### test
# require( testthat )
# test_file("../tests/testthat/XXXXXXX.R")
