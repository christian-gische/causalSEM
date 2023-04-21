## Changelog:
# MH 0.0.3 2022-03-17: removed "seealso", solves NOTE in package checking
# CG 0.0.2 2022-01-13:  changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.1 2021-11-22: initial programming

## Documentation
#' @title Fill in Asymptotics for the Variances of the Interventional 
#' Distribution
#' @description Fills in the Jacobian, the asymptotic
#' covariance matrix, the asymptotic standard errors, and asymptotic z-values 
#' of the variances of the interventional distribution into the internal list.
#' @param internal_list A list with various information extracted from the 
#' model.
#' @return The inputted internal_list with several slots in 
#' ..$interventional_distribution filled in.
#'    ..$jacobian
#'    ..$acov
#'    ..$ase
#'    ..$z_value
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_asymptotics_interventional_variances <- function(internal_list){

  # function name
  fun_name <- "fill_in_asymptotics_interventional_variances"

  # function version
  fun_version <- "0.0.3 2022-03-17"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")
  
  # check function arguments 
  ## get class of model object
  model_class <- class(internal_list)
  
  ## set supported classes of model objects
  supported_model_classes <- c( "causalSEM" )
  
  ## check if argument model is supported
  if(!any(model_class %in% supported_model_classes)) stop(
    paste0(
      fun.name.version, ": model of class ", model_class,
      " not supported. Supported fit objects are: ",
      paste(supported_model_classes, collapse = ", ")
    )
  )

  # get verbose argument
  verbose <- internal_list$control$verbose
  
  

  # console output
  if(verbose >= 2) cat(paste0( "start of function ", fun_name_version, " ",
                               Sys.time(), "\n" ))

  # calculate asymptotics 
  ase <- calculate_ase_interventional_variances(model = internal_list,
                                                use_model_values = TRUE)

  # fill in slots of ...$interventional_distribution
  internal_list$interventional_distribution$covariance_matrix$acov <- 
    ase$acov_gamma_2
  internal_list$interventional_distribution$covariance_matrix$ase <- 
    ase$ase_gamma_2
  internal_list$interventional_distribution$covariance_matrix$z_values <- 
    ase$z_gamma_2

  # console output
  if(verbose >= 2) cat( paste0("  end of function ", fun_name_version, " ",
                               Sys.time(), "\n" ))

  # return internal list
  internal_list

}
