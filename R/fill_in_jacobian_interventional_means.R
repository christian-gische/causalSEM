## Changelog:
# CG 0.0.1 2023-03-30: initial programming

## Documentation
#' @title Fill in Jacobian for the Mean to List
#' @description Fills in the Jacobian of the mean vector of the interventional 
#' distribution into the internal list. See, for example, Theorem 9 and 
#' Corollaries 10 and 11 in Gische and Voelkle (2022).
#' @param internal_list A list with various information extracted from the 
#' model.
#' @return The inputted list with slots 
#' in \code{..$interventional_distribution$means} filled in.\cr
#' \tabular{lll}{
#' \tab   \code{..$jacobian} \tab The Jacobian matrix.} 
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
fill_in_jacobian_interventional_means <- function(internal_list = NULL){
  
  # function name
  fun_name <- "fill_in_jacobian_interventional_means"
  
  # function version
  fun_version <- "0.0.1 2023-03-30"
  
  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")
  
  # CG 0.0.3 2023-02-28: check if argument is of class causalSEM 
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
  
  # calculate jacobian
  jacobian <- calculate_jacobian_interventional_means(
    model = internal_list,
    use_model_values = TRUE
  )
  
  # fill in slots of ...$interventional_distribution$means
  internal_list$interventional_distribution$means$jacobian <- jacobian
  
  # console output
  if(verbose >= 2) cat( paste0("  end of function ", fun_name_version, " ",
                               Sys.time(), "\n" ))
  
  # return internal list
  internal_list
  
}
