## Changelog:
# CG 0.0.5 2023-02-28: check if argument is of class causalSEM
# CG 0.0.4 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# 
# CG 0.0.3 2021-11-11: initial programming by 
# splitting previous function add_derivative into two parts
# CG 0.0.2 2021-09-09: corrected copy paste typo 
# MA 0.0.1 2021-09-09: initial programming


## Documentation
#' @title Calculate Jacobian of C-Matrix
#' @description Calculates the Jacobian of the vectorized matrix of structural 
#' coefficients (C-matrix). The Jacobian is defined as the partial derivative 
#' with respect to the parameters vector theta of distinct and functionally 
#' unrelated parameters.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted list with the slot \code{..$derivative}
#'  in the sublist \code{..$C} filled in:
#' \tabular{lll}{\code{..$derivative}:\tab \tab Numeric matrix containing 
#'      the Jacobian of the vectorized C matrix.}
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z


## Function definition
calculate_jacobian_C <- function(internal_list = NULL){
  
  # function name
  fun.name <- "calculate_jacobian_C"
  
  # function version
  fun.version <- "0.0.5 2023-02-28"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # CG 0.0.5 2023-02-28: check if argument is of class causalSEM 
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
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ",
                                  Sys.time(), "\n" ) )
  
  # vectorize matrices
  C_vec <- as.vector(internal_list$info_model$C$labels)
  
  # prepare matrices for the partial derivatives
  C_deriv <- matrix(0, nrow = length(C_vec), 
                    ncol = internal_list$info_model$param$n_par_unique)
  colnames(C_deriv) <- internal_list$info_model$param$labels_par_unique
  
  # partial derivatives of C with respect to the parameters
  for (i in seq_len(internal_list$info_model$param$n_par_unique)) {
    C_deriv[C_vec == internal_list$info_model$param$labels_par_unique[i],
            i] <- 1
  }
  
  # populate derivative slots of C 
  # CG 0.0.2: changed slot from ...$C$... to ...$Psi... in <- Psi_deriv 
  # assignment
  internal_list$info_model$C$derivative <- C_deriv
  
  # console output
  if(verbose >= 2) cat(paste0("  end of function ", fun.name.version, " ",
                              Sys.time(), "\n"))
  
  # return internal list
  return(internal_list)
}
