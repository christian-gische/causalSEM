## Changelog:
# CG 0.0.4 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# 
# CG 0.0.3 2021-11-11: initial programming by 
# splitting previous function add_derivative into two parts
# CG 0.0.2 2021-09-09: corrected copy paste typo 
# MA 0.0.1 2021-09-09: initial programming


## Documentation
#' @title Calculate Jacobian of Matrix of Structural Coefficients
#' @description Calculates the Jacobian of the vectorized matrix of structural 
#' coefficients (partial derivative with respect to the parameters vector theta)
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return The inputted internal_list with the slot "derivative" of
#' the ..$C slot containing the vectorized Jacobian of the C matrix.
#' @seealso \code{\link{calculate_jacobian_Psi}}
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal


## Function definition
calculate_jacobian_C <- function(internal_list){
  
  # function name
  fun.name <- "calculate_jacobian_C"
  
  # function version
  fun.version <- "0.0.4 2022-01-13"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
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
