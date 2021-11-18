## Changelog:
# CG 0.0.3 2021-11-11: initial programming by 
#                      splitting previous function add_derivative into two parts
# CG 0.0.2 2021-09-09: corrected copy paste typo 
# MA 0.0.1 2021-09-09: initial programming


## Documentation
#' @title Adds Jacobian of Psi with respect to parameter vector theta to the internal list
#' @description Internal function that adds the partial derivative of the vectorized
#' Psi matrix with respect to the parameters vector theta.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{calculate_jacobian_Psi} returns the inputted internal_list. The slot "derivative" of
#' the Psi slot is populated with a numerical matrix consisting of zeros and ones.
#' @seealso \code{\link{calculate_jacobian_Psi}}
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal


## Function definition
calculate_jacobian_Psi <- function(internal_list){
  
  # function name
  fun.name <- "calculate_jacobian_Psi"
  
  # function version
  fun.version <- "0.0.3 2021-11-11"
  
  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )
  
  # get verbose argument
  verbose <- internal_list$control$verbose
  
  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )
  
  # vectorize matrices
  Psi_vec <- as.vector(internal_list$info_model$Psi$labels)
  
  # prepare matrices for the partial derivatives
  Psi_deriv <- matrix(0, nrow = length(Psi_vec), ncol = internal_list$info_model$param$n_par_unique)
  colnames(Psi_deriv) <- internal_list$info_model$param$labels_par_unique
  
  # partial derivatives of Psi with respect to the parameters
  for (i in seq_len(internal_list$info_model$param$n_par_unique)) {
    Psi_deriv[Psi_vec == internal_list$info_model$param$labels_par_unique[i], i] <- 1
  }
  
  # populate derivative slots of C and Psi
  # CG 0.0.2: changed slot from ...$C$... to ...$Psi... in <- Psi_deriv assignment
  internal_list$info_model$Psi$derivative <- Psi_deriv
  
  # console output
  if(verbose >= 2) cat(paste0("  end of function ", fun.name.version, " ", Sys.time(), "\n"))
  
  # return internal list
  return(internal_list)
}
