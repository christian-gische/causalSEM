## Changelog:
# MA 0.0.1 2021-09-09: initial programming


## Documentation
#' @title Adds derivatives of C and Psi to the internal list
#' @description Internal function that adds the partial derivative of the vectorized
#' C and Psi matrices with respect to the parameters.
#'    verbose argument is written into the internal list
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{build_C} returns the inputted internal_list. The slot "derivative" of
#' the C and Psi slot is populated with a numerical matrix consisting of zeros and ones.
#' @seealso \code{\link{build_Psi}}
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal


## Function definition
add_derivative <- function(internal_list){

  # function name
  fun.name <- "add_derivative"

  # function version
  fun.version <- "0.0.1 2021-09-09"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

  # vectorize matrices
  C_vec <- as.vector(internal_list$info_model$C$labels)
  Psi_vec <- as.vector(internal_list$info_model$Psi$labels)

  # prepare matrices for the partial derivatives
  C_deriv <- matrix(0, nrow = length(C_vec), ncol = internal_list$info_model$param$n_par_unique)
  Psi_deriv <- matrix(0, nrow = length(Psi_vec), ncol = internal_list$info_model$param$n_par_unique)
  colnames(C_deriv) <- colnames(Psi_deriv) <- internal_list$info_model$param$labels_par_unique

  # partial derivatives of C with respect to the parameters
  for (i in seq_len(internal_list$info_model$param$n_par_unique)) {
    C_deriv[C_vec == internal_list$info_model$param$labels_par_unique[i], i] <- 1
  }

  # partial derivatives of Psi with respect to the parameters
  for (i in seq_len(internal_list$info_model$param$n_par_unique)) {
    Psi_deriv[Psi_vec == internal_list$info_model$param$labels_par_unique[i], i] <- 1
  }

  # populate derivative slots of C and Psi
  internal_list$info_model$C$derivative <- C_deriv
  internal_list$info_model$C$derivative <- Psi_deriv

  # console output
  if(verbose >= 2) cat(paste0("  end of function ", fun.name.version, " ", Sys.time(), "\n"))

  # return internal list
  return(internal_list)
}
