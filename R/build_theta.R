## Changelog:
# CG 0.0.3 2021-09-09: updated comments; added comment of checks
# CG 0.0.2 2021-09-02: updated code; roxygen documentation added
# CG 0.0.1 2021-07-29: initial programming


## Documentation
#' @title Extracts the number, the numeric values, and the labels
#' of distinct and functionally unrelated SEM model parameters
#' @description Internal function that extracts the numeric values and
#'   labels of distinct and functionally unrelated parameters from a
#'   fitted structural equation model (i.e., all uniquely labelled
#'   coefficients in C and Psi matrix. Duplicates due to symmetry or
#'   equality constraints are removed.). The entries in the resulting
#'   vectors follow the following predefined order:
#'   First: structural coefficients from the C matrix are added rowwise.
#'   Second: variance covariance parameters from the Psi matrix are added rowwise.
#'   Supported fitted objects: lavaan.
#' @param internal_list A list with various information extracted from the
#'    model.
#' @return \code{build_theta} returns the inputted internal_list with slot
#'    param populated with a five-entry list:
#'    "n_par" is an integer indicating the total number of estimated parameters
#'     (potential duplicates due to symmetry or equality constraints are counted).
#'    "n_par_unique" is an integer indicating the number of distinct and functionally
#'     unrelated parameters.
#'    "labels_par_unique" is a character vector containing the labels of distinct
#'     and functionally unrelated parameters.
#'    "values_par_unique" is a numeric vector containing the parameter values
#'     (estimates) of distinct and functionally unrelated parameters.
#'    "varcov_par_unique" is a numeric matrix containing equal to the variance-covariance
#'      matrix of the estimator of distinct and functionally unrelated parameters.
#' @seealso \code{\link{build_Psi, build_C}}
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

# function definition
build_theta <- function( internal_list ){

  # function name
  fun.name <- "build_theta"

  # function version
  fun.version <- "0.0.2 2021-09-02"

  # function name+version
  fun.name.version <- paste0( fun.name, " (", fun.version, ")" )

  # get verbose argument
  verbose <- internal_list$control$verbose

  # console output
  if( verbose >= 2 ) cat( paste0( "start of function ", fun.name.version, " ", Sys.time(), "\n" ) )

  # get fit object from internal_list
  fit <- internal_list$fitted_object

  # get class of fit object
  fit.class <- internal_list$fitted_object_class

  # supported fit objects
  supported.fit.objects <- c( "lavaan" )

  # check if supported
  if( !any( supported.fit.objects %in% fit.class ) ) stop( paste0( fun.name.version, ": fit object of class ", fit.class, " not supported. Supported fit objects are: ", paste( supported.fit.objects, collapse=", " ) ) )

  # require package
  if( fit.class %in% "lavaan" ) require( lavaan )

  # model representation must be "LISREL"
  model.rep <- fit@Model@representation
  if( !model.rep %in% "LISREL" ) stop( paste0( fun.name.version, ": model representation as defined in fit@Model@representation must be LISREL, but it is ", paste( model.rep, collapse=", " ) ) )

  # check whether beta is present in fit object
  GLIST.names <- names( fit@Model@GLIST )
  if( !any( GLIST.names %in% "beta" ) ) stop( paste0( fun.name.version, ": fit@Model@GLIST does not contain beta, but only ", paste( GLIST.names, collapse=", " ) ) )

  # get vector all estimated parameters in a predefined order.
  # (might contain duplicates due to symmetry or equality constraints).

  # structural coefficients from the C matrix (rowwise)
  coef_c<-as.vector(t(lavInspect(internal_list$fitted_object, what = "free")$beta))
  coef_c<-coef_c[coef_c != 0]

  # variance covariance parameters from the Psi matrix (rowwise)

  coef_psi<-as.vector(t(lavInspect(internal_list$fitted_object, what = "free")$psi))
  coef_psi<-coef_psi[coef_psi != 0]

  # create joint parameter vector

  coef_joint<-c(coef_c,coef_psi)

  # get parameter labels of all parameters in C and Psi

  par_names <- names(coef(internal_list$fitted_object))[coef_joint]

  # get parameter labels of uniquely labelled parameters in C and Psi
  # (remove duplicates due to symmetry or equality constraints)

  labels_par_unique <- unique(par_names)

  # get total number of parameters C and Psi

  n_par <- length(par_names)

  # get number of uniquely labelled parameters in C and Psi
  # (remove duplicates due to symmetry or equality constraints)

  n_par_unique <- length(labels_par_unique)

  # get numeric values of uniquely labelled parameters in C and Psi
  # (remove duplicates due to symmetry or equality constratins)

  values_par_unique <- coef(internal_list$fitted_object)[labels_par_unique]

  # todo: CHECK if result is identical to reading the slots
  # internal_list$info_model$C and internal_list$info_model$Psi, respectively

  # get variance covariance matrices of estimator of uniquely labelled
  # parameters in C and Psi

  varcov_par_unique <- vcov(internal_list$fitted_object)
  varcov_par_unique_r <- varcov_par_unique[labels_par_unique, ]
  varcov_par_unique <- varcov_par_unique_r[, labels_par_unique]

  # todo: DO DIMENSION CHECK

  dim(varcov_par_unique) == c(n_par_unique, n_par_unique)

  # create list

  theta.list <- list(
    n_par = n_par, # total number of estimated parameters
    n_par_unique = n_par_unique, # number of distinct and functionally unrelated parameters
    labels_par_unique = labels_par_unique, # names of distinct and functionally unrelated parameters
    values_par_unique = values_par_unique, # parameter values (estimates) of distinct and functionally unrelated parameters
    varcov_par_unique = varcov_par_unique # variance-covariance matrix of the estimator of distinct and functionally unrelated parameters
  )

  # populate slot param of internal_list
  internal_list$info_model$param <- theta.list

  # console output
  if( verbose >= 1 ) cat( paste0( "  end of function ", fun.name.version, " ", Sys.time(), "\n" ) )

  # return internal_list
  return( internal_list )
}

