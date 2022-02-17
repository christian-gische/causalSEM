## Changelog:
# MA 0.0.1 2022-02-17: initial programming

## Documentation
#' @title Extract Causal Coefficients
#' @description coef method for class "causalSEM".
#' @param x an object of class "causalSEM", usually, a result of a call to intervention_effect.
#' @param what string indicating what type of coefficients are to be returned.
#' 'means' returns interventional means, 'var' returns interventional variances,
#' and 'varcov' returns interventional variances and covariances.
#' @return \code{coef.causalSEM} numeric vector.
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @export

## Function definition
coef.causalSEM <- function(x, what = "means"){

  # function name
  fun_name <- "coef.causalSEM"

  # function version
  fun_version <- "0.0.1 2022-02-17"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(internal_list$control$verbose >= 2) {
    cat(paste0("start of function ",fun_name_version, " ", Sys.time(), "\n" ))
  }

  # return means
  if (what == "means") {
    means <- as.vector(x$interventional_distribution$means$values)
    names(means) <- x$info_model$var_names
    return(means)
  } else if (what == "var") { # return variances
    vars <- diag(x$interventional_distribution$covariance_matrix$values)
    return(vars)
  } else if (what == "varcov") { # return variances and covariances
    varcov <- rep(NA, times = x$info_model$n_ov * (x$info_model$n_ov - 1) / 2)
    counter <- 1
    for (i in 1:x$info_model$n_ov) {
      for (j in 1:i) {
        varcov[counter] <- x$interventional_distribution$covariance_matrix$values[i, j]
        names(varcov)[counter] <- paste0(x$info_model$var_names[i], x$info_model$var_names[j])
        counter <- counter + 1
      }
    }
    return(varcov)
  } else {
    stop("Unknwon argument. Use 'means', 'var', or 'varcov'.")
  }

}
