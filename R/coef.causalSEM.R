## Changelog:
# MA 0.0.2 2022-03-09: added varcov_matrix option
# MA 0.0.1 2022-02-17: initial programming

## Documentation
#' @title Extract Causal Coefficients
#' @description coef method for class "causalSEM".
#' @param x an object of class "causalSEM", usually, a result of a call to intervention_effect.
#' @param what string indicating what type of coefficients are to be returned.
#' 'means' returns interventional means, 'var' returns interventional variances,
#' and 'varcov' returns interventional variances and covariances as numeric
#' vectors. 'varcov_matrix' returns interventional variances and covariances
#' as a matrix.
#' @return \code{coef.causalSEM} numeric vector or matrix.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @export

## Function definition
coef.causalSEM <- function(x, what = "means"){

  # function name
  fun_name <- "coef.causalSEM"

  # function version
  fun_version <- "0.0.2 2022-03-09"

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
  } else if (what == "varcov_matrix") { # return variance-covariance matrix
    varcov <- x$interventional_distribution$covariance_matrix$values
    return(varcov)
  } else {
    stop("Unknwon argument. Use 'means', 'var', 'varcov', or 'varcov_matrix'.")
  }

}
