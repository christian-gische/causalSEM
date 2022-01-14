## Changelog:
# MA 0.0.1 2022-01-13: initial programming

## Documentation
#' @title Create output table with interventional means and variances
#' @description Internal function that creates a table with the interventional
#' means and variances with corresponding standard errors and 95% confidence
#' intervals.
#' @param internal_list internal_list or object of class causalSEM
#' @return \code{fill_in_print_table} returns a list containing two tables with
#' the interventional means and variances.
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords internal

## Function definition
fill_in_print_table <- function(internal_list){

  # function name
  fun_name <- "fill_in_print_table"

  # function version
  fun_version <- "0.0.1 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(internal_list$control$verbose >= 2) cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n" ))

  # Functions to compute the lower and upper border of the 95% confidence
  # interval

  CI_lower_border <- function(mean, sd) {
    qnorm(p = 0.025, mean = mean, sd = sd)
  }

  CI_upper_border <- function(mean, sd) {
    qnorm(p = 0.975, mean = mean, sd = sd)
  }

# Data frame with the interventional means
  interventional_means <- data.frame(
    Variable = internal_list$info_model$var_names,
    "Est." = internal_list$interventional_distribution$moments$mean_vector,
    "Std. Err." = internal_list$interventional_distribution$ase$means,
    "CI_lower" = mapply(CI_lower_border,
                            internal_list$interventional_distribution$moments$mean_vector,
                            internal_list$interventional_distribution$ase$means),
    "CI_upper" = mapply(CI_upper_border,
                            internal_list$interventional_distribution$moments$mean_vector,
                            internal_list$interventional_distribution$ase$means)
  )

  # Data frame with the interventional means
  interventional_variances <- data.frame(
    Variable = internal_list$info_model$var_names,
    "Est." = diag(internal_list$interventional_distribution$moments$covariance_matrix),
    "Std. Err." = internal_list$interventional_distribution$ase$variances,
    "CI_lower" = mapply(CI_lower_border,
                        diag(internal_list$interventional_distribution$moments$covariance_matrix),
                        internal_list$interventional_distribution$ase$variances),
    "CI_upper" = mapply(CI_upper_border,
                        diag(internal_list$interventional_distribution$moments$covariance_matrix),
                        internal_list$interventional_distribution$ase$variances)
  )

  # Prepare output
  output <- list(interventional_means = interventional_means,
                 interventional_variances = interventional_variances)

  output

}
