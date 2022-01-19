## Changelog:
# CG 0.0.2 2022-01-13: changed structure of internal_list
#                       cleaned up code (documentation, 80 char per line)
#                       changed dot-case to snake-case
# MA 0.0.1 2022-01-13:  initial programming

## Documentation
#' @title Create output table with interventional means and variances
#' @description Internal function that creates a table with the interventional
#' means and variances with corresponding standard errors and 95% confidence
#' intervals.
#' @param internal_list internal_list or object of class causalSEM
#' @return \code{fill_in_print_table} returns a list containing two tables with
#' the interventional means and variances.
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @keywords internal

## Function definition
fill_in_print_table <- function(internal_list){

  # function name
  fun_name <- "fill_in_print_table"

  # function version
  fun_version <- "0.0.2 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(internal_list$control$verbose >= 2) cat(
    paste0("start of function ", fun_name_version, " ", Sys.time(), "\n" ))

  # Functions to compute the lower and upper border of the 95% confidence
  # interval

  CI_lower_border <- function(mean, sd) {
    qnorm(p = 0.025, mean = mean, sd = sd)
  }

  CI_upper_border <- function(mean, sd) {
    qnorm(p = 0.975, mean = mean, sd = sd)
  }

# Data frame with the interventional means
# CG 0.0.2: changed path in internal list 
  interventional_means <- data.frame(
    Variable = internal_list$info_model$var_names,
    "Est." = internal_list$interventional_distribution$means$values,
    "Std. Err." = internal_list$interventional_distribution$means$ase,
    "CI_lower" = mapply(CI_lower_border,
                        internal_list$interventional_distribution$means$values,
                        internal_list$interventional_distribution$means$ase),
    "CI_upper" = mapply(CI_upper_border,
                        internal_list$interventional_distribution$means$values,
                        internal_list$interventional_distribution$means$ase)
  )

  # Data frame with the interventional variances
  # CG 0.0.2: changed path in internal list 
  interventional_variances <- data.frame(
    Variable = internal_list$info_model$var_names,
    "Est." = diag(
      internal_list$interventional_distribution$covariance_matrix$values),
    "Std. Err." = 
      internal_list$interventional_distribution$covariance_matrix$ase,
    "CI_lower" = 
     mapply(CI_lower_border,
            diag(
            internal_list$interventional_distribution$covariance_matrix$values),
            internal_list$interventional_distribution$covariance_matrix$ase),
    "CI_upper" = 
    mapply(CI_upper_border,
           diag(
           internal_list$interventional_distribution$covariance_matrix$values),
           internal_list$interventional_distribution$covariance_matrix$ase)
  )

  # Prepare output
  output <- list(interventional_means = interventional_means,
                 interventional_variances = interventional_variances)

  output

}
