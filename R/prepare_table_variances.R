## Changelog:
# MA 0.0.1 2022-02-17: initial programming

## Documentation
#' @title Prepare Tables with Information About Interventional Variances
#' @description Internal helper function that creates a formatted table
#' containing various information about the interventional variances. Used in
#' print.causalSEM and summary.causalSEM
#' @param x an object of class "causalSEM", usually, a result of a call to
#' intervention_effect.
#' @param digit integer indicating the number of decimal places (round) or
#' significant digits (signif) to be used.
#' @return \code{prepare_table_variances} returns a character vector.
#' @references
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z


## Function definition
prepare_table_variances <- function(x, digits = 3){

  # function name
  fun_name <- "prepare_table_variances"

  # function version
  fun_version <- "0.0.1 2022-02-17"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(x$control$verbose >= 2) {
    cat(paste0("start of function ",fun_name_version, " ", Sys.time(), "\n" ))
  }

  # data.frame with variances
  df_variances <- data.frame(
    x$tables$interventional_means$Variable,
    round(x$tables$interventional_variances[, 2:3], digits = digits),
    paste0(
      "[",
      round(x$tables$interventional_variances$CI_lower, digits = digits),
      ",",
      round(x$tables$interventional_variances$CI_upper, digits = digits),
      "]")
  )

  # Add row with row names
  df_variances <- rbind(
    c("Variable", "Est.", "Std. Err.", "95% CI"),
    df_variances
  )

  # Delete column names
  colnames(df_variances) <- ""

  # Transform data.farme into strings
  df_variances <- utils::capture.output(print(df_variances, row.names = FALSE))
  df_variances <- df_variances[-1]

  # Return output
  df_variances

}
