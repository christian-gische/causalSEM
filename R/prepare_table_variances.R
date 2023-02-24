## Changelog:
# MA 0.0.2 2022-03-15: added causal.var.type and inserted NAs for interventional
##                     variables
# MA 0.0.1 2022-02-17: initial programming

## Documentation
#' @title Prepare Table for Interventional Variances
#' @description Create a formatted table containing information 
#' about interventional variances. Used in \code{print.causalSEM} and 
#' \code{summary.causalSEM}.
#' @param x An object of class \code{causalSEM}.
#' @param digit Single number, integer. Indicating the number of decimal places
#' (round) or significant digits (signif) to be used.
#' @return A character vector with information about interventional variances.
#' @references Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
#' Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
#' 868–901. https://doi.org/10.1007/s11336-021-09811-z
#' @export


## Function definition
prepare_table_variances <- function(x, digits = 3){

  # function name
  fun_name <- "prepare_table_variances"

  # function version
  fun_version <- "0.0.2 2022-03-15"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(x$control$verbose >= 2) {
    cat(paste0("start of function ",fun_name_version, " ", Sys.time(), "\n" ))
  }

  # Determine causal variable type
  interventions <- rowSums(x$constant_matrices$select_intervention)

  causal_var_type <- ifelse(test = interventions == 1,
                            yes = "interv.",
                            no = "pre/post interv.")

  # data.frame with variances
  df_variances <- data.frame(
    x$tables$interventional_means$Variable,
    round(x$tables$interventional_variances[, 2:3], digits = digits),
    paste0(
      "[",
      round(x$tables$interventional_variances$CI_lower, digits = digits),
      ",",
      round(x$tables$interventional_variances$CI_upper, digits = digits),
      "]"),
    causal_var_type
  )

  # Add row with row names
  df_variances <- rbind(
    c("Variable", "Est.", "Std. Err.", "95% CI", "Causal. Var. Type"),
    df_variances
  )

  # Delete column names
  colnames(df_variances) <- ""

  # Insert NAs for interventions
  df_variances[df_variances[, 5] == "interv.", c(3, 4)] <- "NA"

  # Transform data.farme into strings
  df_variances <- utils::capture.output(print(df_variances, row.names = FALSE))
  df_variances <- df_variances[-1]

  # Return output
  df_variances

}
