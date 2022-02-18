## Changelog:
# MA 0.0.2 2022-02-08: changed formatting
# MA 0.0.1 2022-01-13: initial programming

## Documentation
#' @title Printing a Causal SEM Object
#' @description print method for class "causalSEM".
#' @param x an object of class "causalSEM", usually, a result of a call to intervention_effect.
#' @param digit integer indicating the number of decimal places (round) or
#' significant digits (signif) to be used.
#' @return \code{print.causalSEM} returns a table with summary statistics.
#' @references
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @export

## Function definition
print.causalSEM <- function(x, digits = 3){

  # function name
  fun_name <- "print.causalSEM"

  # function version
  fun_version <- "0.0.2 2022-02-08"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(x$control$verbose >= 2) {
    cat(paste0("start of function ",fun_name_version, " ", Sys.time(), "\n" ))
  }

  # Prepare first line with names and levels of the interventional variables
  info_variables <- c()
  for (i in 1:x$info_interventions$n_intervention) {
    info_variables[length(info_variables) + 1] <- x$info_interventions$intervention_name[i]
    info_variables[length(info_variables) + 1] <- "="
    info_variables[length(info_variables) + 1] <- x$info_interventions$intervention_level[i]
    if (i < x$info_interventions$n_intervention) {
      info_variables[length(info_variables) + 1] <- ", "
    }
  }
  info_variables <- paste0(info_variables, collapse = "")


  # Collapse infomation about the interventional variables into a single string
  info_variables <- paste0("Intervention: do(", info_variables, ")", collapse = "")


  # data.frame with means
  df_means <- data.frame(
    x$tables$interventional_means$Variable,
    round(x$tables$interventional_means[, 2:3], digits = digits),
    paste0(
      "[",
      round(x$tables$interventional_means$CI_lower, digits = digits),
      ",",
      round(x$tables$interventional_means$CI_upper, digits = digits),
      "]"))

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
  df_means <- rbind(
    c("Variable", "Est.", "Std. Err.", "95% CI"),
    df_means
  )

  df_variances <- rbind(
    c("Variable", "Est.", "Std. Err.", "95% CI"),
    df_variances
  )

  # Delete column names
  colnames(df_means) <- colnames(df_variances) <- ""

  # Transform data.farme into strings
  df_means <- utils::capture.output(print(df_means, row.names = FALSE))
  df_means <- df_means[-1]

  df_variances <- utils::capture.output(print(df_variances, row.names = FALSE))
  df_variances <- df_variances[-1]

  # Put everything together
  output <- c(info_variables,
              "",
              "Interventional means:",
              df_means,
              "",
              "Interventional variances:",
              df_variances)

  # Write table
  writeLines(output)

}

