## Changelog:
# MA 0.0.1 2022-01-13: initial programming

## Documentation
#' @title Printing a Causal SEM Object
#' @description print method for class "causalSEM".
#' @param internal_list internal_list or object of class causalSEM
#' @param digit integer indicating the number of decimal places (round) or
#' significant digits (signif) to be used.
#' @return \code{print.causalSEM} returns a table with summary statistics.
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}
#' @keywords
#' @export

## Function definition
print.causalSEM <- function(internal_list, digits = 3){

  # function name
  fun_name <- "print.causalSEM"

  # function version
  fun_version <- "0.0.1 2022-01-13"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(internal_list$control$verbose >= 2) {
    cat(paste0("start of function ",fun_name_version, " ", Sys.time(), "\n" ))
  }

  # Prepare first line with names and levels of the interventional variables
  info_variables <- c()
  for (i in 1:internal_list$info_interventions$n_intervention) {
    info_variables[length(info_variables) + 1] <- internal_list$info_interventions$intervention_name[i]
    info_variables[length(info_variables) + 1] <- "="
    info_variables[length(info_variables) + 1] <- internal_list$info_interventions$intervention_level[i]
    if (i < internal_list$info_interventions$n_intervention) {
      info_variables[length(info_variables) + 1] <- ", "
    }
  }
  info_variables <- paste0(info_variables, collapse = "")


  # Collapse infomation about the interventional variables into a single string
  info_variables <- paste0("Intervention: do(", info_variables, ")", collapse = "")


  # Combine means and variances in a single table and format the confidence
  # intervals
  df <- data.frame(
    internal_list$tables$interventional_means$Variable,
    round(internal_list$tables$interventional_means[, 2:3], digits = digits),
    paste0(
      "[",
      round(internal_list$tables$interventional_means$CI_lower, digits = 3),
      ",",
      round(internal_list$tables$interventional_means$CI_upper, digits = 3),
      "]"),
    round(internal_list$tables$interventional_variances[, 2:3], digits = digits),
    paste0(
      "[",
      round(internal_list$tables$interventional_means$CI_lower, digits = 3),
      ",",
      round(internal_list$tables$interventional_means$CI_upper, digits = 3),
      "]")
  )

  # Add row with row names
  df <- rbind(
    c("Variable", "Est.", "Std. Err.", "95% CI", "Est.", "Std. Err.", "95% CI"),
    df
  )

  # Delete column names
  colnames(df) <- NULL

  # Transform data.farme into strings
  df <- utils::capture.output(print(df, row.names = FALSE))

  n_char <- nchar(df[2])

  # Add header to the table
  middle <- gregexpr(pattern = "CI", text = df[2])[[1]][1] + 2

  df[1] <- paste0("Interventional mean",
                  paste0(rep(" ", times = middle - 19), collapse = ""),
                  "Interventional variance")

  # Add first line with information about the interventional variables
  df <- c(info_variables, df)


  # Write table
  writeLines(df)

}
