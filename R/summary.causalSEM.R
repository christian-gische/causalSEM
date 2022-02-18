## Changelog:
# MA 0.0.1 2022-02-17: initial programming

## Documentation
#' @title Summarizing a Causal SEM Object
#' @description summary method for class "causalSEM".
#' @param x an object of class "causalSEM", usually, a result of a call to intervention_effect.
#' @param digit integer indicating the number of decimal places (round) or
#' significant digits (signif) to be used.
#' @return \code{summary.causalSEM} prints a list of summary statistics.
#' @references
#' @references Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
#' framework for studying causal effects using linear models. Psychometrika 
#' (advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
#' @export

## Function definition
summary.causalSEM <- function(x, digits = 3){

  # function name
  fun_name <- "summary.causalSEM"

  # function version
  fun_version <- "0.0.1 2022-02-17"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # console output
  if(internal_list$control$verbose >= 2) {
    cat(paste0("start of function ",fun_name_version, " ", Sys.time(), "\n" ))
  }

  # Prepare headers of the interventional means and variances
  header_info <- c()
  for (i in 1:x$info_interventions$n_intervention) {
    header_info[length(header_info) + 1] <- x$info_interventions$intervention_name[i]
    header_info[length(header_info) + 1] <- "="
    header_info[length(header_info) + 1] <- x$info_interventions$intervention_level[i]
    if (i < x$info_interventions$n_intervention) {
      header_info[length(header_info) + 1] <- ", "
    }
  }
  header_info <- paste0(header_info, collapse = "")

  # Collapse infomation about the interventional variables into a single string
  header_means <- paste0("Interventional means for do(", header_info, "):", collapse = "")
  header_variances <- paste0("Interventional variances for do(", header_info, "):", collapse = "")

  # Create output

  output <- c(
    "Model Info",
    "----------",
    paste("fitted model class:", x$fitted_object_class[1]),
    paste("number of observations:", x$info_model$n_obs),
    paste("number of variables:", x$info_model$n_ov),
    paste("variable names:", paste(x$info_model$var_names, collapse = ", ")),
    "",
    "Intervention Info",
    "-----------------",
    "Exposure Info",
    paste("number of interventions:", x$info_interventions$n_intervention),
    paste("intervention names:", paste(x$info_interventions$intervention_names,
                                       collapse = ", ")),
    paste("intervention levels:",
          paste(
            round(x$info_interventions$intervention_levels, digits = digits),
            collapse = ", "
            )
    ),
    paste("effect type:", x$info_interventions$effect_type),
    "",
    "Outcome Info",
    paste("number of outcomes:", x$info_interventions$n_outcome),
    paste("outcome names:", paste(x$info_interventions$outcome_names,
                                  collapse = ", ")),
    #paste("lower bounds:", paste(x$info_interventions$lower_bounds,
    #                              collapse = ", ")),
    #paste("lower bounds:", paste(x$info_interventions$upper_bounds,
    #                              collapse = ", ")),
    "",
    header_means,
    prepare_table_means(x, digits = digits),
    "",
    header_variances,
    prepare_table_variances(x, digits = digits))


  # Add interventional probability (if possible)

  # if (all(c(length(x$info_interventions$lower_bounds) == 1,
  #           length(x$info_interventions$upper_bounds) == 1,
  #           x$info_interventions$n_outcome == 1,
  #           x$info_interventions$n_intervention == 1))) {
  #
  #   probability <- calculate_interventional_probabilities(
  #     mean = x$interventional_distribution$means$values[x$info_interventions$intervention_names, ],
  #     sd = x$interventional_distribution$covariance_matrix$values[x$info_interventions$intervention_names, x$info_interventions$intervention_names],
  #     y_low = x$info_interventions$lower_bounds,
  #     y_up = x$info_interventions$upper_bounds,
  #     verbose = FALSE
  #   )
  #
  #   probability <- paste0(
  #     "Interventional probability P(",
  #     round(x$info_interventions$lower_bounds, digits = digits),
  #     "<",
  #     x$info_interventions$outcome_names,
  #     "<",
  #     round(x$info_interventions$upper_bounds, digits = digits),
  #     "|do(",
  #     x$info_interventions$intervention_names,
  #     "=",
  #     round(x$interventional_distribution$means$values[x$info_interventions$intervention_names, ], digits = digits),
  #     "))=",
  #     round(probability, digits = digits)
  #   )
  #
  #   output <- c(output, "", probability)
  #
  # }

  # Write table
  writeLines(output)

}
