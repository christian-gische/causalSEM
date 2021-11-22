## Changelog:
# MA 0.0.1 2021-11-19: initial programming

## Documentation
#' @title Calculate asymptotic standard errors of the interventional probabilities
#' @description Internal functions that calculates the asymptotic covariance,
#' standard errors, and z-values of the interventional probabilities.
#' @param model internal_list or object of class causalSEM
#' @param x interventional level
#' @param intervention_names names of interventional variables
#' @param outcome_names name of outcome variable
#' @param lower_bound numeric with lower bound
#' @param upper_bound numeric with upper bound
#' @param verbose verbosity of console outputs
#' @return \code{calculate_ase_interventional_means} returns the
#' asysmptotic covariance matrix, the aysmptotic standard errors, and the
#' approximate z-values of the the interventional means for a specific
#' interventional level and a specific value in the range of the outcome
#' variable (see Corollaries, 10, 11 in Gische and Voelkle, 2021).
#' @references
#' Gische, C. & Voelkle, M. C. (under review). Beyond the mean: A flexible framework for
#'    studying causal effects using linear models. \url{https://www.researchgate.net/profile/Christian-Gische/publication/335030449_Gische_Voelkle_Causal_Inference_in_Linear_Models/links/6054eb6e299bf1736755110b/Gische-Voelkle-Causal-Inference-in-Linear-Models.pdf}

calculate_ase_interventional_probabilities <- function(
  model, x, intervention_names, outcome_names, lower_bound, upper_bound, verbose
  ) {

  # function name
  fun_name <- "calculate_ase_interventional_probabilities"

  # function version
  fun_version <- "0.0.1 2021-11-19"

  # function name+version
  fun_name_version <- paste0(fun_name, " (", fun_version, ")")

  # get verbose argument
  verbose <- model$control$verbose

  # console output
  if (verbose >= 2) {
    cat(paste0("start of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # TODO check if user argument model is the internal_list or
  # an object of class causalSEM
  # CURRENTLY, the function assumes that the input model is
  # of type internal_list. After allowing for objects of class causalSEM
  # the pathes starting with internal_list$ might need adjustment


  # get variable names of interventional variables
  if (is.character(intervention_names) && all(intervention_names %in% model$info_model$var_names)) {
    x_labels <- intervention_names
  } else {
    x_labels <- model$info_interventions$intervention_name
  }

  # get interventional levels
  if (is.numeric(x) && length (x) == length(intervention_names)) {
    x_values <- x
  } else {
    x_values <- model$info_interventions$intervention_level
  }

  # get total number of variables
  # get number of unique parameters
  n <- model$info_model$n_ov
  n_unique <- model$info_model$param$n_par_unique

  # get intervential means
  # TODO: assign E by calling the function calculate_interventional_means
  gamma_4 <- model$interventional_distribution$probability$p

  # compute jacobian of the pdf
  jac_g4 <- calculate_jacobian_interventional_probabilities(
    model = model,
    x = x_values,
    intervention_names = intervention_names,
    outcome_names = outcome_names,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    verbose = verbose
  )

  # get AV of parameter vector
  acov <- model$info_model$param$varcov_par_unique

  # compute asymptotic variance
  acov_gamma_4 <- jac_g4 %*% acov %*% t(jac_g4)

  # compute asymptotic standard errors
  ase_gamma_4 <- sqrt(diag(acov_gamma_4))

  # compute approximate z-value
  z_gamma_4 <- gamma_4 / ase_gamma_4

  # Prepare output ----

  # Console output
  if (verbose >= 2) {
    cat(paste0("  end of function ", fun_name_version, " ", Sys.time(), "\n"))
  }

  # Output
  list(gamma_4 = gamma_4,
       acov_gamma_4 = acov_gamma_4,
       ase_gamma_4 = ase_gamma_4,
       z_gamma_4 = z_gamma_4)

}
