% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/calculate_jacobian_interventional_probabilities.R
\name{calculate_jacobian_interventional_probabilities}
\alias{calculate_jacobian_interventional_probabilities}
\title{Calculate Jacobian of the Probablity of an Interventional Event}
\usage{
calculate_jacobian_interventional_probabilities(
  model,
  x = NULL,
  intervention_names = NULL,
  outcome_names = NULL,
  lower_bounds = NULL,
  upper_bounds = NULL,
  verbose = NULL
)
}
\arguments{
\item{model}{internal_list or object of class causalSEM}

\item{x}{interventional levels}

\item{intervention_names}{names of interventional variables}

\item{outcome_names}{name of outcome variable}

\item{lower_bounds}{numeric with lower bound}

\item{upper_bounds}{numeric with upper bound}

\item{verbose}{verbosity of console outputs}
}
\value{
\code{calculate_jacobian_interventional_probabilities} returns the
   Jacobian of interventional probabilities (numeric matrices)
   as defined in Eq. 18a (p. 17)
}
\description{
Calculates Jacobian of the probability of a univariate 
interventional evenent. The probability is calculated for (i) a univariate 
outcome variable, (ii) a specific interventional level, and (iii )specific 
values of the lower and upper bound of the critical range of the outcome 
variable.
}
\references{
Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
framework for studying causal effects using linear models. Psychometrika 
(advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
}
