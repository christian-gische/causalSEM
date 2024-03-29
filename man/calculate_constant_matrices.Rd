% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_constant_matrices.R
\name{calculate_constant_matrices}
\alias{calculate_constant_matrices}
\title{Calculate Zero-One Matrices}
\usage{
calculate_constant_matrices(
  model = NULL,
  intervention_names = NULL,
  outcome_names = NULL,
  verbose = NULL,
  use_model_values = FALSE
)
}
\arguments{
\item{model}{Object of class \code{causalSEM}.}

\item{intervention_names}{Names of interventional variables.}

\item{outcome_names}{Names of outcome variables.}

\item{verbose}{Verbosity of console output.}
}
\value{
List with several zero-one matrices:
\tabular{lll}{
\tab   \code{$select_intervention} \tab   \cr
\tab   \code{$select_non_intervention} \tab \cr
\tab   \code{$select_outcome} \tab \cr
\tab   \code{$eliminate_intervention} \tab \cr
\tab   \code{$duplication_matrix} \tab \cr
\tab   \code{$elimination_matrix} \tab \cr
\tab   \code{$commutation_matrix} \tab}
}
\description{
Calculates zero-one matrices used in the computation
of the interventional distribution 
(see, for example, Definition 1 in Gische and Voelkle, 2022).
}
\references{
Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
868–901. https://doi.org/10.1007/s11336-021-09811-z
}
