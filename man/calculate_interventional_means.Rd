% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_interventional_means.R
\name{calculate_interventional_means}
\alias{calculate_interventional_means}
\title{Calculate Interventional Mean}
\usage{
calculate_interventional_means(
  C = NULL,
  x = NULL,
  SI = NULL,
  n = NULL,
  IN = NULL,
  verbose = NULL,
  model = NULL,
  use_model_values = FALSE
)
}
\arguments{
\item{C}{Numeric matrix of structural coefficients.}

\item{x}{Numeric vector of interventional levels.}

\item{SI}{Numeric selection matrix.}

\item{n}{Integer number of observed variables.}

\item{IN}{Numeric zero-one matrix.}

\item{verbose}{Integer number describing the verbosity of console output.
Admissible values: 0: no output (default), 1: user messages, 
2: debugging-relevant messages.}
}
\value{
The numeric mean vector of the interventional distribution.
}
\description{
Calculate mean vector of the interventional distribution 
(see, for example, Eqs. 6a, 12a, and 22a in Gische and Voelkle, 2022). 
The arguments SI and IN (zero-one matrices) of the function are 
described in Definition 1 in Gische and Voelkle (2022).
}
\references{
Gische, C., Voelkle, M.C. (2022) Beyond the Mean: A Flexible 
Framework for Studying Causal Effects Using Linear Models. Psychometrika 87, 
868–901. https://doi.org/10.1007/s11336-021-09811-z
}
