% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fill_in_interventional_probabilities.R
\name{fill_in_interventional_probabilities}
\alias{fill_in_interventional_probabilities}
\title{Fills in Probabilities of Interventional Events to Internal List}
\usage{
fill_in_interventional_probabilities(internal_list)
}
\arguments{
\item{internal_list}{A list with various information extracted from the
model.}
}
\value{
Returns the inputted internal_list with slot
   interventional_distribution$probabilities$values populated with (a) numeric 
   value(s); interventional probabilities are only calculated for outcome 
   variables (drawn from internal_list$info_interventions$outcome_names).
}
\description{
Fills in probabilities of interventional events to internal 
list.
}
\references{
Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
framework for studying causal effects using linear models. Psychometrika 
(advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
}
\keyword{internal}