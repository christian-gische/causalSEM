% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_distributions.R
\name{plot_distributions}
\alias{plot_distributions}
\title{Plot distributions}
\usage{
plot_distributions(
  object,
  plot = TRUE,
  plot.dir = NULL,
  scales.free = FALSE,
  facets.color = c("#ed553b", "#3caea3", "#0568bf"),
  shade.alpha = 0.1
)
}
\arguments{
\item{object}{The object returned from calling
function \code{intervention_effect}.}

\item{plot, }{character, directory to save pdf plot files}

\item{scales.free, }{logical, FALSE (default): all plots have the same
x-axis and y-axis ticks, TRUE: free x-axis and y-axis ticks}

\item{facets.color, }{character, colors for sample, conditional, and causal
statistics}

\item{shade.alpha, }{numeric, alpha value (opaqueness) for shaded areas, 
0 (transparent) ... 1 (opaque)}
}
\value{
\code{plot_distributions} returns ggplot2 code
        for density plots
}
\description{
Function that generates distribution plots
}
\references{
Gische, C., Voelkle, M.C. (2021) Beyond the mean: a flexible 
framework for studying causal effects using linear models. Psychometrika 
(advanced online publication). https://doi.org/10.1007/s11336-021-09811-z
}
