% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ugcflss_simple_desc.R
\name{ugcflss_describe}
\alias{ugcflss_describe}
\title{Compute requested statistic for either entire sample or by group}
\usage{
ugcflss_describe(
  res_obj,
  by_group = FALSE,
  stat = "median",
  interval = 0.89,
  tau = 0.5,
  return_draws = FALSE
)
}
\arguments{
\item{res_obj}{Object returned by main function}

\item{by_group}{If TRUE, compute by group.
If FALSE, compute for entire sample.}

\item{stat}{Statistic of interest, one of "median", "mean", "sd"
or "quantile".}

\item{interval}{Some quantile interval between 0 and 1}

\item{tau}{If stat = "quantile", the value of the probability between 0 and 1}

\item{return_draws}{If TRUE, do not summarize the draws.
If FALSE, summarize the draws.}
}
\value{
Returns dataset.
}
\description{
Compute requested statistic for either entire sample or by group
}
