% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{CompositeReady}
\alias{CompositeReady}
\title{An constructor function for the S4 CompositeReady class}
\usage{
CompositeReady(
  data,
  covmat,
  sigma,
  standardize,
  use.prethreshold,
  distances,
  distanceDensity,
  winsorizedValues,
  better,
  rawdata,
  groups,
  thresholds,
  higherisbetter,
  k,
  rawtrans
)
}
\arguments{
\item{data}{A data frame of the ready to use data}

\item{covmat}{A covariance matrix of the data}

\item{sigma}{A vector of the standard deviations of each variable}

\item{standardize}{A logical whether the data were standardized or not}

\item{use.prethreshold}{A logical value whether to calculate covariance matrix
based on the data after winsorizing, but before applying the threshold.
Defaults to \code{FALSE}, so that covariances are calculated after thresholds
(if any) are applied.}

\item{distances}{A data frame of the distance scores}

\item{distanceDensity}{A ggplot2 graph of the densities of each distance score.
If not passed, generated automatically from the data.}

\item{winsorizedValues}{A data frame indicating the values used (if any) for winsorization.
Should have one row for each variable in the dataset.}

\item{better}{A logical vector the same length as the number of columns in the distance scores
indicating whether higher or lower values are better for each.}

\item{rawdata}{A data frame with at least one row and column}

\item{groups}{an optional character vector. If omitted defaults to
a character vector of all \dQuote{one}s.}

\item{thresholds}{an optional named list where names match the names in groups.
If using defaults for groups, should name it \dQuote{a}.}

\item{higherisbetter}{an optional logical vector}

\item{k}{an optional integer, the number of columns in the raw data}

\item{rawtrans}{A list of functions to transform the raw data (and thresholds).}
}
\value{
An S4 object of class \dQuote{CompositeReady}
}
\description{
An constructor function for the S4 CompositeReady class
}
\examples{
#make me!
}
