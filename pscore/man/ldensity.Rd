% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{ldensity}
\alias{ldensity}
\title{Density Plot for a Long Dataset}
\usage{
ldensity(data, melt = FALSE, x, facet, g, hist = FALSE)
}
\arguments{
\item{data}{A dataset (or melt()ed dataset)}

\item{melt}{Logical whether to melt() dataset}

\item{x}{name of variable for density}

\item{facet}{A variable to use for facetting}

\item{g}{A variable to use for grouping/colouring.  If \code{melt=TRUE}, this is
used as id.var as well.}

\item{hist}{Logical whether to make a density plot or histogram (if TRUE).}
}
\value{
A ggplot2 graph.
}
\description{
Internal function only, not meant for general use
Simple wrapper around ggplot2 functionaly to create
density plots, potentially for many variables and coloured by group.
}
\examples{
# simple facetted plot
pscore:::ldensity(mtcars, TRUE)
# simple coloured plot
pscore:::ldensity(mtcars, x = "mpg", g = "cyl")
}
