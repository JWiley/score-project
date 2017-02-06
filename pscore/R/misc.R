#' Density Plot for a Long Dataset
#'
#' Internal function only, not meant for general use
#' Simple wrapper around ggplot2 functionaly to create
#' density plots, potentially for many variables and coloured by group.
#'
#' @param data A dataset (or melt()ed dataset)
#' @param melt Logical whether to melt() dataset
#' @param x name of variable for density
#' @param facet A variable to use for facetting
#' @param g A variable to use for grouping/colouring.  If \code{melt=TRUE}, this is
#'   used as id.var as well.
#' @param hist Logical whether to make a density plot or histogram (if TRUE).
#' @return A ggplot2 graph.
#' @import methods ggplot2 reshape2
#' @examples
#' # simple facetted plot
#' pscore:::ldensity(mtcars, TRUE)
#' # simple coloured plot
#' pscore:::ldensity(mtcars, x = "mpg", g = "cyl")
ldensity <- function(data, melt = FALSE, x, facet, g, hist=FALSE) {
    data <- as.data.frame(data)

    if (melt) {
        if (!missing(x)) stop("Cannot specifiy both melt = TRUE and an 'x' variable")
        if (!missing(facet)) stop("Cannot specifiy both melt = TRUE and a facet variable")


        if (!missing(g)) {
            data <- melt(data, measure.vars = setdiff(colnames(data), g), id.var = g)
        } else {
            data <- melt(data, measure.vars = colnames(data))
        }
        x <- "value"
        facet <- ~variable
    }

    if (!missing(g)) {
        if (!(is.character(data[, g]) || is.factor(data[, g]))) {
            data[, g] <- factor(data[, g])
        }
        if (hist) {
            p <- ggplot(data, aes_string(x = x, color = g, fill = g))
        } else {
            p <- ggplot(data, aes_string(x = x, color = g))
        }
    } else {
        p <- ggplot(data, aes_string(x = x))
    }

    if (hist) {
        p <- p + geom_histogram()
    } else {
        p <- p + geom_density()
    }

    if (!missing(facet)) {
        p <- p + facet_wrap(facet, scales = "free")
    }
    p + theme_bw()
}

#' Drop unnecessary data from a MahalanobisComposite object.
#'
#' This function removes graphs and other sensitive data.
#'
#' @param object A MahalanobisComposite object
#' @return A MahalanobisComposite object with some slots replaced with missing values
#' @export
#' @examples
#' # make me!!!
dropData <- function(object) {
  object@scores <- NA_real_
  object@scoreHistogram <- NA_real_
  object@screePlot <- NA_real_
  object@loadingGraph <- NA_real_
  object@loadingTable <- matrix(NA_real_)
  object@CompositeReady@data <- data.frame(NA_real_)
  object@CompositeReady@distances <- data.frame(NA_real_)
  object@CompositeReady@rawdata <- data.frame(NA_real_)
  object@CompositeReady@groups <- ""
  object@CompositeReady@distanceDensity <- list()
  return(object)
}



#' @name BioDB
#' @title Biomarker Thresholds Database
#' @description This data set lists the clinical high risk thresholds for a variety of biomarkers
#' @docType data
#' @format a \code{list}.
#' @source Various publications
NULL
## BioDB <- list(
##     MetabolicSyndrome = data.frame(
##         Biomarker = c("SBP", "DBP", "Triglycerides", "HDL-C", "WaistCircumference", "BMI", "Glucose", "HbA1c"),
##         Units = c("mmHg", "mmHg", "mmol/L", "mmol/L", "cm", "kg/m^2", "mmol/L", "percent"),
##         Female = c(130, 85, 1.7, 1.3, 80, 25, 5.6, 5.7),
##         Male = c(130, 85, 1.7, 1.0, 94, 25, 5.6, 5.7),
##         Higherisbetter = c(0, 0, 0, 1, 0, 0, 0, 0)))
## save(BioDB, file = "../data/BioDB.rda")
