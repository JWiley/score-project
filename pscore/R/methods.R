#' An constructor function for the S4 CompositeData class
#'
#' @param rawdata A data frame with at least one row and column
#' @param groups an optional character vector. If omitted defaults to
#'   a character vector of all \dQuote{one}s.
#' @param thresholds an optional named list where names match the names in groups.
#'   If using defaults for groups, should name it \dQuote{a}.
#' @param higherisbetter an optional logical vector
#' @param k an optional integer, the number of columns in the raw data
#' @return An S4 object of class \dQuote{CompositeData}
#' @export
#' @examples
#' #make me!
CompositeData <- function(rawdata, groups, thresholds, higherisbetter, k) {
    if (missing(rawdata)) {
        stop("rawdata must be specified")
    }

    if (nrow(rawdata) < 1 || ncol(rawdata) < 1) {
        stop("rawdata must have at least one row and column.")
    }

    if (missing(groups)) {
        groups <- rep("one", nrow(rawdata))
    }

    if (anyNA(groups)) {
        warnings("Groups cannot have missing data and these rows of data are dropped")
        rawdata <- rawdata[!is.na(groups), , drop = FALSE]
        groups <- groups[!is.na(groups)]
    }

    if (missing(thresholds)) {
        un <- unique(groups)
        tmp <- lapply(un, function(x) rep(0, ncol(rawdata)))
        names(tmp) <- un
        thresholds <-tmp
    }

    if (missing(higherisbetter)) {
        higherisbetter <- rep(TRUE, ncol(rawdata))
    }

    if (missing(k)) {
        k <- ncol(rawdata)
    }

    object <- new("CompositeData",
                  rawdata = rawdata,
                  groups = groups,
                  thresholds = thresholds,
                  higherisbetter = higherisbetter,
                  k = k)

    test <- validObject(object)
    if (isTRUE(test)) object else test
}

#' An constructor function for the S4 DistanceScores class
#'
#' @param distances A data frame of the distance scores
#' @param distanceDensity A ggplot2 graph of the densities of each distance score.
#'   If not passed, generated automatically from the data.
#' @param winsorizedValues A data frame indicating the values used (if any) for winsorization.
#'   Should have one row for each variable in the dataset.
#' @param better A logical vector the same length as the number of columns in the distance scores
#'   indicating whether higher or lower values are better for each.
#' @inheritParams CompositeData
#' @return An S4 object of class \dQuote{DistanceScores}
#' @export
#' @examples
#' #make me!
DistanceScores <- function(distances, distanceDensity, winsorizedValues, better, rawdata, groups, thresholds, higherisbetter, k) {
    stopifnot(is.data.frame(distances))

    if (missing(distances)) {
        stop("distances must be specified")
    }

    if (nrow(distances) < 1 || ncol(distances) < 1) {
        stop("distances must have at least one row and column.")
    }

    if (missing(distanceDensity)) {
        distanceDensity <- ldensity(distances, TRUE)
    }

    if (missing(winsorizedValues)) {
        winsorizedValues <- data.frame(
            low = rep(NA_real_, ncol(distances)),
            high = rep(NA_real_, ncol(distances)),
            percentile = rep(NA_real_, ncol(distances)))
    }

    if (missing(better)) {
        better <- rep(NA, ncol(distances))
    }

    object <- new("DistanceScores",
                  distances = distances,
                  distanceDensity = distanceDensity,
                  winsorizedValues = winsorizedValues,
                  better = better,
                  CompositeData(
                      rawdata = rawdata,
                      groups = groups,
                      thresholds = thresholds,
                      higherisbetter = higherisbetter,
                      k = k)
                  )

    test <- validObject(object)
    if (isTRUE(test)) object else test
}
