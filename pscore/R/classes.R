#' @include misc.R methods.R
NULL

#' A S4 class to represent data for creating a composite
#'
#' @slot rawdata A data frame of the data to be used for the composite scores
#' @slot groups A character string, the same length as the number of rows of
#'   the data indicating to which group each row belong.  May be all the same
#'   if only one group present in the data.
#' @slot thresholds A list with as many elements as there are unique
#'   groups in the data, and where each element is a vector the same
#'   length as the number of columns in the data frame, indicating the
#'   reference thresholds for each variable, by group (all the same if only
#'   one group).
#' @slot higherisbetter A logical vector the same length as the number of
#'   columns in the data frame, indicating whether higher is better for
#'   each variable (if \code{TRUE}) and otherwise (if \code{FALSE}) that
#'   lower is better, indicating that variable should be reversed.
#' @slot k The number of variables as an integer.
#' @export
setClass("CompositeData",
         slots = list(
             rawdata = "data.frame",
             groups = "character",
             thresholds = "list",
             higherisbetter = "logical",
             k = "integer"),
         prototype = list(
             rawdata = data.frame(),
             groups = NA_character_,
             thresholds = list(),
             higherisbetter = NA,
             k = NA_integer_),
         validity = function(object) {
             errors <- character()
             row_data <- nrow(object@rawdata)
             col_data <- ncol(object@rawdata)
             length_groups <- length(object@groups)
             unique_groups <- length(unique(object@groups))
             length_thresholds <- length(object@thresholds)
             n_thresholds <- unique(sapply(object@thresholds, length))
             length_hisb <- length(object@higherisbetter)
             tnames <- names(object@thresholds)
             gnames <- unique(object@groups)

             if (!all(apply(object@rawdata, 2, is.numeric))) {
                 msg <- "All columns in the data must be numeric"
                 errors <- c(errors, msg)
             }

             if (any(colSums(is.na(object@rawdata)) == row_data)) {
                 msg <- sprintf("The following variables are completely missing: %s",
                                paste(colnames(object@rawdata)[colSums(is.na(object@rawdata)) == row_data],
                                      collapse = ", "))
                 errors <- c(errors, msg)
             }

             if (!identical(row_data, length_groups)) {
                 msg <- paste0("Groups is length ", length_groups,
                               ".  Should be ", row_data)
                 errors <- c(errors, msg)
             }

             if (identical(unique_groups, 0L)) {
                 msg <- paste0("Number of groups must be a positive integer")
                 errors <- c(errors, msg)
             }

             if (!identical(unique_groups, length_thresholds)) {
                 msg <- paste0("Thresholds is length ", length_thresholds,
                               ".  Should be ", unique_groups)
                 errors <- c(errors, msg)
             }

             if (length(n_thresholds) > 1) {
                 msg <- paste0("Thresholds should be a list, ",
                               "with a vector for each group, ",
                               "and all vectors should be the same length.")
                 errors <- c(errors, msg)
             }

             if (!identical(col_data, n_thresholds[1])) {
                 msg <- paste0("There are ", n_thresholds, " thresholds",
                               ".  There should be ", col_data, " thresholds.")
                 errors <- c(errors, msg)
             }


             if (!identical(col_data, length_hisb)) {
                 msg <- paste0("higherisbetter is length ", length_hisb,
                               ".  Should be ", col_data, ".")
                 errors <- c(errors, msg)
             }

             if (!all(sort(tnames) == sort(gnames))) {
                 msg <- "Thresholds must be a named list where the names match the unique names in groups"
                 errors <- c(errors, msg)
             }

             if (!identical(col_data, object@k)) {
                 msg <- paste0("k is ", object@k,
                               ".  Should be ", col_data, ".")
                 errors <- c(errors, msg)
             }

             if (length(errors) == 0) TRUE else errors
           }

)

#' An S4 class to represent distance scores
#'
#' @slot distances A data frame of the distance scores
#' @slot distanceDensity A ggplot2 density graph
#' @slot winsorizedValues A data frame of the values at which each variable was winsorized.
#'   If percentile is 0, that means no winsorization, and values will simply be min and max.
#' @slot better A logical value whether better scores than threshold were allowed
#' @export
setClass("DistanceScores",
    slots = list(
        distances = "data.frame",
        distanceDensity = "ANY",
        winsorizedValues = "data.frame",
        better = "logical"),
    prototype = list(
        distances = data.frame(),
        distanceDensity = NA,
        winsorizedValues = data.frame(),
        better = NA),
    contains = "CompositeData")

#' An S4 class to represent composite ready data
#'
#' @slot data A data frame ready for use to generate composite scores
#' @slot covmat A covariance matrix
#' @slot sigma The standard deviation of each variable
#' @slot standardize A logical value whether standardization was applied
#' @export
setClass("CompositeReady",
    slots = list(
        data = "data.frame",
        covmat = "matrix",
        sigma = "numeric",
        standardize = "logical"),
    prototype = list(
        data = data.frame(),
        covmat = matrix(NA_real_),
        sigma = NA_real_,
        standardize = NA),
    contains = "DistanceScores")

#' An S4 class to represent composite scores based on Mahalanobis distance
#'
#' @slot scores A vector of the final scores
#' @slot scoreHistogram A histogram of the final scores
#' @slot screePlot A screeplot from the PCA
#' @slot loadingGraph A graph of the component loadings
#' @slot loadingTable A table of all the component loadings
#' @slot pca Prinicipal component analysis results. A list (coercied from princomp output).
#' @slot ncomponents The number of components of the PCA to be used.
#' @slot CompositeReady The original CompositeReady class object passed in
#' @export
#' @rdname Scores
setClass("MahalanobisScores",
 slots = list(
     scores = "numeric",
     scoreHistogram = "ANY",
     screePlot = "ANY",
     loadingGraph = "ANY",
     loadingTable = "matrix",
     pca = "list",
     ncomponents = "numeric",
     CompositeReady = "CompositeReady"
     ),
 prototype = list(
   scores = NA_real_,
   scoreHistogram = NA,
   screePlot = NA,
   loadingGraph = NA,
   loadingTable = matrix(NA_character_),
   pca = list(NA_real_),
   ncomponents = NA_real_,
   CompositeReady = CompositeReady(
     data = data.frame(NA_real_),
     distances = data.frame(NA_real_),
     rawdata = data.frame(0))
   ))


#' An S4 class to represent composite scores based on summing
#'
#' @slot scores A vector of the final scores
#' @slot scoreHistogram A histogram of the final scores
#' @slot transform A character string of the type of transformation applied
#' @slot type A character string indicating whether data were summed or averaged
#' @slot trans A list of functions to transform data and backtransform it
#' @slot systems A list with as many elements as there are systems where
#'   each element is a character vector containing the variable names for
#'   each system.  If all variables belong to one system, a list with one
#'   element that is missing.
#' @slot CompositeReady The original CompositeReady class object passed in
#' @export
#' @rdname Scores
setClass("SumScores",
         slots = list(
             scores = "numeric",
             scoreHistogram = "ANY",
             transform = "character",
             type = "character",
             trans = "list",
             systems = "list",
             CompositeReady = "CompositeReady"
             ),
         prototype = list(
             scores = NA_real_,
             scoreHistogram = NA,
             transform = NA_character_,
             type = NA_character_,
             trans = list(to = I, from = I),
             systems = list(NA_character_),
             CompositeReady = new("CompositeReady")
             ))


#' An S4 class to represent composite scores based on (confirmatory) factor analysis
#'
#' @slot scores A vector of the final scores
#' @slot scoreHistogram A histogram of the final scores
#' @slot factorScores A data frame with all factor score predictions
#' @slot type A character string indicating the type of factor model used
#' @slot factors A list with as many elements as there are specific factors,
#'   where each element is a character vector of the variables for
#'   a specific factor
#' @slot Fit A fitted model object from lavaan.
#' @slot CompositeReady The original CompositeReady class object passed in
#' @export
#' @rdname Scores
setClass("FactorScores",
      slots = list(
          scores = "numeric",
          scoreHistogram = "ANY",
          factorScores = "data.frame",
          type = "character",
          factors = "list",
          Fit = "ANY",
          CompositeReady = "CompositeReady"
          ),
      prototype = list(
          scores = NA_real_,
          scoreHistogram = NA,
          factorScores = data.frame(),
          type = NA_character_,
          factors = list(NA_character_),
          Fit = NA,
          CompositeReady = new("CompositeReady")
          ))
