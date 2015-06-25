#' A S4 class to represent data for creating a composite
#'
#' @slot data A data frame of the data to be used for the composite scores
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
#' @exportClass CompositeData
#' @import methods
CompositeData <- setClass("CompositeData",
                          slots = list(
                              data = "data.frame",
                              groups = "character",
                              thresholds = "list",
                              higherisbetter = "logical"),
                          prototype = list(
                              data = data.frame(),
                              groups = NA_character_,
                              thresholds = list(),
                              higherisbetter = NA),
                          validity = function(object) {
                              errors <- character()
                              row_data <- nrow(object@data)
                              col_data <- ncol(object@data)
                              length_groups <- length(object@groups)
                              unique_groups <- length(unique(object@groups))
                              length_thresholds <- length(object@thresholds)
                              n_thresholds <- unique(sapply(object@thresholds, length))
                              length_hisb <- length(object@higherisbetter)
                              tnames <- names(object@thresholds)
                              gnames <- unique(object@groups)

                              if (!all(apply(object@data, 2, is.numeric))) {
                                  msg <- "All columns in the data must be numeric"
                                  errors <- c(errors, msg)
                              }

                              if (any(colSums(is.na(object@data)) == row_data)) {
                                  msg <- sprintf("The following variables are completely missing: %s",
                                                 paste(colnames(object@data)[colSums(is.na(object@data)) == row_data],
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

                              if (length(errors) == 0) TRUE else errors
                          })

#' An S4 initialize method for CompositeData class
#'
#' @docType methods
#' @param .Object an object of class CompositeData
#' @param data A data frame with at least one row and column
#' @param groups an optional character vector. If omitted defaults to
#'   a character vector of all \dQuote{a}s.
#' @param thresholds an optional named list where names match the names in groups.
#'   If using defaults for groups, should name it \dQuote{a}.
#' @param higherisbetter an optional logical vector
#' @aliases initialize,CompositeData
#' @export
setMethod("initialize",
          "CompositeData",
          function(.Object, data, groups, thresholds, higherisbetter) {
          if (missing(data)) {
              stop("data must be specified")
          }
          if (nrow(data) < 1 || ncol(data) < 1) {
              stop("data must have at least one row and column.")
          }

          .Object@data <- data

          if (missing(groups)) {
              .Object@groups <- rep("one", nrow(data))
          } else {
              .Object@groups <- groups
          }

          if (anyNA(.Object@groups)) {
              warnings("Groups cannot have missing data and these rows of data are dropped")
              .Object@data <- .Object@data[!is.na(.Object@groups), , drop = FALSE]
              .Object@groups <- .Object@groups[!is.na(.Object@groups)]
          }

          if (missing(thresholds)) {
              un <- unique(.Object@groups)
              tmp <- lapply(un, function(x) rep(0, ncol(data)))
              names(tmp) <- un
              .Object@thresholds <-tmp
          } else {
              .Object@thresholds <- thresholds
          }

          if (missing(higherisbetter)) {
              .Object@higherisbetter <- rep(TRUE, ncol(data))
          } else {
              .Object@higherisbetter <- higherisbetter
          }

          test <- validObject(.Object)
          if (isTRUE(test)) .Object else test
          })

#' An S4 class to represent distance scores
#'
#' @slot distances A data frame of the distance scores
#' @slot density A ggplot2 density graph
#' @slot winsorizedValues A data frame of the values at which each variable was winsorized.
#'   If percentile is 0, that means no winsorization, and values will simply be min and max.
#' @slot better A logical value whether better scores than threshold were allowed
#' @slot call The function call
#' @slot compositeData The original CompositeData class object passed in.
#' @exportClass DistanceScores
DistanceScores <- setClass("DistanceScores",
                      slots = list(
                          distances = "data.frame",
                          density = "ANY",
                          winsorizedValues = "data.frame",
                          better = "logical",
                          call = "call",
                          compositeData = "CompositeData"
                          ),
                      prototype = list(
                          distances = data.frame(),
                          density = NA,
                          winsorizedValues = data.frame(),
                          better = NA,
                          call = (function(x) match.call())(),
                          compositeData = new("CompositeData", data.frame(NA_real_))
                          ))

#' An S4 class to represent composite ready data
#'
#' @slot data A data frame ready for use
#' @slot covmat A covariance matrix
#' @slot sigma The standard deviation of each variable
#' @slot standardize A logical value whether standardization was applied
#' @slot k The number of variables
#' @slot DistanceScores The original DistanceScores class object passed in.
#' @exportClass DistanceScores
CompositeReady <- setClass("CompositeReady",
                      slots = list(
                          data = "data.frame",
                          covmat = "matrix",
                          sigma = "numeric",
                          standardize = "logical",
                          k = "integer",
                          distanceScores = "DistanceScores"
                          ),
                      prototype = list(
                          data = data.frame(),
                          covmat = matrix(NA_real_),
                          sigma = NA_real_,
                          standardize = NA,
                          k = NA_integer_,
                          distanceScores = new("DistanceScores")
                          ))
