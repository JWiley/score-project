#' Internal function to prepare data for prediction
#'
#' @param object An object of S4 class \dQuote{MahalanobisScores},
#'   \dQuote{SumScores}, or \dQuote{FactorScores} containing
#'   a model and results to be used to get predictions on new data.
#' @param newdata A data frame with identical variable names as was used
#'   to build the initial model.
#' @param groups A vector with the same length as the data frame in \code{newdata},
#'   has rows, containing the groups each row belongs to.  See \code{CompositeData}
#'   for more details.
#' @return An object of S4 class \dQuote{CompositeReady}
#' @rdname preparePredict
.preparePredict <- function(object, newdata, groups) {
  stopifnot(is.data.frame(newdata))

  stopifnot(identical(ncol(newdata), ncol(object@CompositeReady@rawdata)))

  ug <- unique(groups)
  thresholds <- object@CompositeReady@thresholds
  thresholds <- thresholds[names(thresholds) %in% ug]

  d <- CompositeData(rawdata = newdata,
                     groups = groups,
                     thresholds = thresholds,
                     higherisbetter = object@CompositeReady@higherisbetter,
                     k = object@CompositeReady@k)
  dres <- prepareDistances(d,
                           winsorize = object@CompositeReady@winsorizedValues$percentile,
                           values = object@CompositeReady@winsorizedValues,
                           better = object@CompositeReady@better)

  prepareComposite(dres,
                   covmat = object@CompositeReady@covmat,
                   standardize = object@CompositeReady@standardize)
}

#' Internal function to prepare data for prediction
#'
#' @param object An object of S4 class \dQuote{MahalanobisScores},
#'   \dQuote{SumScores}, or \dQuote{FactorScores} containing
#'   a model and results to be used to get predictions on new data.
#' @param newdata A data frame with identical variable names as was used
#'   to build the initial model.
#' @param groups A vector with the same length as the data frame in \code{newdata},
#'   has rows, containing the groups each row belongs to.  See \code{CompositeData}
#'   for more details.
#' @return An object of S4 class \dQuote{CompositeReady}
#' @export
#' @examples
#' # make me!!
predictCS <- function(object, newdata, groups) {

  cprep <- .preparePredict(object, newdata, groups)

  if (is(object, "SumScores")) {
    if (is.na(object@systems[[1]])) {
      comp <- sumComposite(cprep,
                           transform = object@transform,
                           type = object@type)
    } else {
        comp <- sumComposite(cprep,
                             transform = object@transform,
                             type = object@type,
                             systems = object@systems)
    }
  } else if (is(object, "MahalanobisScores")) {
      comp <- mahalanobisComposite(cprep,
                                   ncomponents = object@ncomponents,
                                   pca = object@pca)
  } else if (is(object, "FactorScores")) {
      comp <- as.data.frame(predict(object@Fit, newdata = cprep@data))
  } else {
      stop(sprintf("Cannot handle object of class \"%s\".", class(object)))
  }

  return(comp)
}

## Examples or unit tests
## maybe add a tests directory to make sure these things work?
## testit1 <- predictCS(mcomp, newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")], groups = "one")
## testit2 <- predictCS(scomp, newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")], groups = "one")
## testit3 <- predictCS(fcomp, newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")], groups = "one")

## all.equal(
##   testit1@scores,
##   mcomp@scores[1])

## all.equal(
##   testit2@scores,
##   scomp@scores[1])

## all.equal(
##   testit3@scores,
##   fcomp@scores[1])

