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

  stopifnot(identical(ncol(newdata), object@CompositeReady@k))

  ug <- unique(groups)
  thresholds <- object@CompositeReady@thresholds
  thresholds <- thresholds[names(thresholds) %in% ug]

  d <- CompositeData(rawdata = newdata,
                     groups = groups,
                     thresholds = thresholds,
                     higherisbetter = object@CompositeReady@higherisbetter,
                     k = object@CompositeReady@k,
                     rawtrans = object@CompositeReady@rawtrans)

  prepareComposite(d,
                   winsorize = object@CompositeReady@winsorizedValues$percentile,
                   values = object@CompositeReady@winsorizedValues,
                   better = object@CompositeReady@better,
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
#' d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "disp")],
#'                    thresholds = list(one = with(mtcars, c(
#'                                      mpg = max(mpg),
#'                                      hp = max(hp),
#'                                      wt = min(wt),
#'                                      disp = min(disp)))),
#'                    higherisbetter = c(TRUE, TRUE, FALSE, FALSE))
#' ## create the distance scores
#' ## and prepare to create the composite
#' dres <- prepareComposite(d)
#'
#' ## create composite based on summing the (standardized)
#' scomp <- sumComposite(dres, "square", "sum")
#' ## use model to generate predictions on new data
#' predictCS(scomp,
#'           newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
#'           groups = "one")
#'
#' ## create composite based on mahalanobis distances
#' mcomp <- mahalanobisComposite(dres)
#' ## use model to generate predictions on new data
#' predictCS(mcomp,
#'           newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
#'           groups = "one")
#' ## note in this too simple example, there are negative variance estimates
#' ## create composite based on factor scores
#' fcomp <- factorComposite(dres, type = "onefactor")
#' ## use model to generate predictions on new data
#' predictCS(fcomp,
#'           newdata = mtcars[1:5, c("mpg", "hp", "wt", "disp")],
#'           groups = rep("one", 5))
predictCS <- function(object, newdata, groups) {

  cprep <- .preparePredict(object, newdata, groups)

  if (is(object, "SumScores")) {
    if (is.na(object@systems[[1]])) {
      comp <- sumComposite(cprep,
                           transform = object@transform,
                           type = object@type)@scores
    } else {
        comp <- sumComposite(cprep,
                             transform = object@transform,
                             type = object@type,
                             systems = object@systems)@scores
    }
  } else if (is(object, "MahalanobisScores")) {
      comp <- mahalanobisComposite(cprep,
                                   ncomponents = object@ncomponents,
                                   pca = object@pca)@scores
  } else if (is(object, "FactorScores")) {
      comp <- as.data.frame(predict(object@Fit, newdata = cprep@data))
  } else {
      stop(sprintf("Cannot handle object of class \"%s\".", class(object)))
  }

  return(comp)
}
