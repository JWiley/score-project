#' Prepare distance scores on data in preparation for composite scoring
#'
#' @param object An object of class \sQuote{CompositeData}.
#' @param winsorize Whether to winsorize the data or not.  Defaults to \code{FALSE}.
#'   If not \code{FALSE}, the percentile to winsorize at.  For example, .01 would be
#'   the .01 and the 1 - .01 percentiles.
#' @param values The values to use for winsorization.  Optional.  If specified, preempts
#'   the percentiles given by winsorize.
#' @param better Logical indicating whether \dQuote{better} values than the threshold
#'   are allowed. Defaults to \code{TRUE}.
#' @return An S4 object of class DistanceScores
#' @family prepare
#' @export
#' @examples
#' # this example creates distances for the built in mtcars data
#' # see ?mtcars for more details
#' # The distances are calculated from the "best" in the dataset
#' # First we create an appropriate CompositeData class object
#' # higher mpg & hp are better and lower wt & qsec are better
#' d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
#'   thresholds = list(one = with(mtcars, c(
#'     mpg = max(mpg),
#'     hp = max(hp),
#'     wt = min(wt),
#'     qsec = min(qsec)))
#'   ),
#'   higherisbetter = c(TRUE, TRUE, FALSE, FALSE),
#'   rawtrans = list(
#'     mpg = function(x) x,
#'     hp = function(x) x,
#'     wt = function(x) x,
#'     qsec = sqrt))
#'
#'
#' # create the distance scores
#' dres <- prepareDistances(d)
#'
#' # see a density plot of the distance scores
#' dres@@distanceDensity
#' # regular summary of distance scores
#' summary(dres@@distances)
#'
#' # cleanup
#' rm(d, dres)
prepareDistances <- function(object, winsorize = 0, values, better = TRUE) {
  # data and input checks
  n <- nrow(object@rawdata)
  ng <- length(unique(object@groups))


  ## transform thresholds to match raw data transformations
  thresholds <- object@thresholds
  thresholds <- lapply(thresholds, function(x) {
    sapply(1:object@k, function(i) {
      object@rawtrans[[i]](x[i])
    })
  })

  ## create the threshold matrix using the (transformed) thresholds
  thresholdmatrix <- t(sapply(object@groups, function(i) thresholds[[as.character(i)]]))
  rownames(thresholdmatrix) <- NULL
  colnames(thresholdmatrix) <- colnames(object@rawdata)

  ## transform raw data, prior to windsorizing
  d <- object@rawdata
  d[] <- lapply(1:object@k, function(i) {
    object@rawtrans[[i]](d[, i])
  })

  d <- winsorizor(d, percentile = winsorize, values = values, na.rm = TRUE)
  winsorizedValues <- attr(d, "winsorizedValues")

  d <- as.data.frame(sapply(1:object@k, function(i) {
    if (object@higherisbetter[i] == 0) {
      d[, i, drop = FALSE] - thresholdmatrix[, i, drop = FALSE]
    } else if (object@higherisbetter[i] == 1) {
      thresholdmatrix[, i, drop = FALSE] - d[, i, drop = FALSE]
    }
  }))

  colnames(d) <- colnames(thresholdmatrix)

  # if "better" than threshold are not allowed
  # than truncate at zero, otherwise leave as is
  if (!better) {
      d <- as.data.frame(apply(d, 2, pmax, 0))
  }

  if (n > 2) {
    dplot <- ldensity(cbind(d, Group = object@groups), melt = TRUE, g = "Group")
  } else {
    dplot <- NA
  }

  DistanceScores(
      distances = d,
      distanceDensity = dplot,
      winsorizedValues = winsorizedValues,
      better = better,
      rawdata = object@rawdata,
      groups = object@groups,
      thresholds = object@thresholds,
      higherisbetter = object@higherisbetter,
      k = object@k,
      rawtrans = object@rawtrans
      )
}


#' Prepare data to have a composite calculated
#'
#' @param object An DistanceScores class object
#' @param covmat The covariance matrix to use.  If missing,
#'   austomatically calculated from the data.
#' @param standardize A logical value whether to standardize the data or not.
#'   Defaults to \code{TRUE}.
#' @return An S4 object of class \dQuote{CompositeReady}.
#' @family prepare
#' @importFrom stats cov
#' @export
#' @examples
#' # this example creates distances for the built in mtcars data
#' # see ?mtcars for more details
#' # The distances are calculated from the "best" in the dataset
#' # First we create an appropriate CompositeData class object
#' # higher mpg & hp are better and lower wt & qsec are better
#' d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
#'   thresholds = list(one = with(mtcars, c(
#'     mpg = max(mpg),
#'     hp = max(hp),
#'     wt = min(wt),
#'     qsec = min(qsec)))
#'   ),
#'   higherisbetter = c(TRUE, TRUE, FALSE, FALSE))
#'
#' # create the distance scores
#' dres <- prepareDistances(d)
#'
#' # see a density plot of the distance scores
#' dres@@distanceDensity
#' # regular summary of distance scores
#' summary(dres@@distances)
#'
#' # now prepare to create the composite
#' # covariance matrix will be calculated from the data
#' # and data will be standardized to unit variance by default
#' cprep <- prepareComposite(dres)
#'
#' # examine covariance matrix
#' round(cprep@@covmat,2)
#'
#' # cleanup
#' rm(d, dres, cprep)
prepareComposite <- function(object, covmat, standardize = TRUE) {
    stopifnot(is(object, "DistanceScores"))

    k <- ncol(object@distances)
    if (missing(covmat)) {
        covmat <- cov(object@distances, use = "pairwise.complete.obs")
    }

    stopifnot(identical(k, ncol(covmat)))

    sigma <- sqrt(diag(covmat))

    if (standardize) {
        data <- sweep(object@distances, 2, sigma, "/")
    } else {
        data <- object@distances
    }

    CompositeReady(
      data = as.data.frame(data),
      covmat = covmat,
      sigma = sigma,
      standardize = standardize,
      distances = object@distances,
      distanceDensity = object@distanceDensity,
      winsorizedValues = object@winsorizedValues,
      better = object@better,
      rawdata = object@rawdata,
      groups = object@groups,
      thresholds = object@thresholds,
      higherisbetter = object@higherisbetter,
      k = object@k,
      rawtrans = object@rawtrans
      )
}

#' Score Data Using the Mahalanobis Distance
#'
#' Create a composite using the Mahalanobis Distance
#'
#' @param object An object of class \code{CompositeReady}
#' @param ncomponents the number of components to use from the
#'   principal component analysis. If missing, defaults to the
#'   number of columns in the data.
#' @param pca An optional PCA object from princomp to use.
#'   If not passed, will be calculated from the data.
#' @return An S4 object of class \code{MahalanobisScores}.
#' @export
#' @importFrom stats princomp
#' @family composite
#' @examples
#' # this example creates distances for the built in mtcars data
#' # see ?mtcars for more details
#' # The distances are calculated from the "best" in the dataset
#' # First we create an appropriate CompositeData class object
#' # higher mpg & hp are better and lower wt & qsec are better
#' d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
#'   thresholds = list(one = with(mtcars, c(
#'     mpg = max(mpg),
#'     hp = max(hp),
#'     wt = min(wt),
#'     qsec = min(qsec)))
#'   ),
#'   higherisbetter = c(TRUE, TRUE, FALSE, FALSE))
#'
#' # create the distance scores
#' dres <- prepareDistances(d)
#'
#' # see a density plot of the distance scores
#' dres@@distanceDensity
#' # regular summary of distance scores
#' summary(dres@@distances)
#'
#' # now prepare to create the composite
#' # covariance matrix will be calculated from the data
#' # and data will be standardized to unit variance by default
#' cprep <- prepareComposite(dres)
#'
#' # examine covariance matrix
#' round(cprep@@covmat,2)
#'
#' # now we can create the composite based on mahalanobis distances
#' # from our defined thresholds
#' mcomp <- mahalanobisComposite(cprep, 1)
#'
#' # view a histogram of the composite scores
#' mcomp@@scoreHistogram
#'
#' # summarize the composite scores
#' summary(mcomp@@scores)
#'
#' # check the screeplot and loadings
#' mcomp@@screePlot
#' mcomp@@loadingGraph
#' # examine the loadings as a table
#' mcomp@@loadingTable
#'
#' # one component is adequate to explain these data
#' # to be safe can pick first two and re-run model
#'
#' # use only first two components
#' mcomp2 <- mahalanobisComposite(cprep, ncomponents = 2)
#'
#' # view a histogram of the updated composite scores
#' mcomp2@@scoreHistogram
#'
#' # summarize the composite scores
#' summary(mcomp2@@scores)
#'
#' # compare using all versus two components
#' plot(mcomp@@scores, mcomp2@@scores)
#'
#' # cleanup
#' rm(d, dres, cprep, mcomp, mcomp2)
mahalanobisComposite <- function(object, ncomponents, pca) {
  stopifnot(is(object, "CompositeReady"))

  if (missing(ncomponents)) {
    ncomponents <- object@k
  }

  stopifnot(ncomponents <= object@k)

  if (missing(pca)) {
    pca <- princomp(scores = FALSE, cor = object@standardize, covmat = object@covmat)
  }

  screeplot <- ggplot(data.frame(Component = 1:object@k, Eigenvalue = pca$sdev^2),
    aes(Component, Eigenvalue)) +
    geom_line() + geom_point() +
    geom_hline(aes(yintercept = 1)) +
    scale_x_continuous(breaks = 1:object@k) +
    theme_classic()

  Lbase <- as.matrix(pca$loadings[])

  ltab <- Lbase
  colnames(ltab) <- paste0("C", 1:object@k)
  ltab[] <- format(round(ltab, 2), nsmall=2, digits=2)
  ltab <- ltab[, 1:ncomponents, drop = FALSE]

  L <- as.data.frame(pca$loadings[, 1:ncomponents, drop = FALSE])
  colnames(L) <- paste0("Comp", 1:ncomponents)
  L$Variable <- factor(rownames(L), levels = colnames(object@data)[1:object@k])
  L <- melt(L, id.vars = "Variable")

  loadingsplot <- ggplot(L, aes(Variable, abs(value), fill = ifelse(value > 0, "positive", "negative"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual("Direction", values = c("positive" = "black", "negative" = "grey80")) +
    facet_grid(variable ~ .) +
    ylab("Absolute Loading") +
    xlab("") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust=.5))

  c.scores <- as.matrix(object@data) %*% Lbase %*% diag(1/pca$sdev)
  c.scores <- as.data.frame(c.scores)
  colnames(c.scores) <- paste0("C", 1:object@k)

  finalScores <- sqrt(rowSums(c.scores[, 1:ncomponents, drop = FALSE]^2))
  # alternate way
  # finalScores <- sqrt(rowSums((data %*% solve(cov2cor(covmat))) * data))

  class(pca) <- "list"

  new("MahalanobisScores",
      scores = finalScores,
      scoreHistogram = ldensity(data.frame(Scores = finalScores), x = "Scores", hist = TRUE),
      screePlot = screeplot,
      loadingGraph = loadingsplot,
      loadingTable = ltab,
      pca = pca,
      ncomponents = ncomponents,
      CompositeReady = object)
}

# clear R CMD CHECK notes
if(getRversion() >= "2.15.1")  utils::globalVariables(c("Component", "Eigenvalue", "Variable", "value"))

#' Score Data Using a simple sum
#'
#' Create a composite using summation
#'
#' @param object An object of class \code{CompositeReady}
#' @param transform A character string indicating the type of transformation to use.
#'   One of \dQuote{square}, \dQuote{abs}, or \dQuote{none}, which either sums the raw data,
#'   sums the squared data and then takes the square root, or sums the absolute values of the
#'   data.
#' @param type A character string indicating the type of aggregation to use.
#'   One of \dQuote{sum} or \dQuote{mean}.
#' @param systems An optional list where each element is a character vector of the
#'   variable names within a particular system.  If given, scores are first averaged
#'   within a system, before being aggregated across systems.
#' @return An S4 object of class \code{SumScores}.
#' @export
#' @family composite
#' @examples
#' # this example creates distances for the built in mtcars data
#' # see ?mtcars for more details
#' # The distances are calculated from the "best" in the dataset
#' # First we create an appropriate CompositeData class object
#' # higher mpg & hp are better and lower wt & qsec are better
#' d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
#'   thresholds = list(one = with(mtcars, c(
#'     mpg = max(mpg),
#'     hp = max(hp),
#'     wt = min(wt),
#'     qsec = min(qsec)))
#'   ),
#'   higherisbetter = c(TRUE, TRUE, FALSE, FALSE))
#'
#' # create the distance scores
#' dres <- prepareDistances(d)
#'
#' # see a density plot of the distance scores
#' dres@@distanceDensity
#' # regular summary of distance scores
#' summary(dres@@distances)
#'
#' # now prepare to create the composite
#' # covariance matrix will be calculated from the data
#' # and data will be standardized to unit variance by default
#' cprep <- prepareComposite(dres)
#'
#' # examine covariance matrix
#' round(cprep@@covmat,2)
#'
#' # now we can create the composite based on summing the (standardized)
#' # distances from our defined thresholds
#' # by default, distances are squared, then summed, and then square rooted
#' # to be back on the original scale
#' scomp <- sumComposite(cprep, "square", "sum")
#'
#' # view a histogram and summary of the composite scores
#' scomp@@scoreHistogram
#' summary(scomp@@scores)
#'
#' # calculate average (mean) instead of sum
#' scomp2 <- sumComposite(cprep, "square", "mean")
#'
#' # view a histogram and summary of the composite scores
#' scomp2@@scoreHistogram
#' summary(scomp2@@scores)
#'
#' # scores are still the same (just different scaling)
#' plot(scomp@@scores, scomp2@@scores)
#'
#' # first average scores within a system, then sum
#' # within a system, scores are always averaged, never summed
#' scomp3 <- sumComposite(cprep, "square", "sum",
#'   systems = list(
#'     environment = c("mpg"),
#'     performance = c("hp", "qsec", "wt")))
#'
#' # view a histogram and summary of the composite scores
#' scomp3@@scoreHistogram
#' summary(scomp3@@scores)
#'
#' # compare all three scores
#' # because of the different number of indicators within each system
#' # there is a re-weighting for S3
#' plot(data.frame(S1 = scomp@@scores, S2 = scomp2@@scores, S3 = scomp3@@scores))
#'
#' # cleanup
#' rm(d, dres, cprep, scomp, scomp2, scomp3)
sumComposite <- function(object, transform = c("square", "abs", "none"), type = c("sum", "mean"), systems) {
  stopifnot(is(object, "CompositeReady"))

  transform <- match.arg(transform)
  type <- match.arg(type)

  aggregator <- switch(type,
                       sum = rowSums,
                       mean = rowMeans)
  ## switch function to transform data (to) and backtransform (back)
  trans <- switch(transform,
               square = list(to = function(x) x^2, back = sqrt),
               abs = list(to = abs, back = I),
               none = list(to = I, back = I))

  if (!missing(systems)) {
      x <- do.call(cbind, lapply(systems, function(v) {
        rowMeans(trans$to(object@data[, v, drop = FALSE]))
      }))
      finalScores <- trans$back(aggregator(x))
  } else {
      finalScores <- trans$back(aggregator(trans$to(object@data)))
      systems <- list(NA_character_)
  }

  new("SumScores",
      scores = as.numeric(finalScores),
      scoreHistogram = ldensity(data.frame(Scores = finalScores), x = "Scores", hist = TRUE),
      transform = transform,
      type = type,
      trans = trans,
      systems = systems,
      CompositeReady = object)
}


#' Score Data Using a Factor Model
#'
#' Create a composite using a Factor Model
#'
#' @param object An object of class \code{CompositeReady}
#' @param type A character string indicating the type of factor model to use
#' @param factors A named list where names are the factor names and each
#'   element is a character string of the indicator names.
#' @return An S4 object of class \code{FactorScores}.
#' @import lavaan
#' @export
#' @family composite
#' @examples
#' # this example creates distances for the built in mtcars data
#' # see ?mtcars for more details
#' # The distances are calculated from the "best" in the dataset
#' # First we create an appropriate CompositeData class object
#' # higher mpg & hp are better and lower wt & qsec are better
#' d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
#'   thresholds = list(one = with(mtcars, c(
#'     mpg = max(mpg),
#'     hp = max(hp),
#'     wt = min(wt),
#'     qsec = min(qsec)))
#'   ),
#'   higherisbetter = c(TRUE, TRUE, FALSE, FALSE))
#'
#' # create the distance scores
#' dres <- prepareDistances(d)
#'
#' # see a density plot of the distance scores
#' dres@@distanceDensity
#' # regular summary of distance scores
#' summary(dres@@distances)
#'
#' # now prepare to create the composite
#' # covariance matrix will be calculated from the data
#' # and data will be standardized to unit variance by default
#' cprep <- prepareComposite(dres)
#'
#' # examine covariance matrix
#' round(cprep@@covmat,2)
#'
#' # now we can create the composite based on summing the (standardized)
#' # distances from our defined thresholds
#' # by default, distances are squared, then summed, and then square rooted
#' # to be back on the original scale
#' fcomp <- factorComposite(cprep, type = "onefactor")
#'
#' # view a histogram of the composite scores
#' fcomp@@scoreHistogram
#'
#' # summarize the composite scores
#' summary(fcomp@@scores)
#'
#' # we can also fit a second-order factor model
#' # there are not enough indicators to identify the factor
#' # and so lavaan gives us warning messages
#' fcomp2 <- factorComposite(cprep, type = "secondorderfactor",
#'   factors = list(speed = c("hp", "qsec")))
#'
#' # view a histogram of the composite scores
#' fcomp2@@scoreHistogram
#'
#' # summarize the composite scores
#' summary(fcomp2@@scores)
#'
#' # compare one and second-order factor model scores
#' plot(fcomp@@scores, fcomp2@@scores)
#'
#' # cleanup
#' rm(d, dres, cprep, fcomp, fcomp2)
factorComposite <- function(object, type = c("onefactor", "secondorderfactor", "bifactor"), factors = list(NA_character_)) {
    stopifnot(is(object, "CompositeReady"))
    type <- match.arg(type)

    vars <- colnames(object@data)
    unused <- setdiff(vars, unlist(factors))

    onefactor <- paste0("Composite =~ ", paste(vars, collapse = " + "))

    m.factors <- paste(sapply(1:length(factors), function(i) {
        sprintf("%s =~ %s", names(factors)[i],
                ifelse(length(factors[[i]]) < 3,
                       paste(paste0("b", i, "1", " * ", factors[[i]]), collapse = " + "),
                       paste(paste0("b", i, 1:length(factors[[i]]), " * ", factors[[i]]), collapse = " + ")))
    }), collapse = "\n")

    if (length(factors) > 1) {
        m.factors.covariances <- sapply(1:(length(factors) - 1), function(i) {
            sapply((i + 1):(length(factors)), function(j) {
                sprintf("%s ~~ 0 * %s", names(factors)[i], names(factors)[j])
            })
        })
    } else {
        m.factors.covariances <- ""
    }

    m.overall <- paste0("Composite =~ ", paste(c(names(factors), unused), collapse = " + "))

    secondorderfactor <- paste(m.factors, m.overall, sep = "\n")

    bifactor <- paste(
        m.factors,
        onefactor,
        paste(paste("Composite ~~ 0 * ", names(factors)), collapse = "\n"),
        paste(unlist(m.factors.covariances), collapse = "\n"),
        sep = "\n")

    fit <- switch(type,
                  onefactor = sem(onefactor, data = object@data),
                  secondorderfactor = sem(secondorderfactor, data = object@data),
                  bifactor = sem(bifactor, data = object@data))

    factorScores <- as.data.frame(predict(fit))
    finalScores <- factorScores$Composite

    new("FactorScores",
      scores = finalScores,
      scoreHistogram = ldensity(data.frame(Scores = finalScores), x = "Scores", hist = TRUE),
      factorScores = factorScores,
      type = type,
      factors = factors,
      Fit = fit,
      CompositeReady = object)
}
