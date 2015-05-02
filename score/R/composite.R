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
#' @import ggplot2 reshape2
#' @examples
#' # simple facetted plot
#' ldensity(mtcars, TRUE)
#' # simple coloured plot
#' ldensity(mtcars, x = "mpg", g = "factor(cyl)")
ldensity <- function(data, melt = FALSE, x, facet, g, hist=FALSE) {
    data <- as.data.frame(data)
    if (melt) {
        if (!missing(g)) {
            data <- melt(data, id.var = g)
        } else {
            data <- melt(data)
        }
        x <- "value"
        facet <- ~variable
    }

    if (!missing(g)) {
        p <- ggplot(data, aes_string(x = x, color = g))
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

#' Winsorize at specified percentiles
#'
#' Simple function winsorizes data at the specified percentile.
#'
#' @param d A vector, matrix, or data frame to be winsorized
#' @param percentile The percentile bounded by [0, 1] to winsorize data at
#' @param na.rm A logical whether to remove NAs.
#' @return winsorized data.
#' @export
#' @examples
#' #make me!
winsorizor <- function(d, percentile, na.rm = TRUE) {
    stopifnot(percentile >= 0 && percentile <= 1)

    f <- function(x, percentile, na.rm) {
          low <- quantile(x, probs = 0 + percentile, na.rm = na.rm)
          high <- quantile(x, probs = 1 - percentile, na.rm = na.rm)
          pmin(pmax(x, low), high)
    }

    if (is.vector(d)) {
        d <- f(d, percentile = percentile, na.rm = na.rm)
    } else if (is.matrix(d) || is.data.frame(d)) {
        d <- as.data.frame(apply(d, 2, f, percentile = percentile, na.rm = na.rm))
    }
    return(d)
}


#' Calculate distance scores on data in preparation for composite scoring
#'
#' @param d The data
#' @param g A grouping variable
#' @param thresholds Thresholds to use when calculating distances
#'   (e.g., median, clinical thresholds, etc.).  If groups are used,
#'   must be thresholds for each group (e.g., to allow separate thresholds for
#'   females and males).
#' @param higherisbetter A logical vector for each biomarker whether higher scores
#'   are better or not.
#' @param winsorize Whether to winsorize the data or not.  Defaults to \code{FALSE}.
#'   If not \code{FALSE}, the percentile to winsorize at.  For example, .01 would be
#'   the .01 and the 1 - .01 percentiles.
#' @param better Logical indicating whether \dQuote{better} values than the threshold are allowed.
#' @param na.rm A logical whether missing values should be ommitted
#' @param saveall A logical whether to save all intermediary datasets and graphs. Defaults to \code{FALSE}.
#' @return A list of results.
#' @export
#' @examples
#' # make me!
distanceScores <- function(d, g, thresholds, higherisbetter, winsorize = FALSE, better = TRUE, na.rm = TRUE, saveall = FALSE) {
  fcall <- match.call()

  # data and input checks
  stopifnot(all(apply(d, 2, is.numeric)))
  k <- ncol(d)

  if (missing(higherisbetter)) {
      higherisbetter <- rep(0, k)
  }

  if (missing(thresholds)) {
      thresholds <- list(rep(0, k))
  }

  if (missing(g)) {
      g <- rep("1", nrow(d))
      stopifnot(identical(length(thresholds), 1L))
      if (is.null(names(thresholds))) {
        names(thresholds) <- "1"
      }
  }
  ng <- length(unique(g))

  stopifnot(identical(length(higherisbetter), k))
  stopifnot(all(sapply(thresholds, length) == k))
  stopifnot(identical(length(g), nrow(d)))

  # handle missing values
  if (na.rm) {
      okindex <- which(rowSums(is.na(cbind(d, g))) == 0)
      d <- d[okindex, ]
      g <- g[okindex]
      if (!identical(length(unique(g)), ng)) {
          warning("After removing missing values, levels of grouping variable no longer equal")
      }
  }

  if (any(unlist(thresholds) != 0) & saveall) d.original <- d


  # create the threshold matrix
  thresholdmatrix <- t(sapply(g, function(i) thresholds[[as.character(i)]]))
  rownames(thresholdmatrix) <- NULL
  colnames(thresholdmatrix) <- colnames(d)

  if (winsorize) {
      d <- winsorizor(d, percentile = winsorize, na.rm = TRUE)
      if (saveall) d.winsorize <- d
  }


  d <- as.data.frame(sapply(1:k, function(i) {
    if (higherisbetter[i] == 0) {
      d[, i] - thresholdmatrix[, i]
    } else if (higherisbetter[i] == 1) {
      thresholdmatrix[, i] - d[, i]
    }
  }))

  colnames(d) <- colnames(thresholdmatrix)


  if (saveall) d.distances <- d

  # if "better" than threshold are not allowed
  # than truncate at zero, otherwise leave as is
  if (!better) {
      d <- as.data.frame(apply(d, 2, pmax, 0))
  }

  if (saveall) {
    data <- list(
      Distance = if(!better) d.distances else NULL,
      Winsorized = if(winsorize) d.winsorize else NULL,
      Raw = if(exists("d.original")) d.original else NULL)

    plots <- list(
      Distance = if(!better) ldensity(cbind(d.distances, Group = g), melt = TRUE, g = "Group") else NULL,
      Winsorized = if(winsorize) ldensity(cbind(d.winsorize, Group = g), melt = TRUE, g = "Group") else NULL,
      Raw = if(exists("d.original")) ldensity(cbind(d.original, Group = g), melt = TRUE, g = "Group") else NULL)
  } else {
    plots <- data <- list(Distance = NULL, Windsorized = NULL, Raw = NULL)
  }

  out <- list(
      Distances = d,
      Density = ldensity(cbind(d, Group = g), melt = TRUE, g = "Group"),
      Groups = g,
      SavedPlots = plots, SavedData = data,
      thresholds = thresholds, higherisbetter = higherisbetter,
      winsorize = winsorize, better = better, na.rm = na.rm,
      call = fcall)
  class(out) <- c("distancescores", "list")

  return(out)
}



#' Prepare data to have a composite calculated
#'
#' @param object An object ready for use
#' @param covmat The covariance matrix to use.  If missing,
#'   austomatically calculated from the data.
#' @param standardize A logical value whether to standardize the data or not.
#' @return A list of results.
#' @export
#' @examples
#' # make me!
prepareComposite <- function(object, covmat, standardize = TRUE) {
    k <- ncol(object$Distances)
    if (missing(covmat)) {
        covmat <- cov(object$Distances)
    }

    stopifnot(identical(k, ncol(covmat)))

    sigma <- sqrt(diag(covmat))

    if (standardize) {
        data <- sweep(object$Distances, 2, sigma, "/")
    } else {
        data <- object$Distances
    }

    out <- c(list(
        data = data,
        covmat = covmat,
        sigma = sigma,
        standardize = standardize,
        k = k), object)

    class(out) <- c("compositedata", "list")

    return(out)
}


#' Score Data Using the Mahalanobis Distance
#'
#' Create Metabolic Syndrome Severity Score using the Mahalanobis Distance
#'
#' @param object An object ready for use
#' @param ncomponents the number of components to use from the
#'   principal component analysis
#' @return A list of results.
#' @export
#' @examples
#' # make me!
mahalanobisComposite <- function(object, ncomponents) {
  if (missing(ncomponents)) {
    ncomponents <- object$k
  }

  stopifnot(ncomponents <= object$k)

  pca <- princomp(scores = FALSE, cor = object$standardize, covmat = object$covmat)

  screeplot <- ggplot(data.frame(Component = 1:object$k, Eigenvalue = pca$sdev^2),
    aes(Component, Eigenvalue)) +
    geom_line() + geom_point() +
    geom_hline(aes(yintercept = 1)) +
    scale_x_continuous(breaks = 1:object$k) +
    theme_classic()

  Lbase <- as.matrix(pca$loadings[])

  ltab <- Lbase
  colnames(ltab) <- paste0("C", 1:object$k)
  ltab[] <- format(round(ltab, 2), nsmall=2, digits=2)

  L <- as.data.frame(pca$loadings[, 1:ncomponents])
  colnames(L) <- paste0("Comp", 1:ncomponents)
  L$Variable <- factor(rownames(L), levels = colnames(object$data)[1:ncomponents])
  L <- melt(L, id.vars = "Variable")

  loadingsplot <- ggplot(L, aes(Variable, abs(value), fill = ifelse(value > 0, "positive", "negative"))) +
    geom_bar(stat = "identity") +
    scale_fill_manual("Direction", values = c("positive" = "black", "negative" = "grey80")) +
    facet_grid(variable ~ .) +
    ylab("Absolute Loading") +
    xlab("") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust=.5))

  c.scores <- as.matrix(object$data) %*% Lbase %*% diag(1/pca$sdev)
  c.scores <- as.data.frame(c.scores)
  colnames(c.scores) <- paste0("C", 1:object$k)

  finalScores <- sqrt(rowSums(c.scores[, 1:ncomponents]^2))
  # alternate way
  # finalScores <- sqrt(rowSums((data %*% solve(cov2cor(covmat))) * data))

  out <- c(list(
      Scores = finalScores,
      ScoreHistogram = ldensity(data.frame(Scores = finalScores), x = "Scores", hist = TRUE),
      Screeplot = screeplot,
      LoadingGraph = loadingsplot,
      LoadingTable = ltab), object)
  class(out) <- c("mahalanobiscomposite", "list")

  return(out)
}


#' Score Data Using a simple sum
#'
#' Create Metabolic Syndrome Severity Score using summation
#'
#' @param object An object ready for use
#' @param transformation A character string indicating the type of transformation to use.
#'   One of \dQuote{square}, \dQuote{abs}, or \dQuote{none}, which either sums the raw data,
#'   sums the squared data and then takes the square root, or sums the absolute values of the
#'   data.
#' @return A list of results.
#' @export
#' @examples
#' # make me!
scoreComposite <- function(object, transform = c("square", "abs", "none")) {
## TODO: extend to allow for averaging within systems
## first before averaging across systems
  transform <- match.arg(transform)

  finalScores <- switch(transform,
                        square = sqrt(rowSums(object$data^2)),
                        abs = rowSums(abs(object$data)),
                        none = rowSums(object$data))

  out <- c(list(
      Scores = finalScores,
      ScoreHistogram = ldensity(data.frame(Scores = finalScores), x = "Scores", hist = TRUE),
      transform = transform), object)
  class(out) <- c("scorecomposite", "list")

  return(out)
}


#' Score Data Using a Factor Model
#'
#' Create Metabolic Syndrome Severity Score using a Factor Model
#'
#' @param object An object ready for use
#' @param type A character string indicating the type of factor model to use
#' @param factors A named list where names are the factor names and each
#'   element is a character string of the indicator names.
#' @return A list of results.
#' @import lavaan
#' @export
#' @examples
#' # make me!
scoreFactor <- function(object, type = c("onefactor", "secondorderfactor", "bifactor"), factors = "") {
    type <- match.arg(type)

    vars <- colnames(object$data)
    unused <- setdiff(vars, unlist(factors))

    onefactor <- paste0("MSSS =~ ", paste(vars, collapse = " + "))

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

    m.overall <- paste0("MSSS =~ ", paste(c(names(factors), unused), collapse = " + "))

    secondorderfactor <- paste(m.factors, m.overall, sep = "\n")

    bifactor <- paste(
        m.factors,
        onefactor,
        paste(paste("MSSS ~~ 0 * ", names(factors)), collapse = "\n"),
        paste(unlist(m.factors.covariances), collapse = "\n"),
        sep = "\n")

    fit <- switch(type,
                  onefactor = sem(onefactor, data = object$data),
                  secondorderfactor = sem(secondorderfactor, data = object$data),
                  bifactor = sem(bifactor, data = object$data))

    list(
        Fit = fit,
        Scores = as.data.frame(predict(fit)))
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
