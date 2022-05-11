test_that("predictions using a sum score model on the same raw data, yield the same scores", {
              d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
                                 thresholds = list(one = with(mtcars, c(
                                                       mpg = max(mpg),
                                                       hp = max(hp),
                                                       wt = min(wt),
                                                       qsec = min(qsec)))),
                                 higherisbetter = c(TRUE, TRUE, FALSE, FALSE))
              ## create the distance scores
              ## prepare to create the composite
              dres <- prepareComposite(d)

              ## create composite based on summing the (standardized)
              scomp <- sumComposite(dres, "square", "sum")
              ## use model to generate predictions on new data
              yhat <- predictCS(scomp,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, scomp@scores[1])

              ## create composite based on summing the (standardized)
              scomp <- sumComposite(dres, "abs", "sum")
              ## use model to generate predictions on new data
              yhat <- predictCS(scomp,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, scomp@scores[1])

              ## create composite based on summing the (standardized)
              scomp <- sumComposite(dres, "none", "sum")
              ## use model to generate predictions on new data
              yhat <- predictCS(scomp,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, scomp@scores[1])

              ## create composite based on summing the (standardized)
              scomp <- sumComposite(dres, "square", "mean")
              ## use model to generate predictions on new data
              yhat <- predictCS(scomp,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, scomp@scores[1])

              ## create composite based on summing the (standardized)
              scomp <- sumComposite(dres, "square", "sum",
                                    systems = list(
                                        environment = c("mpg"),
                                        performance = c("hp", "qsec", "wt")))
              ## use model to generate predictions on new data
              yhat <- predictCS(scomp,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, scomp@scores[1])


              ## create composite based on summing the (standardized)
              scomp <- sumComposite(dres, "none", "mean",
                                    systems = list(
                                        environment = c("mpg"),
                                        performance = c("hp", "qsec", "wt")))
              ## use model to generate predictions on new data
              yhat <- predictCS(scomp,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, scomp@scores[1])


              d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
                                 thresholds = list(one = with(mtcars, c(
                                                       mpg = max(mpg),
                                                       hp = max(hp),
                                                       wt = min(wt),
                                                       qsec = min(qsec)))),
                                 higherisbetter = c(TRUE, TRUE, FALSE, FALSE),
                                 rawtrans = list(
                                     mpg = function(x) x^4, # extreme on purpose
                                     hp = function(x) x,
                                     wt = function(x) x,
                                     qsec = sqrt))

              ## create the distance scores
              ## prepare to create the composite
              dres <- prepareComposite(d)

              ## create composite based on summing the (standardized)
              scomp2 <- sumComposite(dres, "square", "sum")
              ## use model to generate predictions on new data
              yhat <- predictCS(scomp2,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, scomp2@scores[1])

})



test_that("predictions using a Mahalanobis Distance score model on the same raw data, yield the same scores", {
              d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
                                 thresholds = list(one = with(mtcars, c(
                                                       mpg = max(mpg),
                                                       hp = max(hp),
                                                       wt = min(wt),
                                                       qsec = min(qsec)))),
                                 higherisbetter = c(TRUE, TRUE, FALSE, FALSE))
              ## create the distance scores
              ## prepare to create the composite
              dres <- prepareComposite(d)

              ## create composite based on mahalanobis distance
              mcomp <- mahalanobisComposite(dres)
              ## use model to generate predictions on new data
              yhat <- predictCS(mcomp,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, mcomp@scores[1])

              ## create composite based on mahalanobis distance
              mcomp <- mahalanobisComposite(dres, 2)
              ## use model to generate predictions on new data
              yhat <- predictCS(mcomp,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, mcomp@scores[1])


              d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "qsec")],
                                 thresholds = list(one = with(mtcars, c(
                                                       mpg = max(mpg),
                                                       hp = max(hp),
                                                       wt = min(wt),
                                                       qsec = min(qsec)))),
                                 higherisbetter = c(TRUE, TRUE, FALSE, FALSE),
                                 rawtrans = list(
                                     mpg = function(x) x^4, # extreme on purpose
                                     hp = function(x) x,
                                     wt = function(x) x,
                                     qsec = sqrt))

              ## create the distance scores
              ## prepare to create the composite
              dres <- prepareComposite(d)

              ## create composite based on mahalanobis distance
              mcomp2 <- mahalanobisComposite(dres, 2)
              ## use model to generate predictions on new data
              yhat <- predictCS(mcomp2,
                                newdata = mtcars[1, c("mpg", "hp", "wt", "qsec")],
                                groups = "one")
              expect_equivalent(yhat, mcomp2@scores[1])

})

test_that("predictions using a Factor score model on the same raw data, yield the same scores", {
              d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "disp")],
                                 thresholds = list(one = with(mtcars, c(
                                                       mpg = max(mpg),
                                                       hp = max(hp),
                                                       wt = min(wt),
                                                       disp = min(disp)))),
                                 higherisbetter = c(TRUE, TRUE, FALSE, FALSE))
              ## create the distance scores
              ## prepare to create the composite
              dres <- prepareComposite(d)

              ## create composite based on mahalanobis distance
              fcomp <- factorComposite(dres, type = "onefactor")
              ## use model to generate predictions on new data
              yhat <- predictCS(fcomp,
                                newdata = mtcars[1:5, c("mpg", "hp", "wt", "disp")],
                                groups = rep("one", 5))
              expect_equivalent(yhat$Composite, fcomp@scores[1:5])

              d <- CompositeData(mtcars[, c("mpg", "hp", "wt", "disp")],
                                 thresholds = list(one = with(mtcars, c(
                                                       mpg = max(mpg),
                                                       hp = max(hp),
                                                       wt = min(wt),
                                                       disp = min(disp)))),
                                 higherisbetter = c(TRUE, TRUE, FALSE, FALSE),
                                 rawtrans = list(
                                     mpg = function(x) x^4, # extreme on purpose
                                     hp = function(x) x,
                                     wt = function(x) x,
                                     qsec = sqrt))

              ## create the distance scores
              ## prepare to create the composite
              dres <- prepareComposite(d)

              ## create composite based on mahalanobis distance
              fcomp2 <- factorComposite(dres, type = "onefactor")
              ## use model to generate predictions on new data
              yhat <- predictCS(fcomp2,
                                newdata = mtcars[1:5, c("mpg", "hp", "wt", "disp")],
                                groups = rep("one", 5))
              expect_equivalent(yhat$Composite, fcomp2@scores[1:5])

})
