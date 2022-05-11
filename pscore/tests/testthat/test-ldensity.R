test_that("ldensity produces a ggplot2 graph for all permutations of options", {
              expect_is(pscore:::ldensity(mtcars, TRUE), "gg")
              expect_is(pscore:::ldensity(mtcars, TRUE, g= "cyl"), "gg")
              expect_is(pscore:::ldensity(mtcars, x = "mpg"), "gg")
              expect_is(pscore:::ldensity(mtcars, x = "mpg", facet = ~ cyl), "gg")
              expect_is(pscore:::ldensity(mtcars, x = "mpg", g = "cyl"), "gg")
              expect_is(pscore:::ldensity(mtcars, x = "mpg", facet = ~ vs, g = "cyl"), "gg")

              expect_is(pscore:::ldensity(mtcars, TRUE, hist = TRUE), "gg")
              expect_is(pscore:::ldensity(mtcars, TRUE, g= "cyl", hist = TRUE), "gg")
              expect_is(pscore:::ldensity(mtcars, x = "mpg", hist = TRUE), "gg")
              expect_is(pscore:::ldensity(mtcars, x = "mpg", facet = ~ cyl, hist = TRUE), "gg")
              expect_is(pscore:::ldensity(mtcars, x = "mpg", g = "cyl", hist = TRUE), "gg")
              expect_is(pscore:::ldensity(mtcars, x = "mpg", facet = ~ vs, g = "cyl", hist = TRUE), "gg")
})


test_that("ldensity fails if invalid options are used", {
              expect_error(pscore:::ldensity(mtcars, TRUE, x = "mpg"))
              expect_error(pscore:::ldensity(mtcars, TRUE, facet = ~ cyl))
})
