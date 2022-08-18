library(MASS)
library(magrittr)
library(dplyr)
library(ggplot2)

set.seed(123)
n <- 10000
R <- matrix(c(1, 0.9,
              0.9, 1),
            nrow = 2, ncol = 2, byrow = TRUE)

mu <- c(X = 0, Y = 0)
test_data <- MASS::mvrnorm(n, mu = mu, Sigma = R) %>%
  as.data.frame() %>%
  rename(Z = Y) %>%
  mutate(Y_l = X + Z) %>%
  mutate(Y_nl = sin(2 * X) + Z)


test_that("saves files", {

  BinScatter(data = test_data,
             x_var = "X",
             y_var = "Y_nl",
             binpartial_var = "Z",
             partialBinType = "quantile")
  expect_false(file.exists("./non/existent/path.pdf"))
  expect_false(file.exists("./non/existent/path.txt"))

  BinScatter(data = test_data,
             x_var = "X",
             y_var = "Y_nl",
             plot_path = "path",
             tab_path = "path")
  expect_true(file.exists("./path.pdf"))
  expect_true(file.exists("./path.txt"))
})

test_that("ci", {

  code <- function(ci) {
    BinScatter(data = test_data,
               nBins = 20,
               x_var = "X",
               y_var = "Y_nl",
               ci = ci)
  }
  expect_warning(code(0.8), regexp = NA)
  expect_warning(code(1.0), regexp = "set to 0.95")
  expect_warning(code(0.0), regexp = "set to 0.95")
  expect_warning(code("c"), regexp = "set to 0.95")

})

test_that("dropNA", {

  test_data_na <- test_data
  test_data_na[c(1:5), "X"] <- NA
  test_data_na[c(5:9), "Y_nl"] <- NA

  expect_error(
    BinScatter(data = test_data_na,
               x_var = "X",
               y_var = "Y_nl",
               dropNA = FALSE),
    "Missing values present"
  )

  expect_warning(
    BinScatter(data = test_data_na,
               x_var = "X",
               y_var = "Y_nl",
               dropNA = TRUE),
    "9 observations dropped"
  )

})


test_that("intercept", {

  code <- function(intercept) {

    return (BinScatter(data = test_data,
               x_var = "X",
               y_var = "Y_nl",
               binpartial_var = "Z",
               partialBinType = "uniform",
               ci = 0.95,
               intercept = intercept,
               scale_yvar = TRUE,
            plot_xlim=c(-3,3),
            plot_ylim=c(-2, 2)))
  }
  
  expect_true("(Intercept)" %in% names(code(TRUE)[[1]]$coefficients))
  expect_false("X_bin_1" %in% names(code(TRUE)[[1]]$coefficients))
  expect_false("(Intercept)" %in% names(code(FALSE)[[1]]$coefficients))
  expect_true("X_bin_1" %in% names(code(FALSE)[[1]]$coefficients))

})

test_that("axis limits", {
    results <- BinScatter(data = test_data,
                       x_var = "X",
                       y_var = "Y_nl",
                       binpartial_var = "Z",
                       partialBinType = "uniform",
                       plot_xlim=c(-3,3),
                       plot_ylim=c(-2, 2))
    build <- ggplot_build(results[[2]])
    expect_equal(build$layout$panel_params[[1]]$x$limits, c(-3,3))
    expect_equal(build$layout$panel_params[[1]]$y$limits, c(-2,2))
})
