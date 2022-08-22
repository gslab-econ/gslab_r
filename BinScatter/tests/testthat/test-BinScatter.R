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
          linpartial_var = "Y_l")
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


test_that("accepts vector of variables", {
  
})


test_that("enforces minimum observations per bin", {
  expect_error(
    BinScatter(data = test_data[c(1:100),],
            x_var = "X",
            y_var = "Y_nl",
            binpartial_var = "Z",
            min_obs = 5,
            min_partial_obs = 20),
    "bins of Z have insufficient observations"
  )  
  expect_error(
    BinScatter(data = test_data[c(1:100),],
         x_var = "X", 
         y_var = "Y_nl"),
    "bins of X have insufficient observations"
  )
  expect_error(
    BinScatter(data = test_data[c(1:100),],
            x_var = "X", 
            y_var = "Y_nl", 
            n_bins = 10),
    NA
  )
  expect_error(
    BinScatter(data = test_data[c(1:100),],
            x_var = "X", 
            y_var = "Y_nl", 
            min_obs = 5),
    NA
  )

})
test_that("sets correct level for confidence intervals", {

  code <- function(ci) {
    BinScatter(data = test_data,
               n_bins = 20,
               x_var = "X",
               y_var = "Y_nl",
               ci = ci)
  }
  expect_warning(code(0.8), regexp = NA)
  expect_warning(code(1.0), regexp = "set to 0.95")
  expect_warning(code(0.0), regexp = "set to 0.95")
  expect_warning(code("c"), regexp = "set to 0.95")

})

test_that("outputs correct messages for drop_na", {

  test_data_na <- test_data
  test_data_na[c(1:5), "X"] <- NA
  test_data_na[c(5:9), "Y_nl"] <- NA

  expect_error(
    BinScatter(data = test_data_na,
               x_var = "X",
               y_var = "Y_nl",
               drop_na = FALSE),
    "Missing values present"
  )
  expect_warning(
    BinScatter(data = test_data_na,
               x_var = "X",
               y_var = "Y_nl",
               drop_na = TRUE),
    "9 observations dropped"
  )
  expect_warning(
    BinScatter(data = test_data_na %>% drop_na(),
            x_var = "X",
            y_var = "Y_nl",
            drop_na = FALSE),
    NA
  )
  expect_warning(
    BinScatter(data = test_data_na %>% drop_na(),
            x_var = "X",
            y_var = "Y_nl",
            drop_na = TRUE),
    NA
  )
})

test_that("", {
  
  code <- function(intercept, recenter, ci = NULL) {
    BinScatter(data = test_data,
            x_var = "X",
            y_var = "Y_nl",
            binpartial_var = "Z",
            intercept = intercept,
            recenter = recenter,
            ci = ci)
  }
  
  tol <- 1e-7
  expect_true(var(code(T, F)[,1] - code(F, F)[,1]) < tol)
  expect_true(var(code(T, T)[,1] - code(T, F)[,1]) < tol)
  expect_true(var(code(T, T)[,1] - code(F, F)[,1]) < tol)
  expect_true(var(code(T, F)[,1] - code(F, T)[,1]) < tol)
  expect_true(var(code(F, T)[,1] - code(F, F)[,1]) < tol)
  expect_equal(code(T, T)[,1], code(F, T)[,1])
  
  ci <- 0.9
  expect_true(var(code(F, T, ci)[,3] - code(F, F, ci)[,3]) < tol)
  expect_true(var(code(T, T, ci)[,3] - code(T, F, ci)[,3]) < tol)
  expect_equal(code(F, T)[,1] - code(F, F)[,1], code(F, T, ci)[,3] - code(F, F, ci)[,3])
  expect_equal(code(T, T)[,1] - code(T, F)[,1], code(T, T, ci)[,3] - code(T, F, ci)[,3])
})
