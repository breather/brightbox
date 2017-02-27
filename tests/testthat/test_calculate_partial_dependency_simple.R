library(brightbox)
context("Partial Dependency Simple Examples")

test_that("calculate_partial_dependency works with numeric columns", {
  dt <- data.table(a = 1:6, b = -1:-6)
  fake_predict_fcn <- function(model, newdata) {
    if (model == "model1") {
      return(apply(newdata, 1, sum))
    } else if (model == "model2") {
      return(rep(0, nrow(newdata)))
    }
  }
  models <- list(m1 = "model1",
                 m2 = "model2")
  actual <- calculate_partial_dependency(feature_dt = dt,
                                         feature_col = "a",
                                         model_list = models,
                                         num_grid = 3,
                                         predict_fcn = fake_predict_fcn,
                                         ensemble_fcn = min,
                                         plot = FALSE)
  expected <- data.table(a = c(1, 3.5, 6),
                         m1 = c(-2.5, 0, 2.5),
                         m2 = c(0, 0, 0),
                         ensemble = c(-2.5, -0.75, 0))
  expect_equal(actual, expected)
})

test_that("calculate_partial_dependency works with non-numeric columns", {
  dt <- data.table(a = 1:6, b = -1:-6, c = LETTERS[1:6])
  fake_predict_fcn <- function(model, newdata) {
    if (model == "model1") {
      return(rep(0, nrow(newdata)))
    } else if (model == "model2") {
      return(rep(1, nrow(newdata)))
    }
  }
  models <- list(m1 = "model1",
                 m2 = "model2")
  actual <- calculate_partial_dependency(feature_dt = dt,
                                         feature_col = "c",
                                         model_list = models,
                                         num_grid = 3,
                                         predict_fcn = fake_predict_fcn,
                                         ensemble_fcn = min,
                                         plot = FALSE)
  expected <- data.table(c = LETTERS[1:6],
                         m1 = rep(0, 6),
                         m2 = rep(1, 6),
                         ensemble = rep(0, 6))
  expect_equal(actual, expected)
})
