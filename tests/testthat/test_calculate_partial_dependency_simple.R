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
                                         ensemble_fcn = min)
  expected <- data.table(feature = rep("a", 9),
                         feature_val = rep(c(1, 3.5, 6), 3),
                         model = rep(c("m1", "m2", "ensemble"),
                                     each = 3),
                         prediction = c(c(-2.5, 0, 2.5),
                                        c(0, 0, 0),
                                        c(-2.5, -0.75, 0)))
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
                                         ensemble_fcn = min)
  expected <- data.table(feature = rep("c", 18),
                         feature_val = rep(LETTERS[1:6], 3),
                         model = rep(c("m1", "m2", "ensemble"),
                                     each = 6),
                         prediction = c(rep(0, 6), rep(1, 6), rep(0, 6)))
  expect_equal(actual, expected)
})
