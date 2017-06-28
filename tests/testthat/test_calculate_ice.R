library(brightbox)
context("Individual Conditional Expectation")

test_that("calculate_ice works with numeric columns", {
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
  actual <- calculate_ice(feature_dt = dt,
                          feature_col = "a",
                          model_list = models,
                          num_grid = 3,
                          predict_fcn = fake_predict_fcn,
                          ensemble_fcn = min)
  expected <- data.table(a = rep(c(1, 3.5, 6),
                                 each = 6,
                                 times = 3),
                         b = rep(-1:-6, 9),
                         id = rep(1:6, 9),
                         feature = rep("a", 54),
                         feature_val = rep(c(1, 3.5, 6),
                                           each = 6,
                                           times = 3),
                         model = rep(c("m1", "m2", "ensemble"),
                                     each = 18),
                         prediction = c(0:-5, 2.5:-2.5, 5:0,
                                        rep(0, 18),
                                        0:-5, rep(0, 3), -0.5:-2.5, rep(0, 6)))
  expect_equal(actual, expected)
})

test_that("calculate_ice works with non-numeric columns", {
  dt <- data.table(a = 1:3, b = -1:-3, c = LETTERS[1:3])
  fake_predict_fcn <- function(model, newdata) {
    if (model == "model1") {
      return(rep(0, nrow(newdata)))
    } else if (model == "model2") {
      return(rep(1, nrow(newdata)))
    }
  }
  models <- list(m1 = "model1",
                 m2 = "model2")
  actual <- calculate_ice(feature_dt = dt,
                          feature_col = "c",
                          model_list = models,
                          num_grid = 3,
                          predict_fcn = fake_predict_fcn,
                          ensemble_fcn = min)
  expected <- data.table(a = rep(1:3, 9),
                         b = rep(-1:-3, 9),
                         c = rep(LETTERS[1:3],
                                 each = 3,
                                 times = 3),
                         id = rep(1:3, 9),
                         feature = rep("c", 27),
                         feature_val = rep(LETTERS[1:3],
                                           each = 3,
                                           times = 3),
                         model = rep(c("m1", "m2", "ensemble"),
                                     each = 9),
                         prediction = c(rep(0, 9), rep(1, 9), rep(0, 9)))
  expect_equal(actual, expected)
})
