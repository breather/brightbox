library(brightbox)
context("Partial Dependency variable importance methods")

test_that("calculate_pd_vimp_normed returns expected value on toy data.table",{
  pd <- data.table(feature = rep("a", 9),
                   feature_val = rep(c(1, 3.5, 6), 3),
                   model = rep(c("model1",
                                 "model2",
                                 "ensemble"),
                               each = 3),
                   prediction = c(c(-2.5, 0, 2.5),
                                  c(0, 0, 0),
                                  c(-2.5, -0.75, 0)))
  # Assign value while suppressing warning
  expect_warning(vimp_normed <- calculate_pd_vimp_normed(pd,
                                                         ensemble_colname = "ensemble",
                                                         summary_fcn = min))
  expect_equal(vimp_normed, 2.5e+07)
  expect_warning(vimp_normed_default <- calculate_pd_vimp_normed(pd,
                                                                 ensemble_colname = "ensemble"))
  expect_equal(round(vimp_normed_default, 6), 1.414213)
  expect_error(expect_warning(calculate_pd_vimp_normed(pd,
                                                       ensemble_colname = "ensemble",
                                                       summary_fcn = function(x) x*2)))
  }
)

