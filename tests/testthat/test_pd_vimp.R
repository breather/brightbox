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
  vimp_normed <- calculate_pd_vimp_normed(pd, vimp_colname = "ensemble")
  expect_equal(vimp_normed, 2.5e+07)
  }
)

