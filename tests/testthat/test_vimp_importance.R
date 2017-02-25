library(brightbox)

data(BostonHousing, package = "mlbench")
context("Marginal VIMP")


boston_dt <- data.table(BostonHousing)
x <- boston_dt[ ,-"medv", with = FALSE]
y <- boston_dt$medv

resampling_indices <- caret::createFolds(y, k = 5)

hparams <- data.frame(max_depth = 3,
                       nrounds = 50,
                       colsample_bytree = .8,
                       eta = .1,
                       min_child_weight = 10,
                       gamma = .1,
                       subsample = .8)
x[,chas:= as.numeric(chas)] #xgboost expects numeric

out <- calculate_marginal_vimp(method = "xgbTree",
                               x = x,
                               y = y,
                               resampling_indices = resampling_indices,
                               tuneGrid = hparams,
                               loss_fcn = Metrics::rmse,
                               seed = 25)

out2 <- calculate_marginal_vimp(method = "xgbTree",
                      x = x,
                      y = y,
                      resampling_indices = resampling_indices,
                      tuneGrid = hparams,
                      loss_fcn = Metrics::rmse,
                      seed = 25)

test_that("rm and nox top two most important variables", {
          expect_true(all(out$variable[1:2] %in% c("rm", "nox")))})

test_that("multiple calls return identical result", {
  expect_true(identical(out, out2))})
