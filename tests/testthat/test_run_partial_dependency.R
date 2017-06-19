library(brightbox)
context("Partial Dependency Overall Functionality")

data(BostonHousing, package = "mlbench")

boston_dt <- data.table(BostonHousing)
x <- boston_dt[ ,-"medv", with = FALSE]
y <- boston_dt$medv

resampling_indices <- caret::createFolds(y, k = 5, returnTrain = TRUE)

hparams <- data.frame(max_depth = 3,
                      nrounds = 50,
                      colsample_bytree = .8,
                      eta = .1,
                      min_child_weight = 10,
                      gamma = .1,
                      subsample = .8)
x[, chas:= as.numeric(chas)]  # xgboost expects numeric

xgb <- caret::train(method = "xgbTree",
                    x = x,
                    y = y,
                    resampling_indices = resampling_indices,
                    tuneGrid = hparams,
                    metric = "RMSE",
                    trControl = caret::trainControl(method = "cv",
                                                    index = resampling_indices,
                                                    number = length(resampling_indices)))

test_that("run_partial_dependency identifies most/least important variables", {
  pd_dt <- run_partial_dependency(x, model_list = list(xgb = xgb), plot = FALSE)
  pd_vimp_dt <- unique(pd_dt[, list(feature, vimp)])
  expect_equal("rm", head(pd_vimp_dt[, feature], 1))
  expect_true("zn" %in% tail(pd_vimp_dt[, feature], 7))
})

test_that("run_partial_dependency on individual models", {
  pd_dt <- run_partial_dependency(x,
                                  vimp_colname = "xgb",
                                  model_list = list(xgb = xgb),
                                  plot = FALSE)
  pd_vimp_dt <- unique(pd_dt[, list(feature, vimp)])
  expect_equal("rm", head(pd_vimp_dt[, feature], 1))
  expect_true("zn" %in% tail(pd_vimp_dt[, feature], 7))
})
