library(brightbox)
context("Partial Dependency using caret::train default S3 method")

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

test_that("calculate_pd_vimp identifies most/least important variables", {
  # TODO: need to write calculate_pd_vimp function
  pd_list <- lapply(names(x),
                    pryr::partial(calculate_partial_dependency,
                                  feature_dt = x,
                                  model_list = list(xgb = xgb),
                                  num_grid = 10,
                                  plot = FALSE))
  pd_vimp <- lapply(pd_list,
                    function(x) {
                      var <- colnames(x)[1]
                      vimp_range <- range(x[["ensemble"]])
                      return(data.table(var = var,
                                        vimp = vimp_range[2] - vimp_range[1]))
                    })
  pd_vimp_dt <- do.call(what = rbind, args = pd_vimp)
  pd_vimp_dt <- pd_vimp_dt[order(vimp, decreasing = TRUE)]

  expect_equal("rm", head(pd_vimp_dt[, var], 1))
  expect_true("zn" %in% tail(pd_vimp_dt[, var], 7))
})
