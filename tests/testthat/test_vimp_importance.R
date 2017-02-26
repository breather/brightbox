library(brightbox)

data(BostonHousing, package = "mlbench")
context("Marginal VIMP")


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
x[,chas:= as.numeric(chas)] #xgboost expects numeric

out <- calculate_marginal_vimp(method = "xgbTree",
                               x = x,
                               y = y,
                               resampling_indices = resampling_indices,
                               tuneGrid = hparams,
                               loss_metric = "RMSE",
                               seed = 25)

out2 <- calculate_marginal_vimp(method = "xgbTree",
                      x = x,
                      y = y,
                      resampling_indices = resampling_indices,
                      tuneGrid = hparams,
                      loss_metric = "RMSE",
                      seed = 25)

test_that("rm and nox top two most important variables", {
          expect_true(all(out$variable[1:2] %in% c("rm", "nox")))})

test_that("multiple calls return identical result", {
  expect_true(identical(out, out2))})

base_loss <- base_model_loss_(method = "xgbTree",
                 x = x,
                 y = y,
                 resampling_indices = resampling_indices,
                 tuneGrid = hparams,
                 loss_metric = "RMSE",
                 seed = 25)
test_that("return value is data.frame with as many rows as resampling indices", {
  expect_is(base_loss, "data.frame")
  expect_equal(length(resampling_indices), nrow(base_loss))
  })

out_v <- marginal_vimp_(var = "rm",
               method = "xgbTree",
               x = x,
               y = y,
               resampling_indices = resampling_indices,
               base_resample_dt = base_loss,
               tuneGrid = hparams,
               loss_metric = "RMSE",
               seed = 25)

test_that("return value is numeric", {
 expect_is(out_v, "numeric")
})

marginal_vimp_partial_ <- pryr::partial(marginal_vimp_,
                                        method = "xgbTree",
                                        x = x,
                                        y = y,
                                        resampling_indices = resampling_indices,
                                        base_resample_dt = base_loss$resample,
                                        tuneGrid = hparams,
                                        loss_metric = "RMSE",
                                        seed = 25)

test_that("return value is type closure", {
  expect_type(marginal_vimp_partial_, "closure")
})

