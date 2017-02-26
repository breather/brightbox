library(brightbox)
require(e1071)
data(BostonHousing, package = "mlbench")
context("Marginal VIMP")
library(doMC)
registerDoMC(4)


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

system.time(out <- calculate_marginal_vimp(method = "xgbTree",
                               x = x,
                               y = y,
                               resampling_indices = resampling_indices,
                               tuneGrid = hparams,
                               loss_metric = "RMSE",
                               seed = 25))

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
                 seed = 100)
test_that("return value is data.frame with as many rows as resampling indices", {
  expect_is(base_loss, "data.frame")
  expect_equal(length(resampling_indices), nrow(base_loss))
  expect_equal(paste0("Fold", nrow(base_loss)), base_loss$Resample[nrow(base_loss)])
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

out_v <- marginal_vimp_(var = "rm",
                        method = "xgbTree",
                        x = x,
                        y = y,
                        resampling_indices = resampling_indices,
                        base_resample_dt = base_loss,
                        tuneGrid = hparams,
                        loss_metric = "RMSE",
                        seed = 25)

marginal_vimp_partial_ <- pryr::partial(marginal_vimp_,
                                        method = "xgbTree",
                                        x = x,
                                        y = y,
                                        resampling_indices = resampling_indices,
                                        base_resample_dt = base_loss,
                                        tuneGrid = hparams,
                                        loss_metric = "RMSE",
                                        seed = 25)

test_that("return value is type closure", {
  expect_type(marginal_vimp_partial_, "closure")
})

unlist(mclapply(names(x), marginal_vimp_partial_))

#classification
data(Sonar, package = "mlbench")
sonar_dt <- data.table(Sonar)
x <- sonar_dt[ ,-"Class", with = FALSE]
y <- sonar_dt$Class
resampling_indices <- caret::createFolds(y, k = 5, returnTrain = TRUE)

out <- calculate_marginal_vimp(method = "rpart",
                               x = x,
                               y = y,
                               resampling_indices = resampling_indices,
                               tuneGrid = data.frame(cp = .01),
                               loss_metric = "Kappa",
                               seed = 25)

test_that("top two variables are V11 and V45", {
  expect_true(all(out$variable[1:2] %in% c("V11", "V45")))
  })



