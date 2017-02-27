library(brightbox)

data(BostonHousing, package = "mlbench")
context("Marginal VIMP")




boston_dt <- data.table(BostonHousing)
x <- boston_dt[ ,-"medv", with = FALSE]
y <- boston_dt$medv

set.seed(25) #important for creating folds!!

resampling_indices <- caret::createFolds(y, k = 5, returnTrain = TRUE)

seeds <- vector(mode = "list", length = length(resampling_indices) + 1)
for(i in 1:(length(resampling_indices)+1)) seeds[[i]] <- 25

trControl <- trainControl(method = "cv",
             seeds = seeds,
             index = resampling_indices,
             number = length(resampling_indices))

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
                               trControl = trControl,
                               loss_metric = "RMSE")

out2 <- calculate_marginal_vimp(method = "xgbTree",
                      x = x,
                      y = y,
                      resampling_indices = resampling_indices,
                      tuneGrid = hparams,
                      trControl = trControl,
                      loss_metric = "RMSE")

test_that("rm and nox top two most important variables", {
          expect_true(all(out$variable[1:2] %in% c("rm", "nox")))
})

test_that("multiple calls return identical result", {
  expect_true(identical(out, out2))
})

base_loss <- base_model_loss_(method = "xgbTree",
                 x = x,
                 y = y,
                 resampling_indices = resampling_indices,
                 tuneGrid = hparams,
                 trControl = trControl,
                 loss_metric = "RMSE")
test_that("return value is data.frame with as many rows as resampling indices", {
  expect_equal(class(base_loss), "data.frame")
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
               trControl = trControl,
               loss_metric = "RMSE",
               seed = 25)

test_that("return value is numeric", {
 expect_equal(class(out_v), "numeric")
})

#test parallel
library(doMC)
registerDoMC(detectCores())

out_par <- calculate_marginal_vimp(method = "xgbTree",
                               x = x,
                               y = y,
                               resampling_indices = resampling_indices,
                               tuneGrid = hparams,
                               trControl = trControl,
                               loss_metric = "RMSE",
                               allow_parallel = TRUE)


test_that("rm and nox top two most important variables", {
  expect_true(all(out_par$variable[1:2] %in% c("rm", "nox")))
})

test_that("parallel identical to single core", {
  expect_true(identical(out, out_par))
})


#classification
data(Sonar, package = "mlbench")
sonar_dt <- data.table(Sonar)
x <- sonar_dt[ ,-"Class", with = FALSE]
y <- sonar_dt$Class
resampling_indices <- caret::createFolds(y, k = 3, returnTrain = TRUE)

out_class <- calculate_marginal_vimp(method = "rpart", #fails if xgboost
                               x = x,
                               y = y,
                               resampling_indices = resampling_indices,
                               tuneGrid = data.frame(cp = .01),
                               loss_metric = "ROC",
                               trControl = trainControl(method = "cv",
                                                        seeds = seeds,
                                                        index = resampling_indices,
                                                        number = length(resampling_indices),
                                                        summaryFunction = twoClassSummary,
                                                        classProbs = TRUE),
                               allow_parallel = TRUE)

#reverse sort ordering for AUC (higher AUC = better)
out_class <- out_class[order(delta_over_baseline)]

test_that("top two variables are V12 and V11", {
  expect_true(all(out_class$variable[1:2] %in% c("V12", "V11")))
})

base_AUC <- base_model_loss_(method = "xgbTree",
                 x = x,
                 y = y,
                 resampling_indices = resampling_indices,
                 tuneGrid = hparams,
                 loss_metric = "ROC",
                 trControl = trainControl(method = "cv",
                                          seeds = seeds,
                                          index = resampling_indices,
                                          number = length(resampling_indices),
                                          summaryFunction = twoClassSummary,
                                          classProbs = TRUE),
                 allow_parallel = TRUE)

test_that("return value is data.frame with as many rows as resampling indices", {
  expect_equal(class(base_AUC), "data.frame")
  expect_equal(length(resampling_indices), nrow(base_AUC))
  expect_equal(paste0("Fold", nrow(base_AUC)), base_AUC$Resample[nrow(base_AUC)])
})
