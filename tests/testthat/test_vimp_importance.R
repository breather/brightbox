library(brightbox)

library(mlbench)
library(caret)
data(BostonHousing)
library(Metrics)
context("variable importance of training data")


boston_dt <- data.table(BostonHousing)
x <- boston_dt[ ,-"medv", with = FALSE]
y <- boston_dt$medv

inds <- createFolds(y, k = 5)

hparams <- data.frame(max_depth = 3,
                       nrounds = 100,
                       colsample_bytree = .8,
                       eta = .1,
                       min_child_weight = 10,
                       gamma = .1,
                       subsample = .8)
x[,chas:= as.numeric(chas)] #xgboost expects numeric

out <- oob_importance(model = "xgbTree",
               x = x,
               y = y,
               inds = inds,
               tuneparams = hparams,
               f = rmse,
               seed = 25)

out2 <- oob_importance(model = "xgbTree",
                      x = x,
                      y = y,
                      inds = inds,
                      tuneparams = hparams,
                      f = rmse,
                      seed = 25)


test_that("rm and nox top two most important variables", {
          expect_true(all(out$variable[1:2] %in% c("rm", "nox")))})

test_that("multiple calls return identical result", {
  expect_true(identical(out, out2))})


glm_out <- oob_importance(model = "glm",
                      x = x,
                      y = y,
                      inds = inds,
                      tuneparams = NULL,
                      f = rmse,
                      seed = 25)

glm_out2 <- oob_importance(model = "glm",
                          x = x,
                          y = y,
                          inds = inds,
                          tuneparams = NULL,
                          f = rmse,
                          seed = 25)

test_that("varaible importance data.table has as many rows as predictors in x", {
  expect_equal(ncol(x), nrow(glm_out))})

test_that("multiple calls return identical result", {
  expect_true(identical(glm_out, glm_out2))})


