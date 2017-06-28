## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- warning = FALSE, message = FALSE-----------------------------------
# Example of partial dependency plots for a linear model
library(data.table)
dt <- data.table(a = 1:3, b = 2:4, c = c(8, 11, 14))
lm1 <- lm(c ~ a + b - 1, dt)
lm1$coefficients

## ---- fig.height = 3, fig.width = 7--------------------------------------
# Note that the slopes of the plotted lines match the coefficients
library(brightbox)
pd <- run_partial_dependency(feature_dt = dt[, c("a", "b")],
                             model_list = list(linear_model = lm1))

## ------------------------------------------------------------------------
# First, load the data
library(data.table)
library(mlbench)
data(BostonHousing, package = "mlbench")
head(BostonHousing)

## ------------------------------------------------------------------------
# Split into features and response
boston_dt <- data.table(BostonHousing)
x <- boston_dt[ , -"medv", with = FALSE]
y <- boston_dt$medv
# Prep the data (numeric columns are friendlier)
x[, chas := as.numeric(chas)]

## ---- warning = FALSE, message = FALSE-----------------------------------
library(caret)
# Train an xgboost model
xgb <- train(x = x, y = y,
             method = "xgbTree", metric = "RMSE")
# Train a single-layer neural net
nn <- train(x = x, y = y,
            method = "nnet", metric = "RMSE",
            preProc = c("center", "scale"),
            tuneGrid = expand.grid(size = c(10, 15, 20), decay = c(0, 5e-4, 0.05)),
            linout = TRUE,
            trace = FALSE)

## ---- fig.height = 5, fig.width = 8--------------------------------------
library(brightbox)
# Generate partial dependency plots for each feature
pd <- run_partial_dependency(x, model_list = list(xgboost = xgb, nnet = nn),
                             ensemble_colname = "ensemble", ensemble_fcn = median)

## ------------------------------------------------------------------------
# pd was previously saved from run_partial_dependency
head(pd)

## ------------------------------------------------------------------------
# Inspect the variable importance of each feature
unique(pd[, list(feature, vimp)])

## ---- fig.height = 5, fig.width = 8--------------------------------------
pd_top <- run_partial_dependency(x, model_list = list(xgboost = xgb, nnet = nn),
                                 feature_cols = unique(pd[["feature"]])[1:10],
                                 ensemble_colname = "ensemble", ensemble_fcn = median)

## ---- warning = FALSE, message = FALSE-----------------------------------
library(caret)
# Train 50 xgboost models
xgb_list <- list()
for (i in 1:50) {
  ind <- sample(x = 1:nrow(x), size = 404)  # randomly sample ~80% of rows
  xgb_list[[i]] <- train(x = x[ind], y = y[ind],
                         method = "xgbTree", metric = "RMSE",
                         trControl = trainControl(method = "none"),
                         tuneGrid = xgb$bestTune)
}
names(xgb_list) <- paste("xgb", 1:50, sep = "_")

## ---- fig.height = 5, fig.width = 8--------------------------------------
# return list of partial dependency data.tables, one data.table for each feature
pd_list <- loop_calculate_partial_dependency(feature_dt = x, model_list = xgb_list)  
pd <- do.call(rbind, pd_list)  # rbind data.tables into one long data.table

pd_quant <- pd[model != "ensemble", 
               list(lcl = quantile(prediction, .05),
                    ensemble = median(prediction),
                    ucl = quantile(prediction, .95)),
                    by = c("feature", "feature_val")]

ggplot(data = pd_quant,
       aes(x = feature_val,
           y = prediction)) +
           geom_line(aes(y = ensemble), size = 1.2, color = "red") +
           geom_line(aes(y = ucl), size = 0.5, linetype = "dashed") +
           geom_line(aes(y = lcl), size = 0.5, linetype = "dashed") +
           facet_wrap(~feature, scales = "free") +
           labs(x = "Value Cutpoint", y = "Partial Dependence")

## ---- warning = FALSE, message = FALSE-----------------------------------
# apply calculate_pd_vimp_normed to each element of pd_list
vimp_normed_vec <- sapply(pd_list, calculate_pd_vimp_normed)
#Print descending order of variable importance
print(vimp_normed_vec[order(-vimp_normed_vec)])

