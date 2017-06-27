#' Calculate partial dependency for list of models
#'
#' Given a list of models, get their average prediction over a range of
#' values from a specified feature. This is simply calculated by taking the average
#' of the output from \code{\link{calculate_ice}} for each model and value cutpoint.
#'
#' @return Output is a \code{data.table} with the columns \code{feature},
#' \code{feature_val}, \code{model}, and \code{prediction}
#'
#' @inheritParams run_partial_dependency
#' @inheritParams generate_grid_range_numeric_
#' @examples
#' \dontrun{
#' dt <- data.table(a = 1:3, b = 2:4, c = c(8, 11, 14))
#' m <- lm(c ~ a + b - 1, dt)
#' gm <- glm(c ~ a + b - 1, data = dt)
#' calculate_partial_dependency(dt, "a", list(lm1 = m),
#'                              num_grid = 6)
#' calculate_partial_dependency(dt, "a", list(lm1 = m, glm1 = gm),
#'                              num_grid = 6, ensemble_fcn = sum)
#' calculate_partial_dependency(dt, "a", list(lm1 = m),
#'                              num_grid = 6, custom_range = c(1,6))
#' }
#' @import data.table
#' @export
calculate_partial_dependency <- function(feature_dt,
                                         feature_col,
                                         model_list,
                                         num_grid = 10,
                                         custom_range = NULL,
                                         predict_fcn = predict,
                                         ensemble_colname = "ensemble",
                                         ensemble_fcn = median,
                                         ensemble_models = names(model_list)) {
  ice <- calculate_ice(feature_dt = feature_dt,
                       feature_col = feature_col,
                       model_list = model_list,
                       num_grid = num_grid,
                       custom_range = custom_range,
                       predict_fcn = predict_fcn,
                       ensemble_colname = ensemble_colname,
                       ensemble_fcn = ensemble_fcn,
                       ensemble_models = ensemble_models)
  # Get average of predictions for each point in num_grid
  pd <- ice[,
            list(prediction = mean(prediction)),
            by = c("feature",
                   "feature_val",
                   "model")]
  return(pd)
}

#' Calculate variable importance based on partial dependency
#'
#' Variable importance is calculated as the difference between the min and max
#' of the partial dependency output for a given feature
#'
#' @param pd output from \code{\link{calculate_partial_dependency}}
#' @param vimp_colname name of model (taken from the column names in \code{pd})
#'        for which to calculate variable importance
#' @examples
#' \dontrun{
#' # Example output from calculate_partial_dependency
#' pd <- data.table(feature = rep("a", 9),
#'                  feature_val = rep(c(1, 3.5, 6), 3),
#'                  model = rep(c("model1",
#'                                "model2",
#'                                "ensemble"),
#'                              each = 3),
#'                  prediction = c(c(-2.5, 0, 2.5),
#'                                 c(0, 0, 0),
#'                                 c(-2.5, -0.75, 0)))
#' calculate_pd_vimp(pd, vimp_colname = "ensemble")
#' }
#' @export
calculate_pd_vimp <- function(pd, vimp_colname = "ensemble") {
  vimp_range <- range(pd[model == vimp_colname, prediction])
  return(vimp_range[2] - vimp_range[1])
}

#' Calculate normed variable importance based on partial dependency,
#' taking into account model variability
#'
#' Normed variable importance is calculated as the difference between the min and max,
#' divided by the minimum standard deviation of the individual model predictions at any value cutpoint
#' Most effective when training many models on different subsamples of training data.
#'
#'
#' @param pd output from \code{\link{calculate_partial_dependency}}
#' @param ensemble_colname name of ensemble column specified in \code{\link{calculate_partial_dependency}}.
#' @param epsilon small value to add to the minimum standard deviation before dividing to prevent Inf return value.
#'          Defaults to 1e-07.
#' @examples
#' \dontrun{
#' # Example output from calculate_partial_dependency
#' pd <- data.table(feature = rep("a", 9),
#'                  feature_val = rep(c(1, 3.5, 6), 3),
#'                  model = rep(c("model1",
#'                                "model2",
#'                                "ensemble"),
#'                              each = 3),
#'                  prediction = c(c(-2.5, 0, 2.5),
#'                                 c(0, 0, 0),
#'                                 c(-2.5, -0.75, 0)))
#' calculate_pd_vimp_normed(pd, ensemble_colname = "ensemble")
#' }
#' @export
calculate_pd_vimp_normed <- function(pd, ensemble_colname = "ensemble", epsilon = 1e-07) {
  if (length(unique(pd$model)) < 10) {
    warning("small number of models may lead to misleading normed vimp scores")
  }

  vimp_range <- range(pd[model == ensemble_colname, prediction])
  pd_vimp <- vimp_range[2] - vimp_range[1]
  cutpoint_sd <- pd[model != ensemble_colname,
                    list(model_sd = sd(prediction)),
                    by = "feature_val"]
  min_sd <- min(cutpoint_sd$model_sd) + epsilon
  return(pd_vimp/min_sd)
}


#' Generate a partial dependency plot
#'
#' Take the output from \code{calculate_partial_dependency} and plot the
#' partial dependencies for every model.
#' This function returns the plot object so it can be further modified.
#'
#' @inheritParams calculate_pd_vimp
#' @examples
#' \dontrun{
#' # Example output from calculate_partial_dependency
#' pd <- data.table(feature = rep("a", 9),
#'                  feature_val = rep(c(1, 3.5, 6), 3),
#'                  model = rep(c("model1",
#'                                "model2",
#'                                "ensemble"),
#'                              each = 3),
#'                  prediction = c(c(-2.5, 0, 2.5),
#'                                 c(0, 0, 0),
#'                                 c(-2.5, -0.75, 0)))
#' plot_partial_dependency(pd)
#' }
#' @export
plot_partial_dependency <- function(pd) {
  feature_col <- pd[["feature"]][1]
  model_names <- unique(pd[["model"]])
  # Set size attributes for each model in graph
  size_models <- rep(0.5, length(model_names))
  names(size_models) <- model_names
  # Set type attributes for each model in graph
  type_models <- rep("dotdash", length(model_names))
  names(type_models) <- model_names
  # Adjust size and type attributes for ensemble model
  # NOTE: assumes ensemble model is last element
  size_models[length(model_names)] = 1.7
  type_models[length(model_names)] = "solid"

  return(ggplot2::ggplot(data = pd,
                         ggplot2::aes(x = feature_val, y = prediction,
                                      colour = model, group = model,
                                      size = model, linetype = model)) +
           ggplot2::geom_point() + ggplot2::geom_line() +
           ggplot2::scale_size_manual(values = size_models) +
           ggplot2::scale_linetype_manual(values = type_models) +
           ggplot2::labs(x = feature_col, y = "Partial Dependence"))
}
