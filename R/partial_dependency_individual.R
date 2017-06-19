#' Calculate partial dependency for list of models
#'
#' Given a list of models, get their average prediction over a range of
#' values from a specified feature
#'
#' @return Output is a \code{data.table} with one column for every model in \code{model_list},
#' an ensemble column, plus the feature name and feature value columns
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
  if (is.numeric(feature_dt[[feature_col]])) {
    grid_dt <- generate_grid_range_numeric_(feature_dt, feature_col,
                                            num_grid, custom_range)
  } else {
    grid_dt <- generate_grid_range_categorical_(feature_dt, feature_col, custom_range)
  }
  # Generate predictions
  pred <- lapply(model_list, predict_fcn, newdata = grid_dt)
  pred <- data.table::data.table(do.call(cbind, pred))
  names(pred) <- names(model_list)
  # Create ensemble
  data.table::set(pred,
                  j = ensemble_colname,
                  value = apply(pred[, ensemble_models, with = FALSE],
                                1,
                                ensemble_fcn))
  # cbind prediction with inputs
  input_pred <- cbind(grid_dt, pred)
  # Get average of predictions for each point in num_grid
  avg_pred <- input_pred[,
                         lapply(.SD, mean),
                         by = feature_col,
                         .SDcols = c(names(model_list),
                                     ensemble_colname)]
  data.table::setnames(avg_pred, old = feature_col, new = "feature_val")
  avg_pred <- cbind(data.table(feature = feature_col), avg_pred)
  # Long format makes the column names consistent across runs
  avg_pred_long <- data.table::data.table(tidyr::gather(avg_pred, model,
                                                        prediction,
                                                        -feature,
                                                        -feature_val))
  return(avg_pred_long)
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
