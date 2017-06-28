#' Calculate individual conditional expectations for list of models
#'
#' Given a list of models, get their predictions over a range of
#' values from a specified feature. This is done by copying an input matrix \code{X}
#' several times and replacing a specified feature with different values for each copy.
#' The list of models then make predictions on this modified data.
#'
#' @return Output is a \code{data.table} of the same structure as \code{feature_dt},
#' with several changes:
#' \enumerate{
#'   \item \code{feature_dt} is copied \code{num_grid} or \code{length(custom_range)} times
#'   \item the values in \code{feature_col} will have been modified
#'   \item new columns will have been added. One for each value in \code{model_list}
#'         as well as \code{ensemble_colname}
#'   \item an \code{id} column will have been added to track observations
#' }
#'
#' @inheritParams run_partial_dependency
#' @inheritParams generate_grid_range_numeric_
#' @examples
#' \dontrun{
#' dt <- data.table(a = 1:3, b = 2:4, c = c(8, 11, 14))
#' m <- lm(c ~ a + b - 1, dt)
#' gm <- glm(c ~ a + b - 1, data = dt)
#' calculate_ice(dt, "a", list(lm1 = m),
#'               num_grid = 6)
#' calculate_ice(dt, "a", list(lm1 = m, glm1 = gm),
#'               num_grid = 6, ensemble_fcn = sum)
#' calculate_ice(dt, "a", list(lm1 = m),
#'               num_grid = 6, custom_range = c(1,6))
#' }
#' @import data.table
#' @export
calculate_ice <- function(feature_dt,
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
  # generate an observation id
  input_pred[, id := rep(1:nrow(feature_dt),
                         ifelse(is.null(custom_range),
                                num_grid,
                                length(custom_range)))]
  data.table::set(input_pred, j = "feature", value = feature_col)
  data.table::set(input_pred, j = "feature_val", value = input_pred[[feature_col]])
  input_long <- tidyr::gather_(input_pred,
                               "model",
                               "prediction",
                               c(names(model_list),
                                 ensemble_colname))
  input_long <- data.table::data.table(input_long)
  return(input_long)
}
