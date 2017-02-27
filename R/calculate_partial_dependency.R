#' Calculate partial dependency for list of models and optionally plot it
#'
#' Given a list of models, get their average prediction over a range of
#' values from a specified feature
#'
#' @param model_list named list of model objects. Each name will become a column
#'        containing predictions from that model.
#' @param predict_fcn function that accepts a model as its first argument
#'        and \code{newdata} as one of its named arguments
#' @param ensemble_colname character. Name of the column containing ensemble predictions
#' @param ensemble_fcn function that combines a vector of predictions into
#'        a single ensemble. Default is median
#' @param ensemble_models character vector of names from model_list. These models
#'        will be combined by ensemble_fcn to form the ensemble
#' @param plot TRUE/FALSE Should the partial dependencies be plotted? Defaults to TRUE
#' @inheritParams generate_grid_range_numeric_
#' @examples
#' \dontrun{
#' TODO: fill this out
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
                                         ensemble_models = names(model_list),
                                         plot = TRUE) {
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
  if (plot) {
    print(ggplot2::ggplot(data = tidyr::gather(avg_pred,
                                               "model",
                                               "prediction",
                                               -1),
                          ggplot2::aes_string(x = feature_col,
                                              y = "prediction",
                                              group = "model",
                                              color = "model")) +
            ggplot2::geom_point() +
            ggplot2::geom_line() +
            ggplot2::labs(title = paste("Partial dependency plot for",
                                        feature_col)) +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")))
  }
  return(avg_pred)
}
