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

#' Calculate variable importance based on partial dependency for list of models
#'
#' Given a list of models, get their average prediction over a range of
#' values for each feature in \code{features_cols}
#'
#' @param vimp_colname name of model (taken from from \code{model_list} or
#'        \code{ensemble_colname}) for which to calculate variable importance
#' @param feature_cols character vector of column names in \code{feature_dt}
#'        on which to calculate variable importance.
#'        Defaults to all columns in \code{feature_dt}
#' @param allow_parallel boolean for parallel execution.
#'        If set to TRUE, user must specify parallel backend in their R session
#'        to take advantage of multiple cores. Defaults to FALSE.
#' @param ... additional arguments to pass to  \code{\link{calculate_partial_dependency}}
#' @inheritParams calculate_partial_dependency
#' @examples
#' \dontrun{
#' TODO: fill this out
#' }
#' @import data.table
#' @export
calculate_pd_vimp <- function(feature_dt,
                              model_list,
                              vimp_colname = "ensemble",
                              feature_cols = names(feature_dt),
                              allow_parallel = FALSE,
                              ...) {
  # Prepopulate calculate_partial_dependency with arguments
  pd_list_partial_ <- pryr::partial(calculate_partial_dependency,
                                    feature_dt = feature_dt,
                                    model_list = model_list,
                                    ...)
  # calculate partial dependency for each feature in feature_cols
  if (allow_parallel) {
    pd_list <- parallel::mclapply(feature_cols, pd_list_partial_)
  } else {
    pd_list <- lapply(feature_cols, pd_list_partial_)
  }
  # Calculate partial dependency range for each variable in feature_cols
  calculate_vimp_from_pd_list_ <- function(x) {
    var <- colnames(x)[1]
    vimp_range <- range(x[[vimp_colname]])
    return(data.table(var = var,
                      vimp = vimp_range[2] - vimp_range[1]))
  }
  pd_vimp <- lapply(pd_list, calculate_vimp_from_pd_list_)
  # Return results as data.table
  pd_vimp_dt <- do.call(what = rbind, args = pd_vimp)
  pd_vimp_dt <- pd_vimp_dt[order(vimp, decreasing = TRUE)]
  return(pd_vimp_dt)
}
