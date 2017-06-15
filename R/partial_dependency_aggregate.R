#' Convenience function to calculate partial dependency over multiple features
#'
#' Loops through feature columns in a \code{data.table} and
#' calculates partial dependency for each
#'
#' @return a list of \code{data.tables} returned by \code{\link{calculate_partial_dependency}}
#'
#' @param ... additional arguments to pass to \code{\link{calculate_partial_dependency}}
#' @inheritParams run_partial_dependency
#' @examples
#' \dontrun{
#' dt <- data.table(a = 1:3, b = 2:4, c = c(8, 11, 14))
#' m <- lm(c ~ a + b - 1, dt)
#' gm <- glm(c ~ a + b - 1, data = dt)
#' loop_calculate_partial_dependency(dt, feature_cols = c("a", "b"),
#'                                   model_list = list(lm1 = m), num_grid = 6)
#' }
#' @export
loop_calculate_partial_dependency <- function(feature_dt,
                                              model_list,
                                              feature_cols = names(feature_dt),
                                              predict_fcn = predict,
                                              ensemble_fcn = median,
                                              ...) {
  pd_list <- lapply(feature_cols, calculate_partial_dependency,
                    feature_dt = feature_dt,
                    model_list = model_list,
                    predict_fcn = predict_fcn,
                    ensemble_fcn = ensemble_fcn,
                    ...)
  names(pd_list) <- feature_cols
  return(pd_list)
}

#' Convenience function to calculate variable importance over multiple features
#'
#' Variable importance is calculated as the difference between the min and max
#' of the partial dependency output for a given feature
#'
#' @return a list the length of \code{pd_list}, containing the variable importance
#' of each feature
#'
#' @param pd_list output from \code{\link{loop_calculate_partial_dependency}}
#' @inheritParams calculate_pd_vimp
#' @examples
#' \dontrun{
#' # Example output from loop_calculate_partial_dependency
#' pd <- list(a = data.table(feature = c("a", "a", "a"),
#'                           feature_val = c(1, 3.5, 6),
#'                           model1 = c(-2.5, 0, 2.5),
#'                           model2 = c(0, 0, 0),
#'                           ensemble = c(-2.5, -0.75, 0)),
#'            b = data.table(feature = c("b", "b", "b"),
#'                           feature_val = c(2, 3, 4),
#'                           model1 = c(0, 0, 0),
#'                           model2 = c(0, 0, 0),
#'                           ensemble = c(1, 2, 3)))
#' loop_calculate_pd_vimp(pd, vimp_colname = "ensemble")
#' }
#' @export
loop_calculate_pd_vimp <- function(pd_list, vimp_colname = "ensemble") {
  return(lapply(pd_list, calculate_pd_vimp, vimp_colname = vimp_colname))
}

#' Generate partial dependency plots for multiple features
#'
#' Run \code{plot_fn} over output from
#' \code{loop_calculate_partial_dependency}
#'
#' @return a list of ggplot objects
#'
#' @param plot_fcn a function that accepts the output from
#'        \code{\link{calculate_partial_dependency}} and returns a ggplot object.
#'        This function will be looped over \code{pd_list}.
#'
#' @inheritParams loop_calculate_pd_vimp
#' @examples
#' \dontrun{
#' # Example output from loop_calculate_partial_dependency
#' pd <- list(a = data.table(feature = c("a", "a", "a"),
#'                           feature_val = c(1, 3.5, 6),
#'                           model1 = c(-2.5, 0, 2.5),
#'                           model2 = c(0, 0, 0),
#'                           ensemble = c(-2.5, -0.75, 0)),
#'            b = data.table(feature = c("a", "a", "a"),
#'                           feature_val = c(2, 3, 4),
#'                           model1 = c(0, 0, 0),
#'                           model2 = c(0, 0, 0),
#'                           ensemble = c(1, 2, 3)))
#' loop_plot_fcn(pd)
#' }
#' @export
loop_plot_fcn <- function(pd_list, plot_fcn = plot_partial_dependency) {
  return(lapply(pd_list, plot_fcn))
}

#' Multiple partial dependency plots on one page
#'
#' It can be helpful to view multiple partial dependency plots at once
#' to get a sense of variable importance, relationships between variables, etc.
#'
#' If the layout is something like \code{matrix(c(1,2,3,3), nrow=2, byrow=TRUE)},
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @param ... ggplot2 objects
#' @param plotlist a list of ggplot2 objects
#' @param cols Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#' @export
facet_plot_fcn <- function(..., plotlist = NULL, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout),
                                                                 ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}

#' Convenience function for calculating partial dependency and variable importance
#' over multiple features
#'
#' Given a list of models, get their average prediction over a range of
#' values for each feature in \code{features_cols}
#'
#' @return Output is a \code{data.table} with one column for every model in
#' \code{model_list}, an ensemble column, feature name and feature value columns,
#' and the variable importance column
#'
#' @param feature_dt data.table containing features used in predictive model
#' @param model_list named list of model objects. Each name will become a column
#'        containing predictions from that model.
#' @param feature_cols character vector of column names in \code{feature_dt}
#'        on which to calculate variable importance.
#'        Defaults to all columns in \code{feature_dt}
#' @param predict_fcn function that accepts a model as its first argument
#'        and \code{newdata} as one of its named arguments
#' @param ensemble_colname character. Name of the column containing ensemble predictions
#' @param ensemble_fcn function that combines a vector of predictions into
#'        a single ensemble. Default is \code{median}
#' @param ensemble_models character vector of names from model_list. These models
#'        will be combined by ensemble_fcn to form the ensemble
#' @param num_grid number of points to distribute along range of
#'        \code{feature_col} or \code{custom_range}
#' @param custom_range should only be used if \code{feature_cols} is a 1-element vector
#'        Defines a custom range to calculate partial dependency over. This can
#'        be a 2-element numerical vector or a character vector, depending on the
#'        type of \code{feature_cols[1]}
#' @param plot_fcn a function that accepts the output from
#'        \code{\link{calculate_partial_dependency}} and returns a ggplot object.
#' @param vimp_colname name of model (taken from from \code{model_list} or
#'        \code{ensemble_colname}) for which to calculate variable importance
#' @param plot TRUE/FALSE. Should the partial dependencies be plotted? Defaults to TRUE
#' @param facet TRUE/FALSE. If \code{plot = TRUE}, should the graphs be combined
#'        into one plot? Defaults to TRUE
#' @param cols if \code{facet = TRUE}, number of columns in the facetted plot
#' @param layout an alternative to \code{cols}. See \code{\link{facet_plot_fcn}}
#'        for more details
#' @examples
#' \dontrun{
#' dt <- data.table(a = 1:3, b = 2:4, c = c(8, 11, 14))
#' m <- lm(c ~ a + b - 1, dt)
#' gm <- glm(c ~ a + b - 1, data = dt)
#' }
#' run_partial_dependency(feature_dt = dt[, list(a, b)],
#'                        model_list = list(lm1 = m, gm1 = gm))
#' @import data.table
#' @export
run_partial_dependency <- function(feature_dt,
                                   model_list,
                                   feature_cols = names(feature_dt),
                                   predict_fcn = predict,
                                   ensemble_colname = "ensemble",
                                   ensemble_fcn = median,
                                   ensemble_models = names(model_list),
                                   num_grid = 10,
                                   custom_range = NULL,
                                   plot_fcn = plot_partial_dependency,
                                   vimp_colname = "ensemble",
                                   plot = TRUE,
                                   facet = TRUE,
                                   cols = 1,
                                   layout = NULL) {
  pd_list <- loop_calculate_partial_dependency(feature_dt = feature_dt,
                                               feature_cols = feature_cols,
                                               model_list = model_list,
                                               num_grid = num_grid,
                                               custom_range = custom_range,
                                               predict_fcn = predict_fcn,
                                               ensemble_colname = ensemble_colname,
                                               ensemble_fcn = ensemble_fcn,
                                               ensemble_models = ensemble_models)
  if (plot) {
    plot_list <- loop_plot_fcn(pd_list, plot_fcn)
    if (facet) {
      facet_plot_fcn(plotlist = plot_list, cols = cols, layout = layout)
    } else {
      plot_list
    }
  }
  pd_dt <- do.call(rbind, pd_list)
  pd_dt[, vimp := calculate_pd_vimp(.SD, vimp_colname = vimp_colname),
        by = "feature"]
  return(pd_dt[order(vimp, decreasing = TRUE)])
}
