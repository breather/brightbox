#' Internal function for use by calculate_partial_dependency
#'
#' Given a numeric column in a data.table or a custom range, generate
#' uniformly distributed points along its range.
#'
#' @param feature_col character. name of a column in \code{feature_dt}
#' @param custom_range 2-element numeric vector to be used instead of range of
#'        \code{feature_col}
#' @inheritParams run_partial_dependency
#' @examples
#' \dontrun{
#' dt <- data.table(a = 1:3, b = 4:6)
#' generate_grid_range_numeric_(dt, "a", 3)
#' }
generate_grid_range_numeric_ <- function(feature_dt,
                                         feature_col,
                                         num_grid = 100,
                                         custom_range = NULL) {
  feature_dt <- data.table::copy(feature_dt)
  if (is.null(custom_range)){
    grid_range <- range(feature_dt[[feature_col]])
    start <- grid_range[1]
    end <- grid_range[2]
  } else {
    start <- custom_range[1]
    end <- custom_range[2]
  }
  grid <- rep(seq(start, end, length = num_grid),
              each = nrow(feature_dt))
  out <- append_grid_(feature_dt, feature_col, grid)
  return(out)
}

#' Internal function for use by calculate_partial_dependency
#'
#' Given a categorical column in a data.table or a custom range, generate
#' uniformly distributed points along its range.
#'
#' @param custom_range character vector that is a subset of levels in
#'        \code{feature_col}
#' @inheritParams generate_grid_range_numeric_
#' @examples
#' \dontrun{
#' dt <- data.table(a = 1:3, b = 4:6, c = LETTERS[1:3])
#' generate_grid_range_numeric_(dt, "c")
#' }
generate_grid_range_categorical_ <- function(feature_dt,
                                             feature_col,
                                             custom_range = NULL) {
  feature_dt <- data.table::copy(feature_dt)
  if (is.null(custom_range)){
    categories <- unique(feature_dt[[feature_col]])
  } else {
    categories <- unique(custom_range)
  }
  grid <- rep(categories,
              each = nrow(feature_dt))
  out <- append_grid_(feature_dt, feature_col, grid)
  return(out)
}

#' Internal function for use by generate_grid family of functions
#'
#' Replaces \code{feature_col} with \code{grid} values and maintains column order
#'
#' @inheritParams run_partial_dependency
#' @inheritParams generate_grid_range_numeric_
#' @examples
#' \dontrun{
#' dt <- data.table(a = 1:3, b = 4:6)
#' grid <- 10:12
#' append_grid_(dt, "a", grid)
#' }
append_grid_ <- function(feature_dt, feature_col, grid) {
  feature_dt <- data.table::copy(feature_dt)
  # Save old column order so we can reset later
  old_colorder <- colnames(data.table::copy(feature_dt))
  # Drop existing feature_col column to be replaced with grid
  data.table::set(feature_dt, j = feature_col, value = NULL)
  out <- cbind(feature_dt, grid)
  data.table::setnames(out, old = ncol(out), new = feature_col)
  data.table::setcolorder(out, old_colorder)
  return(out)
}
