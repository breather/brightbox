#' Internal function for use by calculate_marginal_vimp
#'
#' Calculate baseline accuracy using all variables in training data
#'
#' @param method character string defining method to pass to caret
#' @param x data.table containing predictor variables
#' @param y vector containing target variable
#' @param resampling_indices a list of integer vectors corresponding to the row indices
#' used for each resampling iteration
#' @param tuneGrid a data.frame containing hyperparameter values for caret.
#' Should only contain one value for each hyperparameter. Set to NULL
#' if caret method does not have any hyperparameter values.
#' @param loss_metric character. Loss metric to evaluate accuracy of model
#' @param trControl trainControl object to be passed to caret train.
#' @param ... additional arguments to pass to caret train
base_model_loss_ <- function(method,
                             x,
                             y,
                             resampling_indices,
                             trControl,
                             tuneGrid,
                             loss_metric,
                             ...) {
    m1 <- caret::train(y = y,
                       x = x,
                       method = method,
                       tuneGrid = tuneGrid,
                       metric = loss_metric,
                       trControl = trControl,
                       ...)

    return(m1$resample)

}

#' Internal function for use by calculate_marginal_vimp
#'
#' Calculate the change in error from baseline model
#' when removing a variable from training data.
#' @param var character. variable to remove from training data
#' in order to obtain importance of that variable.
#' @param base_resample_dt "resample" data.frame from caret model object returned from baseline model
#' @inheritParams base_model_loss_
marginal_vimp_ <- function(var,
                           method,
                           x,
                           y,
                           resampling_indices,
                           tuneGrid,
                           trControl,
                           loss_metric,
                           base_resample_dt,
                           ...) {

  x_drop_var <- x[, -var, with = FALSE]

  m1 <- caret::train(y = y,
                     x = x_drop_var,
                     method = method,
                     tuneGrid = tuneGrid,
                     metric = loss_metric,
                     trControl = trControl,
                     ...)

  return(mean(m1$resample[[loss_metric]] - base_resample_dt[[loss_metric]]))

  }

#' Calculate the marginal importance of variables in a predictor matrix
#'
#' A caret model is trained on training data according to resampling indices
#' specified by the user, and error is calculated on out of sample data.
#' Variable importance is determined by calculating the change in out of sample model performance
#' when a variable is removed relative to baseline out of sample performance when all
#' variables are included.
#' @param vars character vector specifying variables for which to determine marginal importance
#' Defaults to all predictor variables in \code{x}.
#' @param allow_parallel boolean for parallel execution. If set to TRUE, user must specify parallel backend
#' in their R session to take advantage of multiple cores. Defaults to FALSE.
#' @inheritParams base_model_loss_
#' @export
calculate_marginal_vimp <- function(x,
                                    y,
                                    method,
                                    loss_metric,
                                    resampling_indices,
                                    tuneGrid,
                                    trControl,
                                    vars = names(x),
                                    allow_parallel = FALSE,
                                    ...) {
  #get base model loss on each hold out fold
  base_loss <- base_model_loss_(method = method,
                                x = x,
                                y = y,
                                resampling_indices = resampling_indices,
                                tuneGrid = tuneGrid,
                                trControl = trControl,
                                loss_metric = loss_metric,
                                ...)
  #generate function with variable to leave out as sole argument
  #fill in arguments for model training
  marginal_vimp_partial_ <- pryr::partial(marginal_vimp_,
                                          method = method,
                                          x = x,
                                          y = y,
                                          resampling_indices = resampling_indices,
                                          tuneGrid = tuneGrid,
                                          trControl = trControl,
                                          loss_metric = loss_metric,
                                          base_resample_dt = base_loss,
                                          ...)

  if(allow_parallel){
    var_imp <- unlist(mclapply(vars, marginal_vimp_partial_))
    names(var_imp) <- vars
  } else {
    var_imp <- sapply(vars, marginal_vimp_partial_)
  }

  #return data.table with descending variable importance
  var_ordered <- var_imp[order(-var_imp)]
  var_key <- names(var_imp[order(-var_imp)])
  return(data.table::data.table(variable = var_key,
                                delta_over_baseline = var_ordered)
  )
}
