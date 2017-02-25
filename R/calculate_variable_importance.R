#' Internal function for use by calculate_marginal_vimp
#'
#' Calculate baseline accuracy using all variables in training data
#'
#' @param method character string defining method to pass to caret
#' @param x data.table containing predictor variables
#' @param y vector containing target variable
#' @param resampling_indices a list of integer vectors corresponding to the rows
#' used for each training iteration
#' @param tuneGrid a data.frame containing hyperparameter values for caret.
#' Should only contain one value for each hyperparamter
#' @param loss_fcn loss function to evaluate accuracy of model
#' @param seed set random seed for train method
#' @param ... additional arguments to pass to caret train
base_model_loss_ <- function(method, x, y, resampling_indices, tuneGrid,
                             loss_fcn, seed, ...) {
  loss_list <- list() #intialize rmsle list
  for (i in 1:length(resampling_indices)){
    train_bag <- x[resampling_indices[[i]]]
    train_o_bag <- x[-resampling_indices[[i]]]
    label_bag <- y[resampling_indices[[i]]]
    label_o_bag <- y[-resampling_indices[[i]]]
    m1 <- caret::train(y = label_bag,
                       x = train_bag,
                       method = method,
                       tuneGrid = tuneGrid,
                       trControl = caret::trainControl(method = "none", seeds = seed),
                       ...)
    pred <- predict(m1, newdata = train_o_bag)
    loss_list[[i]] <- loss_fcn(pred, label_o_bag)
  }
  return(loss_list)
}

#' Internal function for use by calculate_marginal_vimp
#'
#' Calculate the change in error from baseline model
#' when removing a variable from training data.
#' @param var character. variable to remove from training data
#' in order to obtain importance of that variable.
#' @param base_loss_list list containing basline error of each sampling iteration
#' @inheritParams base_model_loss_
marginal_vimp_ <- function(var, method, x, y, resampling_indices, tuneGrid,
                           loss_fcn, base_loss_list, seed, ...) {
  loss_delta <- 0 #initialize loss
  for (i in 1:length(resampling_indices)){
    train_bag <- x[resampling_indices[[i]], -var, with = FALSE]  #training set for iteration i
    train_o_bag <- x[-resampling_indices[[i]],-var, with= FALSE] #test set for iteration i
    label_bag <- y[resampling_indices[[i]]]
    label_o_bag <- y[-resampling_indices[[i]]]

    m1 <- caret::train(y = label_bag,
                       x = train_bag,
                       method = method,
                       tuneGrid = tuneGrid,
                       trControl = caret::trainControl(method = "none", seeds = seed),
                       ...)
    #out of bag prediction
    pred <- predict(m1, newdata = train_o_bag)
    #marginal change in loss for removing variable 'var'
    loss_delta <- (loss_fcn(pred, label_o_bag) - base_loss_list[[i]]) + loss_delta
  }
  return(loss_delta/length(resampling_indices)) #average delta in loss
}

#' Calculate the importance of all predictor variables in a training set
#'
#' Deterimne variable importance of predictor variables by observing
#' the change in error when removing a single variable at a time
#' from training data
#' @param vars character vector specifying variables for which to determine importance
#' Defaults to all predictor variables in \code{x}.
#' @inheritParams base_model_loss_
#' @export
calculate_marginal_vimp <- function(x, y, method, loss_fcn,
                                    resampling_indices, tuneGrid,
                                    vars = names(x),
                                    seed = sample(.Random.seed, 1),
                                    ...) {
  #get base model loss on each hold out fold
  base_loss <- base_model_loss_(method = method,
                                x = x,
                                y = y,
                                resampling_indices = resampling_indices,
                                tuneGrid = tuneGrid,
                                loss_fcn = loss_fcn,
                                seed = seed,
                                ...)
  #generate function with variable to leave out as sole argument
  #fill in arguments for model training
  marginal_vimp_partial_ <- pryr::partial(marginal_vimp_,
                                          method = method,
                                          x = x,
                                          y = y,
                                          resampling_indices = resampling_indices,
                                          tuneGrid = tuneGrid,
                                          loss_fcn = loss_fcn,
                                          base_loss_list = base_loss,
                                          seed = seed,
                                          ...)
  var_imp <- sapply(vars, marginal_vimp_partial_)
  #return data.table with descending variable importance
  var_ordered <- var_imp[order(-var_imp)]
  var_key <- names(var_imp[order(-var_imp)])
  return(data.table::data.table(variable = var_key,
                                delta_over_baseline = var_ordered)
  )
}
