# TODO: all dependency functions need to start with ::
# TODO: documentation
# TODO: unit tests
library(caret)
library(data.table)
library(xgboost)
library(pryr)

#function to generate baseline accuracy with all variables included in training data


#' Internal function for use by oob_importance
#'
#' Calculate baseline accuracy using all variables in training data
#'
#'
#' @param model character string defining method to pass to caret
#' @param x data.table containing predictor variables
#' @param y vector containing target variable
#' @param inds a list of integer vectors corresponding to the rows
#' used for each training iteration
#' @param tuneparams a data.frame containing hyperparameter values for caret.
#' Should only contain one value for each hyperparamter.
#' If method does not require hyperparamter settings, set to NULL
#' @param f loss function to evaluate accuracy of model
#' @param seed set random seed for train method
#' @param ... additional arguments to pass to caret train
varImpBase <- function(model, x, y, inds, tuneparams, f,
                       seed, ...){
  loss_list <- list() #intialize rmsle list
  set.seed(seed)
  for (i in 1:length(inds)){

    train_bag <- x[inds[[i]]]
    train_o_bag <- x[-inds[[i]]]

    label_bag <- y[inds[[i]]]
    label_o_bag <- y[-inds[[i]]]

    m1 <- train(y = label_bag,
                x = train_bag,
                method = model,
                tuneGrid = tuneparams,
                trControl = trainControl(method = "none", seeds = seed),
                ...
    )
    print(m1$call)
    print(seed)


    pred <- predict(m1, newdata = train_o_bag)
    loss_list[[i]] <- f(pred, label_o_bag)
  }

  return(loss_list)
}



#' Internal function for use by oob_importance
#'
#' Calculate the change in error from baseline model
#' when removing a variable from training data.
#' @param var character. variable to remove from training data
#' in order to obtain importance of that variable.
#' @param loss_list list containing basline error of each sampling iteration
#' @inheritParams varImpBase
varImpBag <- function(var, model, x, y, inds, tuneparams, f, loss_list,
                      seed, ...){

  set.seed(seed)
  #var: variable you wish to remove
  #model: caret method to use for training
  #inds: training indices to use for each bagging iteration
  #tuneparams: hyper parameters to pass for training caret model
  #rmsle_list: list containing benchmark RMSLE (all variables included) for each set of indices
  loss_delta <- 0 #initialize loss

  for (i in 1:length(inds)){

    train_bag <- x[inds[[i]], -var, with = FALSE]  #training set for iteration i
    train_o_bag <- x[-inds[[i]],-var, with= FALSE] #test set for iteration i

    label_bag <- y[inds[[i]]]
    label_o_bag <- y[-inds[[i]]]

    m1 <- train(y = label_bag,
                x = train_bag,
                method = model,
                tuneGrid = tuneparams,
                trControl = trainControl(method = "none", seeds = seed),
                ...
    )
    print(m1$call)
    print(seed)


    #out of bag prediction
    pred <- predict(m1, newdata = train_o_bag)

    #marginal change in loss for removing variable 'var'
    loss_delta <- (f(pred, label_o_bag) - loss_list[[i]]) + loss_delta
  }

  return(loss_delta/length(inds)) #average delta in loss
}

fillvarImpBag <- function(model,
                          x,
                          y,
                          inds,
                          tuneparams,
                          f,
                          loss_list,
                          seed,
                          ...
){
  return(
    function(var){ varImpBag(var,
                             model = model,
                             x = x,
                             y = y,
                             inds = inds,
                             tuneparams = tuneparams,
                             f = f,
                             loss_list = loss_list,
                             seed = seed,
                             ...
    )

    }
  )
}

#' Calculate the importance of all predictor variables in a training set
#'
#' Deterimne variable importance of predictor variables by observing
#' the change in error when removing a single variable at a time
#' from training data
#' @param vars character vector specifying variables for which to determine importance
#' Defaults to all predictor variables in \code{x}.
#' @inheritParams varImpBase
oob_importance <- function(vars = names(x), model, x, y, inds, tuneparams, f,
                           seed = sample(.Random.seed, 1), ...){

  #get base model loss on each hold out fold
  base_list <- varImpBase(model = model,
                          x = x,
                          y = y,
                          inds = inds,
                          tuneparams = tuneparams,
                          f = f,
                          seed = seed,
                          ...)

  #generate function with variable to leave out as sole argument
  #fill in arguments for model training

  varOnly <- fillvarImpBag(model = model,
                           x = x,
                           y = y,
                           inds = inds,
                           tuneparams = tuneparams,
                           f = f,
                           loss_list = base_list,
                           seed = seed,
                           ...)


  var_imp <- sapply(vars,
                    varOnly)

  #return data.table with descending variable importance
  var_num <- var_imp[order(-var_imp)]
  var_key <- names(var_imp[order(-var_imp)])

  return(data.table(variable = var_key,
                    delta_over_baseline = var_num)
  )
}
