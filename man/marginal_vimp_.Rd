% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_variable_importance.R
\name{marginal_vimp_}
\alias{marginal_vimp_}
\title{Internal function for use by calculate_marginal_vimp}
\usage{
marginal_vimp_(var, method, x, y, resampling_indices, tuneGrid, trControl,
  loss_metric, base_resample_dt, ...)
}
\arguments{
\item{var}{character. variable to remove from training data
in order to obtain importance of that variable.}

\item{method}{character string defining method to pass to caret}

\item{x}{data.table containing predictor variables}

\item{y}{vector containing target variable}

\item{resampling_indices}{a list of integer vectors corresponding to the row indices
used for each resampling iteration}

\item{tuneGrid}{a data.frame containing hyperparameter values for caret.
Should only contain one value for each hyperparameter. Set to NULL
if caret method does not have any hyperparameter values.}

\item{trControl}{trainControl object to be passed to caret train.}

\item{loss_metric}{character. Loss metric to evaluate accuracy of model}

\item{base_resample_dt}{"resample" data.frame from caret model object returned from baseline model}

\item{...}{additional arguments to pass to caret train}
}
\description{
Calculate the change in error from baseline model
when removing a variable from training data.
}

