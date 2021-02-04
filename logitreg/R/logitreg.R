#' Fit logistic regression
#'
#' For fitting `fit_logitreg()` is used.
#'
#' @param design a design matrix or a formula
#' @param ... method specific arguments
#'
#' @return object of class logitreg
#' @export
#'
#' @seealso [ fit_logitreg() ]
logitreg <- function(design, ...) {
  UseMethod("logitreg")
}

#' @rdname logitreg
#' @export
logitreg.default <- function(design, response, ...) {
  model <- fit_logitreg(design, response, ...)
  structure(
    model,
    class = "logitreg"
  )
}

#' @rdname logitreg
#' @export
logitreg.formula <- function(design, data, ...) {
  checkmate::assert_data_frame(data, all.missing = FALSE)
  if ("try-error" %in% class(try(stats::model.matrix(design, data = data), TRUE))) {
    stop("Formula is not compatible with data!")
  }
  response <- stats::model.frame(design, data)[, 1]
  model_matrix <- stats::model.matrix(design, data = data)
  model <- fit_logitreg(model_matrix, response, ...)
  structure(
    model,
    class = "logitreg"
  )
}



#' Predict probabilities for logitreg models
#'
#' Get the predicted probabilities for a logistic regression model.
#'
#' @param object logitreg object
#' @param newdata data.frame or design matrix with new data
#' (default returns fitted probabilities for training data)
#'
#' @return numeric vector with probabilities of size nrow(newdata)
#' @export
predict.logitreg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(object$fitted)
  }
  if ("data.frame" %in% class(newdata)) {
    newdata_matrix <- try(stats::model.matrix(~., newdata))
    if ("try-error" %in% class(newdata_matrix)) {
      stop("dataframe cannot be coerced to model matrix")
    }
    stopifnot(ncol(newdata_matrix) == ncol(object$data$design))
  } else {
    checkmate::assert_matrix(newdata,
      mode = "numeric",
      all.missing = FALSE,
      ncols = ncol(object$data$design)
    )
    if (any(newdata[, 1] != 1)) {
      stop("First column of design matrix must be filled with 1")
    }
    newdata_matrix <- newdata
  }
  estimated_prob <- 1 / (1 + exp(-(newdata_matrix %*% object$coefficients)))
  as.vector(estimated_prob)
}



#' @export
fitted.logitreg <- function(object, ...) {
  object$fitted
}
