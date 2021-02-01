

#' Compute neg. loglikelihood and derivative (log. regression)
#'
#' functions to cumpute the negative loglikelihood and the derivative
#' of a logistic regression
#'
#' @param coefs model coefficients (p)
#' @param design designmatrix (n,p)
#' @param response response vector (n)
#'
#' @return value of neg. loglikelihood/ derivative
neg_loglik <- function(coefs, design, response) {
  probabilities <- stats::plogis(design %*% coefs)
  - sum(response * log(probabilities) + (1 - response) * log(1 - probabilities))
}

#' @rdname neg_loglik
neg_loglik_deriv <- function(coefs, design, response) {
  probabilities <- stats::plogis(design %*% coefs)
  - t(response - probabilities) %*% design
}

#' Fitting logistic regression models
#'
#' fit_logitreg is used to fit logistic regression models. The estimation
#' method is Maximum likelihood.
#'
#' observations with NA's will be omitted
#'
#' @param design design matrix (n,p)
#' @param response response vector (n)
#' @param ... optional arguments for the `optim` function for the estimation
#'
#' @return list of:
#' - `coefficients` estimated MLE coefficients
#' - `fitted` estimated probabilities
#' - `data` list of reponse and design matrix used for the fit
#' @export
#'
#' @examples fit_logitreg(
#'   stats::model.matrix(~., data = mtcars[,-8]),
#'   mtcars$vs
#' )
fit_logitreg <- function(design, response, ...){
  # input checks for design matrix and response variable
  checkmate::assert_matrix(design, all.missing = FALSE, mode = "numeric")
  if(any(design[,1] != 1)){
    stop("First column of design matrix must be filled with 1")
  }
  if(any(colSums(!is.na(design)) == 0)) {
    stop("Complete column filled with NA's found.
         \n Fitting logistic regression model not possible.")
  }
  checkmate::assert_integerish(response, lower = 0, upper = 1, any.missing = FALSE,
                               len = nrow(design))
  # deal with NA's
  no_na_rows_indices <- rowSums(is.na(design)) == 0
  design <- design[no_na_rows_indices,]
  response <- response[no_na_rows_indices]
  num_rows_removed <- sum(!no_na_rows_indices)
  if (num_rows_removed > 0) {
    message(paste(num_rows_removed, "rows were removed due to NA's."))
  }

  optim_result <- stats::optim(par = numeric(dim(design)[2]), fn = neg_loglik,
                        design = design, response = response)
  estimated_prob <- 1 / (1 + exp(-(design %*% optim_result$par)))
  dangerous_probs <- dplyr::near(
    response - estimated_prob,
    rep(0, length(estimated_prob))
  )
  if (any(dangerous_probs == TRUE)) {
    warning("There might be convergence issues")
    warning("fitted probabilities numerically 0 or 1 occurred")
  }
  list(coefficients = optim_result$par,
       fitted = estimated_prob,
       data = list(response = response, design = design)
  )
}
