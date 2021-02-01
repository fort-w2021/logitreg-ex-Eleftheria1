logitreg <- function(design, ...) {
  UseMethod("logitreg")
}

logitreg.default <- function(design, response, ...) {
  model <- fit_logitreg(design, response, ...)
  structure(
    model,
    class = "logitreg"
  )
}

logitreg.formula <- function(design, data, ...) {
  checkmate::assert_formula(design)
  checkmate::assert_data_frame(data, all.missing = FALSE)
  if (class(try(model.matrix(design, data = data), TRUE)) == "try-error") {
    stop("Formula is not compatible with data!")
  }
  response <- model.frame(design, data)[,1]
  model_matrix <- model.matrix(design, data = data)
  model <- fit_logitreg(model_matrix, response, ...)
  structure(
    model,
    class = "logitreg"
  )
}

