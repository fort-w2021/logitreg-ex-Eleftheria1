test_that("logitreg basics", {
  # default method
  expect_equal(
    class(logitreg(
      stats::model.matrix(~., data = mtcars[, -8]),
      mtcars$vs
    )),
    "logitreg"
  )
  expect_equal(
    logitreg(
      stats::model.matrix(~., data = mtcars[, -8]),
      mtcars$vs
    )$coefficients,
    fit_logitreg(
      stats::model.matrix(~., data = mtcars[, -8]),
      mtcars$vs
    )$coefficients
  )
  expect_equal(
    logitreg(
      stats::model.matrix(~., data = mtcars[, -8]),
      mtcars$vs
    )$fitted,
    fit_logitreg(
      stats::model.matrix(~., data = mtcars[, -8]),
      mtcars$vs
    )$fitted
  )
  # formula method
  expect_equal(
    logitreg(vs ~ mpg, data = mtcars)$coefficients,
    fit_logitreg(
      stats::model.matrix(~mpg, data = mtcars),
      mtcars$vs
    )$coefficients
  )
  expect_equal(
    logitreg(vs ~ mpg, data = mtcars)$fitted,
    fit_logitreg(
      stats::model.matrix(~mpg, data = mtcars),
      mtcars$vs
    )$fitted
  )
  expect_equal(
    class(logitreg(vs ~ mpg, data = mtcars)),
    "logitreg"
  )
})

test_that("errors of logitreg", {
  expect_error(
    logitreg(vs ~ mpg, as.matrix(mtcars))
  )
  expect_error(
    logitreg(vroom ~ mpg, mtcars)
  )
})

test_model_logitreg <- logitreg(vs ~ mpg, data = mtcars)
test_matrix <- matrix(c(1, 1, 1, 1),
  nrow = 2,
  byrow = FALSE
)

test_that("predict.logitreg", {
  expect_equal(
    predict(test_model_logitreg),
    test_model_logitreg$fitted
  )
  expect_equal(
    predict(test_model_logitreg, test_matrix),
    as.vector(
      1 / (1 + exp(-(test_matrix %*% test_model_logitreg$coefficients)))
    )
  )
  expect_error(
    predict(test_model_logitreg, "newdata")
  )
  expect_error(
    predict(test_model_logitreg, matrix(c(2, 1, 1, 1),
      nrow = 2,
      byrow = FALSE
    ))
  )
  expect_error(
    predict(test_model_logitreg, mtcars)
  )
})

test_that("fitted.logitreg", {
  expect_equal(
    fitted(test_model_logitreg),
    test_model_logitreg$fitted
  )
})
