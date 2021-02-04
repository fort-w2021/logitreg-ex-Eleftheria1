# load simulated data for testing:
load(paste0(system.file(package = "logitreg"), "/testdata/sim_data.rda"))
load(paste0(system.file(package = "logitreg"), "/testdata/logitreg-data-trouble.Rdata"))
# fit logistic model (fit_logitreg & glm)
logit_reg_model <- fit_logitreg(sim_data$design, sim_data$response)
glm_model <- glm(sim_data$response ~ sim_data$design[, 2], family = binomial)

test_that("works for simple data (sim_data)", {
  expect_true(all(logit_reg_model$fitted >= 0) & all(logit_reg_model$fitted <= 1))
  expect_equal(
    signif(logit_reg_model$coefficients, digits = 2),
    signif(unname(glm_model$coefficients), digits = 2)
  )
  expect_error(fit_logitreg(
    rowbind(sim_data$design, rep(1, dim(sim_data$design)[2])),
    c(sim_data$response, 2)
  ))
  expect_error(fit_logitreg(
    cbind(rep(2, length(sim_data$response)), sim_data$design),
    sim_data$response
  ))
  expect_error(fit_logitreg(
    cbind(sim_data$design, rep(NA, length(sim_data$response))),
    sim_data$response
  ))
})

test_that("can deal with NA's", {
  expect_message(
    fit_logitreg(trouble1$x, trouble1$y)
  )
  expect_equal(
    signif(fit_logitreg(trouble1$x, trouble1$y)$coefficients, 2),
    signif(unname(
      glm(trouble1$y ~ trouble1$x[, -1], family = binomial)$coefficients
    ), 2),
    tolerance = 0.005
  )
})

test_that("note convergence issues", {
  expect_warning(
    fit_logitreg(trouble2$x, trouble2$y)
  )
})
