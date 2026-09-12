test_that("robust standard errors differ from the model's own, estimates do not", {
  skip_if_not_installed("sandwich")
  fit <- fy_test_logistic()
  plain <- fy_est(foresty_main(list(fit), exposure = "no2"))
  robust <- fy_est(foresty_main(list(fit), exposure = "no2", vcov = "robust"))

  expect_equal(plain$estimate, robust$estimate, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(plain$se, robust$se)))
  expect_true(broom::glance(foresty_main(list(fit), exposure = "no2",
                                  vcov = "robust"))$robust)
})

test_that("the robust standard error is the one sandwich computes", {
  skip_if_not_installed("sandwich")
  fit <- fy_test_logistic()
  est <- fy_est(foresty_main(list(fit), exposure = "no2", vcov = "HC0"))
  expected <- sqrt(sandwich::vcovHC(fit, type = "HC0")["no2", "no2"])
  expect_equal(est$se, unname(expected), tolerance = 1e-10)
})

test_that("\"robust\" is HC1", {
  skip_if_not_installed("sandwich")
  fit <- fy_test_logistic()
  a <- fy_est(foresty_main(list(fit), exposure = "no2", vcov = "robust"))
  b <- fy_est(foresty_main(list(fit), exposure = "no2", vcov = "HC1"))
  expect_equal(a$se, b$se)
})

test_that("clusters give the standard error sandwich computes for them", {
  skip_if_not_installed("sandwich")
  fit <- fy_test_logistic()
  groups <- rep(seq_len(400), length.out = nrow(foresty_cohort))
  est <- fy_est(foresty_main(list(fit), exposure = "no2", cluster = groups))
  expected <- sqrt(sandwich::vcovCL(fit, cluster = groups,
                                    type = "HC0")["no2", "no2"])
  expect_equal(est$se, unname(expected), tolerance = 1e-10)
})

test_that("a cluster can be named as a column of the data", {
  skip_if_not_installed("sandwich")
  d <- foresty_cohort
  d$practice <- rep(seq_len(400), length.out = nrow(d))
  fit <- glm(asthma ~ no2 + sex, family = binomial, data = d)
  by_name <- fy_est(foresty_main(list(fit), exposure = "no2",
                                 cluster = "practice"))
  by_value <- fy_est(foresty_main(list(fit), exposure = "no2",
                                  cluster = d$practice))
  expect_equal(by_name$se, by_value$se)
})

test_that("a named cluster column is matched to the rows the model kept", {
  skip_if_not_installed("sandwich")
  d <- foresty_cohort
  d$practice <- rep(seq_len(400), length.out = nrow(d))

  # Rows dropped for a missing value are recorded on the fit, and sandwich
  # takes them out of the cluster itself, so the column is used as it stands
  # and comes to what the formula form comes to.
  missing <- d
  missing$no2[seq_len(500)] <- NA
  fit <- glm(asthma ~ no2 + sex, family = binomial, data = missing)
  expect_equal(
    fy_est(foresty_main(list(fit), exposure = "no2",
                        cluster = "practice"))$se,
    fy_est(foresty_main(list(fit), exposure = "no2",
                        cluster = ~practice))$se
  )

  # Rows left out by `subset` are recorded nowhere, so the column cannot be
  # lined up with them and saying so is the whole of what can be done.
  part <- glm(asthma ~ no2 + sex, family = binomial, data = d,
              subset = maternal_age > 30)
  expect_error(
    foresty_main(list(part), exposure = "no2", cluster = "practice"),
    "cannot be matched to the rows"
  )
  # The formula reaches the rows the model kept, and is what the message says
  # to use.
  expect_silent(foresty_main(list(part), exposure = "no2",
                             cluster = ~practice))
})

test_that("a matrix or a function can be supplied instead", {
  fit <- fy_test_logistic()
  v <- stats::vcov(fit) * 4
  est <- fy_est(foresty_main(list(fit), exposure = "no2", vcov = v))
  plain <- fy_est(foresty_main(list(fit), exposure = "no2"))
  expect_equal(est$se, plain$se * 2, tolerance = 1e-10)

  by_function <- fy_est(foresty_main(list(fit), exposure = "no2",
                                     vcov = function(f) stats::vcov(f) * 4))
  expect_equal(by_function$se, est$se)
})

test_that("robust errors carry through an interaction", {
  skip_if_not_installed("sandwich")
  fit <- fy_test_logistic()
  plain <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  robust <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                                vcov = "robust")

  expect_equal(fy_est(plain)$estimate, fy_est(robust)$estimate,
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(fy_test(plain)$p.value,
                                fy_test(robust)$p.value)))
})

test_that("the classes sandwich cannot handle are told what to do instead", {
  skip_if_not_installed("survival")
  fit <- survival::coxph(
    survival::Surv(followup_years, wheeze) ~ no2 + sex, data = foresty_cohort
  )
  expect_error(
    foresty_main(list(fit), exposure = "no2", vcov = "robust"),
    "robust = TRUE"
  )

  skip_if_not_installed("rms")
  # An rms fit is refused before its standard errors are ever reached, so the
  # refusal is the one about rms rather than one about `vcov`.
  expect_error(
    foresty_main(list(rms::lrm(asthma ~ no2 + sex, data = foresty_cohort)),
                 exposure = "no2", vcov = "robust"),
    "does not support rms fits"
  )
})

test_that("an unknown vcov name is refused", {
  expect_error(
    foresty_main(list(fy_test_logistic()), exposure = "no2", vcov = "sandwichy"),
    "must be \"robust\""
  )
})

test_that("a fit that is already robust is reported as robust", {
  skip_if_not_installed("survival")
  d <- foresty_cohort
  plain <- survival::coxph(
    survival::Surv(followup_years, wheeze) ~ no2 + sex, data = d
  )
  robust <- survival::coxph(
    survival::Surv(followup_years, wheeze) ~ no2 + sex, data = d,
    robust = TRUE, cluster = birth_year
  )

  expect_false(broom::glance(foresty_main(list(plain), exposure = "no2"))$robust)
  expect_true(broom::glance(foresty_main(list(robust), exposure = "no2"))$robust)

  # The robust variance is the fit's own, so foresty reports what it computed.
  est <- fy_est(foresty_main(list(robust), exposure = "no2"))
  expect_equal(est$se, unname(sqrt(stats::vcov(robust)["no2", "no2"])),
               tolerance = 1e-10)
})
