# Agreement across model classes. These are the tests that matter most: the
# same data fitted two ways has to give the same subgroup estimates, whatever
# the fitting function chose to call its coefficients.

expect_same_result <- function(a, b, tolerance = 1e-6, p_tolerance = 1e-4) {
  expect_equal(fy_est(a)$estimate, fy_est(b)$estimate, tolerance = tolerance)
  expect_equal(fy_est(a)$se, fy_est(b)$se, tolerance = tolerance)
  # The p-value is compared more loosely than the estimate it came from. Two
  # functions that fit the same model by different algorithms agree on the
  # coefficients to more figures than they do out in the tail of the
  # distribution, and that difference is theirs rather than foresty's.
  expect_equal(fy_test(a)$p.value, fy_test(b)$p.value,
               tolerance = p_tolerance)
}

test_that("an rms fit is refused", {
  skip_if_not_installed("rms")
  d <- foresty_cohort

  expect_error(
    foresty_main(list(rms::lrm(asthma ~ no2 + sex, data = d)),
                 exposure = "no2"),
    "does not support rms fits"
  )
  expect_error(
    foresty_main(list(rms::ols(no2 ~ black_carbon + sex, data = d)),
                 exposure = "black_carbon"),
    "does not support rms fits"
  )
  expect_error(
    foresty_interaction(rms::lrm(asthma ~ no2 + sex, data = d),
                        exposure = "no2", interaction = "sex"),
    "does not support rms fits"
  )

  skip_if_not_installed("survival")
  expect_error(
    foresty_main(
      list(rms::cph(survival::Surv(followup_years, wheeze) ~ no2 + sex,
                    data = d)),
      exposure = "no2"
    ),
    "does not support rms fits"
  )
})

test_that("a variable transformed by rms inside a base R fit is still read", {
  skip_if_not_installed("rms")
  d <- foresty_cohort
  knots <- stats::quantile(d$maternal_age, probs = c(0.05, 0.35, 0.65, 0.95))
  # The fit is a glm, so it is not an rms fit; rcs() is a basis like ns().
  fit <- glm(asthma ~ no2 + sex + rms::rcs(maternal_age, knots),
             family = binomial, data = d)
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  expect_length(fy_est(x)$estimate, 2L)
  expect_true(all(is.finite(fy_est(x)$se)))
})

test_that("an exposure entered as poly() is contrasted between two values", {
  d <- foresty_cohort
  # An orthogonal basis, whose centring and scaling are recorded in the terms
  # object and have to be reused rather than recomputed from two rows.
  fit <- glm(asthma ~ poly(maternal_age, 2) + no2 + sex, family = binomial,
             data = d)
  est <- fy_est(foresty_main(list(fit), exposure = "maternal_age",
                             at = c(25, 35), exponentiate = FALSE))
  nd <- d[c(1, 1), ]
  nd$maternal_age <- c(25, 35)
  expect_equal(est$estimate,
               unname(diff(stats::predict(fit, newdata = nd, type = "link"))),
               tolerance = 1e-8)

  # And within each level of a modifier, from the model carrying the
  # interaction.
  x <- foresty_interaction(fit, exposure = "maternal_age", interaction = "sex",
                           at = c(25, 35))
  expect_length(fy_est(x)$estimate, 2L)
  expect_true(all(is.finite(fy_est(x)$se)))

  # A raw polynomial is the same question with a different basis.
  raw <- glm(asthma ~ poly(no2, 3, raw = TRUE) + sex, family = binomial,
             data = d)
  nd2 <- d[c(1, 1), ]
  nd2$no2 <- c(10, 20)
  expect_equal(
    fy_est(foresty_main(list(raw), exposure = "no2", at = c(10, 20),
                        exponentiate = FALSE))$estimate,
    unname(diff(stats::predict(raw, newdata = nd2, type = "link"))),
    tolerance = 1e-8
  )

  # Without two values it has no single effect to report, as any spline has.
  expect_error(foresty_main(list(fit), exposure = "maternal_age"),
               "at = c(from, to)", fixed = TRUE)
})

test_that("the effect measure is read from the model", {
  d <- foresty_cohort
  expect_equal(fy_model_info(fy_test_logistic())$measure, "OR")
  expect_equal(fy_model_info(fy_test_linear())$measure, "MD")
  expect_equal(
    fy_model_info(glm(asthma ~ no2, family = poisson(link = "log"), data = d))$measure,
    "RR"
  )
  skip_if_not_installed("survival")
  expect_equal(
    fy_model_info(survival::coxph(
      survival::Surv(followup_years, wheeze) ~ no2, data = d
    ))$measure,
    "HR"
  )
})

test_that("a supplied measure overrides the one read from the model", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2", measure = "Coefficient")
  expect_equal(fy_result(x)$measure, "Coefficient")
  expect_false(fy_result(x)$exponentiate)
  expect_lt(fy_est(x)$estimate, 0.5)
})

test_that("a ratio can be left on the scale it was fitted on", {
  fit <- fy_test_logistic()
  ratio <- fy_est(foresty_main(list(fit), exposure = "no2"))
  logged <- foresty_main(list(fit), exposure = "no2", exponentiate = FALSE)
  est <- fy_est(logged)

  # The same estimate, before it was exponentiated.
  expect_equal(est$estimate, log(ratio$estimate), tolerance = 1e-10)
  expect_equal(est$conf.low, log(ratio$conf.low), tolerance = 1e-10)
  expect_equal(est$se, ratio$se, tolerance = 1e-10)
  expect_equal(est$p.value, ratio$p.value, tolerance = 1e-10)

  # It is still an odds ratio, still referred to a normal rather than a t, and
  # the figure says which scale it is drawn on.
  expect_equal(fy_result(logged)$measure, "OR")
  expect_false(fy_result(logged)$exponentiate)
  expect_equal(fy_infos(logged)[[1L]]$error_df, Inf)
  expect_match(fy_result(logged)$measure_label, "^Log odds ratio for asthma")
  expect_match(gsub("\n", " ", fy_title(logged), fixed = TRUE),
               "^Adjusted log odds ratio for asthma")

  # The subgroup estimates of an interaction figure go the same way.
  by_sex <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                                exponentiate = FALSE)
  expect_equal(
    fy_est(by_sex)$estimate,
    log(fy_est(foresty_interaction(fit, exposure = "no2",
                                   interaction = "sex"))$estimate),
    tolerance = 1e-10
  )

  # `exponentiate = TRUE` is the default, and it means "as the measure asks".
  # A mean difference is on that scale already, so it is drawn as a difference
  # either way rather than as the exponential of one.
  linear <- foresty_main(list(fy_test_linear()), exposure = "black_carbon")
  expect_false(fy_result(linear)$exponentiate)
  expect_equal(
    fy_est(foresty_main(list(fy_test_linear()), exposure = "black_carbon",
                        exponentiate = TRUE))$estimate,
    fy_est(linear)$estimate
  )
  expect_equal(
    fy_est(foresty_main(list(fy_test_linear()), exposure = "black_carbon",
                        exponentiate = FALSE))$estimate,
    fy_est(linear)$estimate
  )

  # And the two scales cannot be drawn against one axis.
  expect_error(
    foresty_combine(foresty_main(list(fit), exposure = "no2"), by_sex),
    "some of the figures report a ratio"
  )
})

test_that("estimates survive a Cox model and count its events", {
  skip_if_not_installed("survival")
  fit <- survival::coxph(
    survival::Surv(followup_years, wheeze) ~ no2 + sex + maternal_asthma,
    data = foresty_cohort
  )
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  expect_equal(fy_result(x)$measure, "HR")
  expect_true(all(is.finite(fy_est(x)$estimate)))
  expect_equal(sum(fy_est(x)$events), sum(foresty_cohort$wheeze))
  expect_equal(sum(fy_est(x)$n), nrow(foresty_cohort))
})

test_that("a covariate entered as a spline does not disturb the exposure", {
  d <- foresty_cohort
  plain <- foresty_interaction(
    glm(asthma ~ no2 + sex + maternal_age, family = binomial, data = d),
    exposure = "no2", interaction = "sex"
  )
  splined <- foresty_interaction(
    glm(asthma ~ no2 + sex + splines::ns(maternal_age, 3),
        family = binomial, data = d),
    exposure = "no2", interaction = "sex"
  )
  # Not identical, since the models differ, but the same to within a hair.
  expect_length(fy_est(splined)$estimate, 2L)
  expect_equal(fy_est(plain)$estimate, fy_est(splined)$estimate,
               tolerance = 0.01)

  # And a polynomial, which is the other way a covariate is let off being a
  # straight line.
  polynomial <- foresty_interaction(
    glm(asthma ~ no2 + sex + poly(maternal_age, 3),
        family = binomial, data = d),
    exposure = "no2", interaction = "sex"
  )
  expect_equal(fy_est(plain)$estimate, fy_est(polynomial)$estimate,
               tolerance = 0.01)
})

test_that("a polr fit is read past the cutpoints it keeps out of coef()", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(
    asthma_severity ~ no2 + sex + maternal_smoking + maternal_age,
    data = foresty_cohort, Hess = TRUE
  )

  # The reason this class needs a test of its own: the three cutpoints of a
  # four-level outcome are in vcov() and not in coef(), so the two have to be
  # intersected by name before a contrast can be laid against them. Aligning
  # them by position instead would silently pair `no2` with a cutpoint.
  expect_equal(length(stats::coef(fit)), 4L)
  expect_equal(nrow(stats::vcov(fit)), 7L)
  aligned <- fy_coefs(fit)
  expect_equal(names(aligned$coef), names(stats::coef(fit)))
  expect_equal(dim(aligned$vcov), c(4L, 4L))

  # A proportional odds model reports odds ratios, and says so.
  expect_equal(fy_model_info(fit)$measure, "OR")
  expect_equal(fy_model_name(fit), "Ordinal (Ordered) logistic regression model")

  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  expect_equal(fy_result(x)$measure, "OR")
  expect_length(fy_est(x)$estimate, 2L)
  expect_true(all(is.finite(fy_est(x)$se)))
  expect_equal(sum(fy_est(x)$n), nrow(foresty_cohort))
  # An ordered outcome of four levels is not an event to be counted.
  expect_true(all(is.na(fy_est(x)$events)))
})

test_that("a splined exposure is contrasted the same way under polr", {
  skip_if_not_installed("MASS")
  d <- foresty_cohort
  fit <- MASS::polr(asthma_severity ~ splines::ns(maternal_age, 4) + sex,
                    data = d, Hess = TRUE)

  # The exposure enters through four coefficients, so its effect is not one
  # number and the two values being compared have to be named -- the same rule
  # a glm follows, and it holds here despite the cutpoints in vcov().
  expect_true(fy_is_splined(fy_model_info(fit), "maternal_age"))
  expect_error(foresty_main(list(fit), exposure = "maternal_age"),
               "at = c(from, to)", fixed = TRUE)

  # Named, it is the difference between two rows of the model's own design,
  # which is the difference between two linear predictors.
  est <- fy_est(foresty_main(list(fit), exposure = "maternal_age",
                             at = c(25, 35), exponentiate = FALSE))
  nd <- d[c(1, 1), ]
  nd$maternal_age <- c(25, 35)

  # Checked against the fitted probabilities rather than against the design
  # matrix, which is the thing under test. A proportional odds model has
  # logit(P(Y <= k)) = zeta_k - eta, so the difference between the cumulative
  # logits at two values of the exposure is the estimate, negated, and it is
  # the same at whichever cutpoint it is taken.
  probs <- stats::predict(fit, newdata = nd, type = "probs")
  cumulative <- t(apply(probs, 1L, cumsum))
  for (k in seq_len(ncol(cumulative) - 1L)) {
    expect_equal(est$estimate,
                 -unname(diff(stats::qlogis(cumulative[, k]))),
                 tolerance = 1e-8)
  }

  # And within each level of a modifier.
  x <- foresty_interaction(fit, exposure = "maternal_age", interaction = "sex",
                           at = c(25, 35))
  expect_length(fy_est(x)$estimate, 2L)
  expect_true(all(is.finite(fy_est(x)$se)))
})

test_that("polr subgroup estimates are the linear combination of the fit", {
  skip_if_not_installed("MASS")
  d <- foresty_cohort
  fit <- MASS::polr(
    asthma_severity ~ no2 + sex + maternal_smoking + maternal_age,
    data = d, Hess = TRUE
  )
  interacted <- MASS::polr(
    asthma_severity ~ no2 * sex + maternal_smoking + maternal_age,
    data = d, Hess = TRUE
  )
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           exponentiate = FALSE)

  # The girls' estimate is the exposure coefficient of the interacted model,
  # and the boys' is that coefficient plus the interaction, each with the
  # standard error of the same combination.
  male <- fy_hand_lincom(interacted, "no2", "no2:sexMale")
  expect_equal(fy_est(x)$estimate,
               c(unname(stats::coef(interacted)[["no2"]]), male$estimate),
               tolerance = 1e-6)
  expect_equal(fy_est(x)$se,
               c(sqrt(stats::vcov(interacted)["no2", "no2"]), male$se),
               tolerance = 1e-6)

  # And the joint test is MASS's own likelihood ratio test of the two models.
  reference <- stats::anova(fit, interacted)
  expect_equal(fy_test(x)$p.value, reference[["Pr(Chi)"]][2], tolerance = 1e-8)
  expect_equal(fy_test(x)$statistic, reference[["LR stat."]][2],
               tolerance = 1e-8)

  # The interaction was simulated into sex and not into maternal smoking, so
  # the modifier that has no interaction does not show one.
  y <- foresty_interaction(fit, exposure = "no2",
                           interaction = "maternal_smoking")
  expect_gt(fy_test(y)$p.value, 0.05)
})

test_that("a two-level multinom fit agrees with the logistic regression of it", {
  skip_if_not_installed("nnet")
  # Two outcome levels leave multinom() with one equation, which is the same
  # model glm() fits by a different algorithm. Its coefficients come back as a
  # plain named vector rather than the matrix a longer outcome produces.
  d <- droplevels(
    foresty_cohort[foresty_cohort$wheeze_phenotype != "Transient", ]
  )
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                        data = d, trace = FALSE)
  logistic <- glm(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                  family = binomial, data = d)

  expect_false(is.matrix(stats::coef(fit)))
  expect_equal(names(fy_coefs(fit)$coef), names(stats::coef(logistic)))
  # One equation, so there is no comparison between outcome levels to name.
  expect_null(fy_model_info(fit)$equations)
  expect_equal(fy_model_name(fit), "Logistic regression model")

  # The tolerance is loosened from the default because the two were fitted by
  # different algorithms.
  expect_same_result(
    foresty_interaction(fit, exposure = "no2", interaction = "sex"),
    foresty_interaction(logistic, exposure = "no2", interaction = "sex"),
    tolerance = 1e-4, p_tolerance = 1e-6
  )
  expect_equal(sum(fy_est(foresty_interaction(fit, exposure = "no2",
                                              interaction = "sex"))$n),
               nrow(d))
})

test_that("the equations of a multinom fit are found and named", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                        data = foresty_cohort, trace = FALSE)

  # Three outcome levels give a coefficient matrix of one row per non-reference
  # level, which is flattened to match the names vcov() uses.
  b <- stats::coef(fit)
  expect_equal(dim(b), c(2L, 4L))
  aligned <- fy_coefs(fit)
  expect_equal(length(aligned$coef), length(b))
  expect_true(all(c("Transient:no2", "Persistent:no2") %in% names(aligned$coef)))
  expect_equal(unname(aligned$coef[["Persistent:no2"]]),
               unname(b["Persistent", "no2"]))
  expect_equal(dim(aligned$vcov), c(length(b), length(b)))

  info <- fy_model_info(fit)
  expect_equal(info$equations$levels, c("None", "Transient", "Persistent"))
  expect_equal(info$equations$reference, "None")
  # The design is as wide as one equation, and the coefficient vector holds one
  # block of it per non-reference level.
  expect_equal(info$n_base, 4L)
  expect_equal(info$n_full, 8L)
  expect_equal(info$equations$blocks$Persistent, 5:8)
  # A term is mapped to the coefficient it produced in every equation, so a
  # joint test of it is a test across all of them.
  expect_equal(info$term_map$no2, c("Transient:no2", "Persistent:no2"))

  expect_equal(info$measure, "OR")
  expect_equal(fy_model_name(fit),
               "Multinomial (polytomous) logistic regression model")
})

test_that("polr fits with a non-logistic link are refused", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(
    asthma_severity ~ no2 + sex, data = foresty_cohort,
    method = "probit"
  )

  expect_error(
    foresty_main(list(fit), exposure = "no2"),
    "only with method = \"logistic\"", fixed = TRUE
  )
})

test_that("a multinom fit is drawn one row per comparison of outcome levels", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                        data = foresty_cohort, trace = FALSE)
  b <- stats::coef(fit)

  x <- foresty_main(list(fit), exposure = "no2", contrast = 10)
  est <- fy_est(x)
  expect_equal(est$outcome_level, c("Transient", "Persistent"))
  expect_equal(est$outcome_reference, c("None", "None"))
  expect_equal(as.character(est$label),
               c("Transient vs None", "Persistent vs None"))

  # Each row is the equation of its own outcome level and nothing else, so it
  # is the coefficient of that equation, ten of them.
  expect_equal(est$estimate, unname(exp(10 * b[, "no2"])), tolerance = 1e-10)
  expect_equal(est$se, unname(10 * sqrt(diag(stats::vcov(fit))[
    c("Transient:no2", "Persistent:no2")
  ])), tolerance = 1e-10)

  # The rows count everybody the model was fitted to, and the events of a row
  # are the people who were in the outcome level it is about.
  expect_equal(est$n, rep(nrow(foresty_cohort), 2L))
  expect_equal(est$events,
               as.integer(table(foresty_cohort$wheeze_phenotype)[
                 c("Transient", "Persistent")
               ]))
})

test_that("the outcome level everything is read against can be chosen", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                        data = foresty_cohort, trace = FALSE)
  b <- stats::coef(fit)
  v <- stats::vcov(fit)

  x <- foresty_main(list(fit), exposure = "no2", contrast = 10,
                    outcome_reference = "Transient", exponentiate = FALSE)
  est <- fy_est(x)
  expect_equal(est$outcome_level, c("None", "Persistent"))
  expect_equal(as.character(est$label),
               c("None vs Transient", "Persistent vs Transient"))

  # Naming another reference is not a refit. The odds ratio of one level
  # against another is the difference of their two equations, and the level the
  # fit was referred to is the zero both are measured from, so "None vs
  # Transient" is the Transient equation negated.
  expect_equal(est$estimate[[1L]], unname(-10 * b["Transient", "no2"]),
               tolerance = 1e-10)
  expect_equal(est$se[[1L]], 10 * sqrt(v["Transient:no2", "Transient:no2"]),
               tolerance = 1e-10)

  # And a comparison between two levels the fit was not referred to is the
  # difference of the two, with the covariance of the pair in its variance.
  expect_equal(est$estimate[[2L]],
               unname(10 * (b["Persistent", "no2"] - b["Transient", "no2"])),
               tolerance = 1e-10)
  expect_equal(
    est$se[[2L]],
    10 * sqrt(v["Persistent:no2", "Persistent:no2"] +
                v["Transient:no2", "Transient:no2"] -
                2 * v["Persistent:no2", "Transient:no2"]),
    tolerance = 1e-10
  )

  expect_error(
    foresty_main(list(fit), exposure = "no2", outcome_reference = "Wheezy"),
    "not a level of the outcome"
  )
  # A fit of one equation has no such choice to make.
  expect_error(
    foresty_main(list(fy_test_logistic()), exposure = "no2",
                 outcome_reference = "Yes"),
    "only a model of more than two outcome levels has"
  )
})

test_that("a multinom interaction is tested jointly across its equations", {
  skip_if_not_installed("nnet")
  d <- foresty_cohort
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                        data = d, trace = FALSE)
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           contrast = 10)

  # Two subgroups by two comparisons of outcome levels, the comparisons named
  # on the rows and the subgroups taking the blocks.
  est <- fy_est(x)
  expect_equal(nrow(est), 4L)
  expect_equal(est$modifier_level, rep(c("Female", "Male"), each = 2L))
  expect_equal(est$outcome_level, rep(c("Transient", "Persistent"), 2L))

  # Every equation has its own interaction coefficient, and the test asks
  # whether any of them is anything, so it has one degree of freedom apiece.
  test <- fy_test(x)
  expect_equal(test$df, 2)
  expect_equal(test$terms,
               c("Transient:no2:sexMale", "Persistent:no2:sexMale"))

  # Against the likelihood ratio test of the same two models, taken by hand.
  interacted <- nnet::multinom(
    wheeze_phenotype ~ no2 * sex + maternal_smoking, data = d, trace = FALSE
  )
  expect_equal(
    test$p.value,
    stats::pchisq(2 * (as.numeric(stats::logLik(interacted)) -
                         as.numeric(stats::logLik(fit))),
                  df = 2, lower.tail = FALSE),
    tolerance = 1e-8
  )

  # The subgroup estimate is the linear combination of the equation it belongs
  # to, and no coefficient of the other equation is in it.
  b <- stats::coef(interacted)
  expect_equal(
    log(est$estimate[est$modifier_level == "Male" &
                       est$outcome_level == "Persistent"]),
    unname(10 * (b["Persistent", "no2"] + b["Persistent", "no2:sexMale"])),
    tolerance = 1e-6
  )
})

test_that("the reference outcome level can be drawn as a row of its own", {
  skip_if_not_installed("nnet")
  d <- foresty_cohort
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                        data = d, trace = FALSE)

  plain <- fy_est(foresty_main(list(fit), exposure = "no2", contrast = 10))
  x <- foresty_main(list(fit), exposure = "no2", contrast = 10,
                    outcome_reference_row = TRUE)
  est <- fy_est(x)

  # One row more than before, at the top, and the rows that were there already
  # are untouched.
  expect_equal(nrow(est), nrow(plain) + 1L)
  expect_equal(as.character(est$label),
               c("None", "Transient vs None", "Persistent vs None"))
  expect_equal(est$estimate[-1L], plain$estimate)
  expect_equal(est$se[-1L], plain$se)

  # It is a definition rather than an estimate: the null value of the scale,
  # and nothing else.
  expect_true(est$reference[[1L]])
  expect_false(any(est$reference[-1L]))
  expect_equal(est$estimate[[1L]], 1)
  expect_true(all(is.na(est[1L, c("se", "conf.low", "conf.high",
                                  "statistic", "p.value")])))
  expect_equal(est$outcome_level[[1L]], "None")

  # On the scale the model was fitted on it is zero rather than one.
  logged <- fy_est(foresty_main(list(fit), exposure = "no2", contrast = 10,
                                outcome_reference_row = TRUE,
                                exponentiate = FALSE))
  expect_equal(logged$estimate[[1L]], 0)

  # The counts beside it are of the people who were in that level.
  expect_equal(est$n[[1L]], nrow(d))
  expect_equal(est$events[[1L]],
               as.integer(sum(d$wheeze_phenotype == "None")))

  # It follows the level the rows are read against rather than the model's own.
  moved <- fy_est(foresty_main(list(fit), exposure = "no2", contrast = 10,
                               outcome_reference = "Transient",
                               outcome_reference_row = TRUE))
  expect_equal(as.character(moved$label),
               c("Transient", "None vs Transient", "Persistent vs Transient"))
  expect_true(moved$reference[[1L]])
})

test_that("the reference outcome row is one row whatever the exposure is", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(wheeze_phenotype ~ urbanicity + sex,
                        data = foresty_cohort, trace = FALSE)
  est <- fy_est(foresty_main(list(fit), exposure = "urbanicity",
                             outcome_reference_row = TRUE))

  # Three levels of the exposure in each of the two comparisons, and a single
  # row for the level they are read against: the same definition at every level
  # of the exposure is not three rows of it.
  expect_equal(nrow(est), 7L)
  expect_equal(sum(est$outcome_level == "None"), 1L)
  expect_equal(as.character(est$outcome_label)[[1L]], "None")
  expect_true(is.na(est$level[[1L]]))
  expect_equal(est$estimate[[1L]], 1)

  # Being one row for every level of the exposure at once, it is counted over
  # all of them: a row whose label names no level of the exposure and whose
  # count was taken at one of them cannot be read.
  d <- foresty_cohort
  expect_equal(est$n[[1L]], nrow(d))
  expect_equal(est$events[[1L]],
               as.integer(sum(d$wheeze_phenotype == "None")))

  # The rows that are about a level of the exposure are still counted at it.
  urban <- est[!is.na(est$level) & as.character(est$level) == "Urban", ]
  expect_true(all(urban$n == sum(d$urbanicity == "Urban")))
})

test_that("each subgroup of an interaction figure carries the reference row", {
  skip_if_not_installed("nnet")
  d <- foresty_cohort
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                        data = d, trace = FALSE)
  est <- fy_est(foresty_interaction(fit, exposure = "no2", interaction = "sex",
                                    contrast = 10,
                                    outcome_reference_row = TRUE))

  expect_equal(nrow(est), 6L)
  expect_equal(est$modifier_level, rep(c("Female", "Male"), each = 3L))
  expect_equal(est$reference, rep(c(TRUE, FALSE, FALSE), 2L))
  expect_equal(est$estimate[est$reference], c(1, 1))
  # Counted within the subgroup, not over everybody.
  expect_equal(est$events[est$reference],
               as.integer(table(d$sex[d$wheeze_phenotype == "None"])),
               ignore_attr = TRUE)

  # And within the subgroup only: a categorical exposure leaves the row one row
  # for all of its levels, so the subgroup is counted whole.
  fit_cat <- nnet::multinom(wheeze_phenotype ~ urbanicity * sex, data = d,
                            trace = FALSE)
  cat_est <- fy_est(suppressMessages(
    foresty_interaction(fit_cat, exposure = "urbanicity", interaction = "sex",
                        outcome_reference_row = TRUE)
  ))
  first <- cat_est[!duplicated(cat_est$modifier_level), ]
  expect_true(all(first$reference))
  expect_equal(first$n, as.integer(table(d$sex)), ignore_attr = TRUE)
  expect_equal(first$events,
               as.integer(table(d$sex[d$wheeze_phenotype == "None"])),
               ignore_attr = TRUE)
})

test_that("every row of the hardest figure carries its own estimate", {
  skip_if_not_installed("nnet")
  d <- foresty_cohort
  fit <- nnet::multinom(wheeze_phenotype ~ urbanicity + sex, data = d,
                        trace = FALSE)

  # A categorical exposure, a multinomial outcome drawn against a reference
  # level of its own, and subgroups of a modifier: three things that each skip
  # a row of the contrast matrix, in one figure. The rows are built in one
  # place and the estimates in another, so what is checked is that they still
  # line up.
  est <- fy_est(foresty_interaction(fit, exposure = "urbanicity",
                                    interaction = "sex",
                                    outcome_reference_row = TRUE))

  # Per subgroup: the outcome's reference level once, then three levels of the
  # exposure in each of the two comparisons.
  expect_equal(nrow(est), 2L * (1L + 3L * 2L))
  expect_equal(est$modifier_level, rep(c("Female", "Male"), each = 7L))
  expect_equal(
    est$reference,
    rep(c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE), 2L)
  )
  expect_true(all(est$estimate[est$reference] == 1))

  # Each estimated row against the same row of the figure drawn without the
  # reference row of the outcome, which is the same set of contrasts less the
  # rows that carry no estimate.
  plain <- fy_est(foresty_interaction(fit, exposure = "urbanicity",
                                      interaction = "sex"))
  expect_equal(est$estimate[!est$reference], plain$estimate[!plain$reference])
  expect_equal(est$se[!est$reference], plain$se[!plain$reference])

  # And one of them against the coefficients by hand: Urban against Rural in
  # the Persistent equation, inside the male subgroup.
  interacted <- nnet::multinom(wheeze_phenotype ~ urbanicity * sex, data = d,
                               trace = FALSE)
  by_hand <- fy_est(foresty_interaction(interacted, exposure = "urbanicity",
                                        interaction = "sex"))
  b <- stats::coef(interacted)
  # The rows of an interaction figure are named for the comparison of the
  # outcome as well as for the level of the exposure, the blocks being spoken
  # for by the subgroups.
  wanted <- by_hand$modifier_level == "Male" &
    as.character(by_hand$level) == "Urban, Persistent vs None"
  expect_equal(
    log(by_hand$estimate[wanted]),
    unname(b["Persistent", "urbanicityUrban"] +
             b["Persistent", "urbanicityUrban:sexMale"]),
    tolerance = 1e-6
  )
})

test_that("a second interaction on the exposure is left alone by the test", {
  d <- foresty_cohort
  # The exposure is interacted with two variables. Only the terms carrying the
  # modifier are what the interaction is tested over, and only they come out of
  # the model the likelihood ratio test compares it with.
  fit <- glm(asthma ~ no2 * sex + no2 * maternal_smoking + maternal_age,
             family = binomial, data = d)
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           contrast = 10, test = "both")
  test <- fy_test(x)

  expect_equal(test$terms, "no2:sexMale")
  expect_equal(test$df, 1)

  reduced <- glm(asthma ~ no2 + sex + no2 * maternal_smoking + maternal_age,
                 family = binomial, data = d)
  expect_equal(
    fy_result(x)$interaction_tests$lrt$p.value,
    stats::anova(reduced, fit, test = "LRT")[["Pr(>Chi)"]][[2L]],
    tolerance = 1e-8
  )

  # The subgroup estimate holds the other interaction at the row the contrast
  # was taken from, which is the first complete observation.
  b <- stats::coef(fit)
  used <- all.vars(stats::formula(fit))
  row <- d[which(stats::complete.cases(d[, used]))[[1L]], ]
  est <- fy_est(x)
  expect_equal(
    log(est$estimate[est$modifier_level == "Male"]),
    unname(10 * (b[["no2"]] + b[["no2:sexMale"]] +
                   b[["no2:maternal_smokingYes"]] *
                     (row$maternal_smoking == "Yes"))),
    tolerance = 1e-6
  )
})

test_that("robust standard errors are refused for a multinom fit", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("sandwich")
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex,
                        data = foresty_cohort, trace = FALSE)
  expect_error(
    foresty_main(list(fit), exposure = "no2", vcov = "robust"),
    "no estimating function for a nnet::multinom"
  )
  # A covariance matrix supplied by hand is used as it stands, so the refusal
  # is of sandwich rather than of robust standard errors as such.
  v <- stats::vcov(fit)
  x <- foresty_main(list(fit), exposure = "no2", vcov = v * 4)
  plain <- foresty_main(list(fit), exposure = "no2")
  expect_equal(fy_est(x)$se, 2 * fy_est(plain)$se, tolerance = 1e-8)
})

test_that("the multinom interaction test spends a df per equation per level", {
  skip_if_not_installed("nnet")
  d <- foresty_cohort
  # A three-level modifier as well as a three-level outcome, so that the two
  # sources of degrees of freedom can be told apart: the test is of every
  # coefficient the interaction added, across every equation, which is
  # (levels of the outcome - 1) x (levels of the modifier - 1) of them.
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + urbanicity,
                        data = d, trace = FALSE)

  by_sex <- fy_test(foresty_interaction(fit, exposure = "no2",
                                        interaction = "sex"))
  expect_equal(by_sex$df, 2)
  expect_length(by_sex$terms, 2L)

  by_urbanicity <- fy_test(foresty_interaction(fit, exposure = "no2",
                                               interaction = "urbanicity"))
  expect_equal(by_urbanicity$df, 4)
  expect_setequal(
    by_urbanicity$terms,
    c("Transient:no2:urbanicitySuburban", "Persistent:no2:urbanicitySuburban",
      "Transient:no2:urbanicityUrban", "Persistent:no2:urbanicityUrban")
  )

  # The joint Wald test is of the same coefficients and spends the same
  # degrees of freedom, and the two agree.
  wald <- fy_test(foresty_interaction(fit, exposure = "no2",
                                      interaction = "urbanicity",
                                      test = "wald"))
  expect_equal(wald$df, 4)
  expect_equal(wald$test, "Wald chi-square")
  expect_equal(wald$p.value, by_urbanicity$p.value, tolerance = 0.05)
})

test_that("a categorical exposure blocks a multinom figure by outcome level", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(wheeze_phenotype ~ urbanicity + sex,
                        data = foresty_cohort, trace = FALSE)
  x <- foresty_main(list(fit), exposure = "urbanicity")
  est <- fy_est(x)

  # Three levels of the exposure within each of the two comparisons of outcome
  # levels: the levels stay on the rows and the comparisons take the blocks.
  expect_equal(nrow(est), 6L)
  expect_equal(as.character(est$level),
               rep(c("Rural", "Suburban", "Urban"), 2L))
  expect_equal(est$outcome_level, rep(c("Transient", "Persistent"), each = 3L))
  expect_equal(est$reference, rep(c(TRUE, FALSE, FALSE), 2L))
  expect_equal(as.character(fy_est(x)$outcome_label),
               rep(c("Transient vs None", "Persistent vs None"), each = 3L))
})

test_that("lme4 fits of both kinds are supported, and are tested by ML", {
  skip_if_not_installed("lme4")
  d <- foresty_cohort
  d$site <- factor(rep(seq_len(20), length.out = nrow(d)))

  # A linear mixed model, whose measure is a mean difference.
  linear <- lme4::lmer(no2 ~ black_carbon + sex + (1 | site), data = d)
  x <- foresty_interaction(linear, exposure = "black_carbon",
                           interaction = "sex")
  expect_equal(fy_result(x)$measure, "MD")
  expect_length(fy_est(x)$estimate, 2L)
  expect_true(all(is.finite(fy_est(x)$se)))

  # A REML likelihood belongs to the fixed effects it was computed under, so
  # the test is taken over the same models refitted by maximum likelihood,
  # which is what lme4's own anova() does.
  reference <- stats::anova(
    lme4::lmer(no2 ~ black_carbon + sex + (1 | site), data = d),
    lme4::lmer(no2 ~ black_carbon * sex + (1 | site), data = d)
  )
  expect_equal(fy_test(x)$p.value, reference[["Pr(>Chisq)"]][2],
               tolerance = 1e-6)

  # And a generalized one, whose measure is an odds ratio.
  logistic <- lme4::glmer(asthma ~ no2 + sex + (1 | site), family = binomial,
                          data = d)
  y <- foresty_interaction(logistic, exposure = "no2", interaction = "sex")
  expect_equal(fy_result(y)$measure, "OR")
  expect_equal(
    fy_test(y)$p.value,
    stats::anova(
      logistic,
      lme4::glmer(asthma ~ no2 * sex + (1 | site), family = binomial, data = d)
    )[["Pr(>Chisq)"]][2],
    tolerance = 1e-6
  )
})

test_that("a mixed model keeps the levels of a factor written in its formula", {
  skip_if_not_installed("lme4")
  d <- foresty_cohort
  d$site <- factor(rep(seq_len(20), length.out = nrow(d)))
  d$visit <- rep(1:4, length.out = nrow(d))

  # `factor(visit)` is a model frame column of that name, and an S4 fit keeps
  # no xlevels of its own to be asked for. Recovering them under the bare
  # `visit` left them out, and the design matrix of a two row contrast then
  # rebuilt the factor from those two rows, where it has one level and no
  # contrasts can be applied to it.
  fit <- lme4::glmer(asthma ~ no2 + sex + factor(visit) + (1 | site),
                     family = binomial, data = d)
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")

  expect_length(fy_est(x)$estimate, 2L)
  expect_true(all(is.finite(fy_est(x)$se)))
})

test_that("a fit made in a loop is refitted with what it recorded itself", {
  skip_if_not_installed("geepack")
  d <- foresty_cohort
  d$id <- rep(seq_len(nrow(d) / 4L), each = 4L)

  # The call written inside the loop keeps the symbol `cs` rather than the
  # string it stood for, and the frame it stood in is gone by the time foresty
  # adds the interaction, so the working correlation has to be taken from the
  # fit itself.
  fits <- lapply(c("independence", "exchangeable"), function(cs)
    geepack::geeglm(asthma ~ no2 + sex + maternal_age, id = id, data = d,
                    family = binomial, corstr = cs))

  x <- suppressMessages(
    foresty_interaction(fits[[2L]], exposure = "no2", interaction = "sex")
  )
  same <- suppressMessages(foresty_interaction(
    geepack::geeglm(asthma ~ no2 * sex + maternal_age, id = id, data = d,
                    family = binomial, corstr = "exchangeable"),
    exposure = "no2", interaction = "sex"
  ))
  other <- suppressMessages(foresty_interaction(
    geepack::geeglm(asthma ~ no2 * sex + maternal_age, id = id, data = d,
                    family = binomial, corstr = "independence"),
    exposure = "no2", interaction = "sex"
  ))

  # It is the exchangeable fit that was carried over, not simply some fit.
  expect_equal(fy_est(x)$estimate, fy_est(same)$estimate)
  expect_false(isTRUE(all.equal(fy_est(x)$se, fy_est(other)$se)))
})
