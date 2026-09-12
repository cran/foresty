test_that("a subgroup estimate is the linear combination worked out by hand", {
  fit <- glm(asthma ~ no2 * sex + maternal_age,
             family = binomial, data = foresty_cohort)
  est <- fy_est(foresty_interaction(fit, exposure = "no2", interaction = "sex"))

  # The reference level carries no interaction term, so its effect is the
  # exposure coefficient alone.
  female <- fy_hand_lincom(fit, "no2", character(0))
  expect_equal(est$estimate[1], exp(female$estimate), tolerance = 1e-10)
  expect_equal(est$se[1], female$se, tolerance = 1e-10)

  male <- fy_hand_lincom(fit, "no2", "no2:sexMale")
  expect_equal(est$estimate[2], exp(male$estimate), tolerance = 1e-10)
  expect_equal(est$se[2], male$se, tolerance = 1e-10)
})

test_that("the interval is formed before exponentiation, not after", {
  fit <- fy_test_logistic()
  est <- fy_est(foresty_main(list(fit), exposure = "no2"))
  b <- stats::coef(fit)["no2"]
  se <- sqrt(stats::vcov(fit)["no2", "no2"])

  expect_equal(est$conf.low, unname(exp(b - stats::qnorm(0.975) * se)),
               tolerance = 1e-10)
  expect_equal(est$conf.high, unname(exp(b + stats::qnorm(0.975) * se)),
               tolerance = 1e-10)
})

test_that("the confidence level is honoured", {
  fit <- fy_test_logistic()
  wide <- fy_est(foresty_main(list(fit), exposure = "no2", ci_level = 0.99))
  narrow <- fy_est(foresty_main(list(fit), exposure = "no2", ci_level = 0.90))

  expect_lt(wide$conf.low, narrow$conf.low)
  expect_gt(wide$conf.high, narrow$conf.high)
  expect_equal(wide$estimate, narrow$estimate)
})

test_that("a gaussian model is tested with t and F rather than the normal", {
  fit <- fy_test_linear()
  info <- fy_model_info(fit)
  expect_true(is.finite(info$error_df))
  expect_equal(info$error_df, stats::df.residual(fit))

  est <- fy_est(foresty_main(list(fit), exposure = "black_carbon"))
  b <- stats::coef(fit)["black_carbon"]
  se <- sqrt(stats::vcov(fit)["black_carbon", "black_carbon"])
  crit <- stats::qt(0.975, df = stats::df.residual(fit))
  expect_equal(est$conf.low, unname(b - crit * se), tolerance = 1e-10)
})

test_that("the joint test over one term matches the model's own Wald test", {
  fit <- glm(asthma ~ no2 * sex + maternal_age,
             family = binomial, data = foresty_cohort)
  info <- fy_model_info(fit)
  test <- fy_joint_test(info, "no2:sexMale")

  z <- stats::coef(fit)["no2:sexMale"] /
    sqrt(stats::vcov(fit)["no2:sexMale", "no2:sexMale"])
  expect_equal(test$statistic, unname(z^2), tolerance = 1e-8)
  expect_equal(test$df, 1)
})

test_that("the likelihood ratio test matches anova() on the two models", {
  fit <- fy_test_logistic()
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           test = "lrt")
  full <- glm(asthma ~ no2 + sex + maternal_smoking + maternal_age + no2:sex,
              family = binomial, data = foresty_cohort)
  reference <- stats::anova(fit, full, test = "LRT")

  expect_equal(fy_test(x)$statistic, reference$Deviance[2], tolerance = 1e-8)
  expect_equal(fy_test(x)$df, 1)
  expect_equal(fy_test(x)$p.value, reference[["Pr(>Chi)"]][2], tolerance = 1e-8)
  expect_equal(fy_test(x)$test, "Likelihood ratio chi-square")

  # It is the test the figure reports, and the only one that was taken.
  expect_equal(unique(fy_est(x)$interaction_p), fy_test(x)$p.value)
  expect_null(fy_est(x)$interaction_p_lrt)
  expect_named(fy_result(x)$interaction_tests, "lrt")
})

test_that("both tests are reported side by side when both are asked for", {
  fit <- fy_test_logistic()
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           test = "both")
  tests <- fy_result(x)$interaction_tests

  expect_named(tests, c("wald", "lrt"))
  # The Wald test is the one the figure has always drawn, and the likelihood
  # ratio test comes beside it rather than in place of it.
  expect_equal(fy_test(x), tests$wald)
  expect_equal(unique(fy_est(x)$interaction_p), tests$wald$p.value)
  expect_equal(unique(fy_est(x)$interaction_p_lrt), tests$lrt$p.value)

  # Two columns beside the plot, each saying which test it holds.
  estimates <- fy_row_order(fy_result(x)$estimates, "modifier_label")
  columns <- names(fy_table_columns(estimates, "Odds ratio (95% CI)"))
  expect_true(all(c("p for\ninteraction\n(Wald)", "p for\ninteraction\n(LR)")
                  %in% columns))

  # Both travel onward, on the figure and in glance().
  expect_true("interaction.lrt.p.value" %in% names(broom::tidy(x)))
  expect_equal(broom::glance(x)$interaction.lrt.p.value, tests$lrt$p.value)
})

test_that("the likelihood ratio test refits a model that already interacts", {
  fit <- glm(asthma ~ no2 * sex + maternal_age,
             family = binomial, data = foresty_cohort)
  expect_message(
    foresty_interaction(fit, exposure = "no2", interaction = "sex",
                        test = "lrt"),
    "already contains"
  )
  x <- suppressMessages(
    foresty_interaction(fit, exposure = "no2", interaction = "sex",
                        test = "lrt")
  )
  reduced <- glm(asthma ~ no2 + sex + maternal_age,
                 family = binomial, data = foresty_cohort)
  reference <- stats::anova(reduced, fit, test = "LRT")

  expect_equal(fy_test(x)$statistic, reference$Deviance[2], tolerance = 1e-8)
  expect_equal(fy_test(x)$p.value, reference[["Pr(>Chi)"]][2], tolerance = 1e-8)
})

test_that("a likelihood ratio test is refused where there is no likelihood", {
  quasi <- glm(asthma ~ no2 + sex + maternal_age, family = quasibinomial,
               data = foresty_cohort)
  expect_error(
    foresty_interaction(quasi, exposure = "no2", interaction = "sex",
                        test = "lrt"),
    "no likelihood ratio test can be taken"
  )

  # Robust standard errors are not what the likelihood ratio test is taken
  # over, so a figure asking for both is told so.
  expect_warning(
    foresty_interaction(fy_test_logistic(), exposure = "no2",
                        interaction = "sex", test = "lrt", vcov = "robust"),
    "takes no account of the robust standard errors"
  )
})

test_that("a three-level modifier gives a test on two degrees of freedom", {
  fit <- glm(asthma ~ maternal_asthma + no2 + sex + urbanicity,
             family = binomial, data = foresty_cohort)
  x <- foresty_interaction(fit, exposure = "maternal_asthma",
                           interaction = "urbanicity")
  est <- fy_est(x)

  # Two levels of the exposure, the reference included, in each of three
  # levels of the modifier.
  expect_equal(nrow(est), 6L)
  expect_equal(fy_test(x)$df, 2)
  expect_length(fy_test(x)$terms, 2L)
  expect_equal(levels(factor(est$modifier_label, levels = unique(est$modifier_label))),
               c("Rural", "Suburban", "Urban"))
})

test_that("the likelihood ratio test is the default, and falls back to Wald", {
  fit <- fy_test_logistic()
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  expect_named(fy_result(x)$interaction_tests, "lrt")
  expect_equal(fy_test(x)$test, "Likelihood ratio chi-square")

  # A default that cannot be taken is not an error: the Wald test is reported
  # in its place and the reason is given. A test asked for by name still is.
  quasi <- glm(asthma ~ no2 + sex + maternal_age, family = quasibinomial,
               data = foresty_cohort)
  expect_message(
    fallback <- foresty_interaction(quasi, exposure = "no2",
                                    interaction = "sex"),
    "quasi-likelihood family has no likelihood"
  )
  expect_named(fy_result(fallback)$interaction_tests, "wald")

  # Robust standard errors are no part of a likelihood, so the default is the
  # test that does answer to them.
  skip_if_not_installed("sandwich")
  expect_message(
    robust <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                                  vcov = "robust"),
    "the joint Wald test is reported instead"
  )
  expect_named(fy_result(robust)$interaction_tests, "wald")
})

test_that("a GEE is never given a likelihood ratio test of the interaction", {
  skip_if_not_installed("geepack")
  d <- foresty_cohort
  d$id <- seq_len(nrow(d))
  fit <- geepack::geeglm(asthma ~ no2 + sex + maternal_age, id = id,
                         family = binomial, data = d, corstr = "independence")

  # Left to itself, foresty reports the Wald test and says why.
  expect_message(
    x <- foresty_interaction(fit, exposure = "no2", interaction = "sex"),
    "estimating equations"
  )
  expect_equal(fy_test(x)$test, "Wald chi-square")

  # Asked for by name it is refused rather than approximated by something else
  # under that heading.
  expect_error(
    foresty_interaction(fit, exposure = "no2", interaction = "sex",
                        test = "lrt"),
    "no likelihood ratio test"
  )

  # Both is the one that can be given, said once.
  expect_message(
    both <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                                test = "both")
  )
  tests <- fy_result(both)$interaction_tests
  expect_length(tests, 1L)
  expect_equal(tests[[1L]]$test, "Wald chi-square")
})
