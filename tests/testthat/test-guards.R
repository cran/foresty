test_that("foresty_main refuses a model that interacts the exposure", {
  fit <- glm(asthma ~ no2 * sex + maternal_age,
             family = binomial, data = foresty_cohort)
  expect_error(
    foresty_main(list(fit), exposure = "no2"),
    "foresty_interaction"
  )
})

test_that("foresty_main insists on a list of models", {
  expect_error(
    foresty_main(fy_test_logistic(), exposure = "no2"),
    "must be a list of fitted models"
  )
  expect_error(foresty_main(list(), exposure = "no2"), "empty list")
})

test_that("an exposure that is not in the model is named in the error", {
  expect_error(
    foresty_main(list(fy_test_logistic()), exposure = "nowhere"),
    "does not appear in this model"
  )
})

test_that("a continuous modifier is refused, with the remedy in the message", {
  expect_error(
    foresty_interaction(fy_test_logistic(), exposure = "no2",
                        interaction = "maternal_age"),
    "cut\\("
  )
})

test_that("a numeric modifier taking three values is refused, however coded", {
  # Codes are the tempting case: the model fitted them as a straight line, so
  # three rows would be constrained to equal steps while looking freely
  # estimated, and the test would keep its single degree of freedom.
  d <- foresty_cohort
  d$urban_code <- as.numeric(d$urbanicity)
  fit <- glm(asthma ~ no2 + urban_code + maternal_age, family = binomial,
             data = d)
  expect_error(
    foresty_interaction(fit, exposure = "no2", interaction = "urban_code"),
    "takes 3 values in this model"
  )
})

test_that("a numeric modifier taking two values is read as those subgroups", {
  # A flag coded 0 and 1 enters the model through the one coefficient a
  # two-level factor gives, so it is the same model and must give the same
  # numbers rather than an error telling the caller to refit it as a factor.
  d <- foresty_cohort
  d$male <- as.numeric(d$sex == "Male")
  numeric_fit <- glm(asthma ~ no2 + male + maternal_smoking + maternal_age,
                     family = binomial, data = d)

  flag <- foresty_interaction(numeric_fit, exposure = "no2",
                              interaction = "male")
  by_sex <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                                interaction = "sex")

  est <- fy_est(flag)
  expect_equal(nrow(est), 2L)
  expect_equal(est$estimate, fy_est(by_sex)$estimate, tolerance = 1e-8)
  expect_equal(est$conf.low, fy_est(by_sex)$conf.low, tolerance = 1e-8)
  expect_equal(fy_test(flag)$p.value, fy_test(by_sex)$p.value,
               tolerance = 1e-8)
  expect_equal(fy_test(flag)$df, fy_test(by_sex)$df)

  # The values are the levels, in numeric order, and the counts behind the rows
  # are the rows of that subgroup.
  expect_equal(as.character(est$modifier_label), c("0", "1"))
  expect_equal(est$n, c(sum(d$male == 0), sum(d$male == 1)))

  # Nothing invents "No" and "Yes" for it, since 0 and 1 are as often two
  # groups as they are an absence and a presence; `level_labels` names them.
  named <- foresty_interaction(numeric_fit, exposure = "no2",
                              interaction = c(Sex = "male"),
                              level_labels = c("0" = "Female", "1" = "Male"))
  expect_equal(as.character(fy_est(named)$modifier_label),
               c("Female", "Male"))
})

test_that("a numeric modifier taking two values other than 0 and 1 works", {
  # The rule is that the modifier has two values, not that they are 0 and 1:
  # the contrast is taken at the values themselves, so it is exact either way.
  d <- foresty_cohort
  d$dose <- ifelse(d$sex == "Male", 20, 10)
  fit <- glm(asthma ~ no2 * dose + maternal_age, family = binomial, data = d)

  # The interaction is in the formula already, so that `coef(fit)` below is the
  # model the estimates came out of; foresty says it is using it as it stands.
  est <- fy_est(suppressMessages(
    foresty_interaction(fit, exposure = "no2", interaction = "dose",
                        exponentiate = FALSE)
  ))
  expect_equal(as.character(est$modifier_label), c("10", "20"))

  # The effect at dose 20 is the sum of the exposure coefficient and twenty
  # times the interaction, which is what the model says and what a caller
  # would otherwise have to work out by hand.
  b <- stats::coef(fit)
  expect_equal(est$estimate,
               c(b[["no2"]] + 10 * b[["no2:dose"]],
                 b[["no2"]] + 20 * b[["no2:dose"]]),
               tolerance = 1e-8)
})

test_that("a modifier that is not in the model is refused", {
  expect_error(
    foresty_interaction(fy_test_logistic(), exposure = "no2",
                        interaction = "nowhere"),
    "not a variable in this model"
  )
})

test_that("a modifier taking one value is refused", {
  # A single-level factor cannot be fitted in the first place, so the guard is
  # exercised on a model frame in which the modifier has gone constant.
  info <- fy_model_info(fy_test_logistic())
  info$mf$sex <- factor("Female", levels = c("Female", "Male"))
  expect_error(fy_check_modifier(info, "sex"), "only one value")
})

test_that("the exposure and the modifier cannot be the same variable", {
  expect_error(
    foresty_interaction(fy_test_logistic(), exposure = "sex",
                        interaction = "sex"),
    "same variable"
  )
})

test_that("a splined exposure is refused unless `at` names the two values", {
  fit <- glm(asthma ~ splines::ns(no2, 3) + sex, family = binomial,
             data = foresty_cohort)
  expect_error(
    foresty_interaction(fit, exposure = "no2", interaction = "sex"),
    "`at = c\\(from, to\\)`"
  )

  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           at = c(10, 20))
  est <- fy_est(x)
  expect_equal(nrow(est), 2L)
  expect_true(all(is.finite(est$estimate)))

  # The rows are the levels of the modifier; the two values compared are the
  # same on both of them, so they are said once, where the exposure is named.
  expect_equal(unique(est$contrast_label), "10 \u2192 20")
  expect_equal(as.character(est$modifier_label), c("Female", "Male"))
  expect_match(gsub("\n", " ", fy_title(x), fixed = TRUE),
               "associated with no2 (10 \u2192 20) within each level of sex",
               fixed = TRUE)
})

test_that("a basis given its knots is a term in one variable, not two", {
  d <- foresty_cohort
  knots <- stats::quantile(d$no2, probs = c(0.05, 0.35, 0.65, 0.95))
  # The knots are named in the term, so all.vars() finds two symbols in it and
  # the exposure would look as though it had been interacted with its own
  # knots.
  fit <- glm(asthma ~ splines::ns(no2, knots = knots[2:3]) + sex,
             family = binomial, data = d)

  est <- fy_est(foresty_main(list(fit), exposure = "no2", at = c(10, 20),
                             exponentiate = FALSE))
  nd <- d[c(1, 1), ]
  nd$no2 <- c(10, 20)
  expect_equal(est$estimate,
               unname(diff(stats::predict(fit, newdata = nd, type = "link"))),
               tolerance = 1e-8)

  # The knots are not a covariate either, so the estimate is adjusted for what
  # is actually in the model.
  info <- fy_model_info(fit)
  expect_equal(sort(unique(unlist(info$term_vars, use.names = FALSE))),
               c("no2", "sex"))
})

test_that("rms::rcs is read as one variable inside a plain glm", {
  skip_if_not_installed("rms")
  d <- foresty_cohort
  knots <- stats::quantile(d$no2, probs = c(0.05, 0.35, 0.65, 0.95))
  fit <- glm(asthma ~ rms::rcs(no2, knots) + sex, family = binomial, data = d)

  est <- fy_est(foresty_main(list(fit), exposure = "no2", at = c(10, 20),
                             exponentiate = FALSE))
  nd <- d[c(1, 1), ]
  nd$no2 <- c(10, 20)
  expect_equal(est$estimate,
               unname(diff(stats::predict(fit, newdata = nd, type = "link"))),
               tolerance = 1e-8)
})

test_that("an exposure expanded into columns before the fit says what to do", {
  skip_if_not_installed("Hmisc")
  d <- foresty_cohort
  knots <- stats::quantile(d$no2, probs = c(0.05, 0.35, 0.65, 0.95))
  basis <- Hmisc::rcspline.eval(d$no2, knots = knots, inclx = TRUE)
  colnames(basis) <- paste0("spline", seq_len(ncol(basis)))
  d <- cbind(d, as.data.frame(basis))
  fit <- glm(asthma ~ spline1 + spline2 + spline3 + sex, family = binomial,
             data = d)

  # The fit records nothing that ties those three columns to no2, so the
  # error says how to fit the same model in a way that does.
  expect_error(
    foresty_main(list(fit), exposure = "no2", at = c(10, 20)),
    "Put the basis in the formula"
  )

  # The same basis kept as one matrix column is a variable of the model by
  # name, and contrasting two values of it would be contrasting two values of
  # nothing, so it is refused where it is named rather than further down.
  wide <- foresty_cohort
  wide$no2_basis <- basis
  matrix_fit <- glm(asthma ~ no2_basis + sex, family = binomial, data = wide)
  expect_error(
    foresty_main(list(matrix_fit), exposure = "no2_basis", at = c(10, 20)),
    "matrix of 3 columns"
  )
})

test_that("a model already carrying the interaction is reused, not refitted", {
  fit <- glm(asthma ~ no2 * sex + maternal_age,
             family = binomial, data = foresty_cohort)
  expect_message(
    x <- foresty_interaction(fit, exposure = "no2", interaction = "sex"),
    "already contains"
  )
  expect_equal(coef(x), stats::coef(fit))
})

test_that("models reporting different measures cannot share one axis", {
  expect_error(
    foresty_main(
      list(fy_test_logistic(), fy_test_linear()),
      exposure = c("no2", "black_carbon")
    ),
    "different effect measures"
  )
})

test_that("one exposure must be given per model", {
  expect_error(
    foresty_main(list(fy_test_logistic(), fy_test_logistic()),
                 exposure = c("no2", "sex", "maternal_age")),
    "one exposure for each"
  )
})

test_that("a column that no model can supply is refused", {
  expect_error(
    foresty_main(list(fy_test_logistic()), exposure = "no2", table = TRUE,
                 columns = "person_time"),
    "none of the columns"
  )
})

test_that("`cluster` is checked before sandwich sees it", {
  # TRUE is the commonest way of getting this wrong, and sandwich's own
  # complaint about it says nothing useful.
  expect_error(
    foresty_main(list(fy_test_logistic()), exposure = "no2", cluster = TRUE),
    "names the variable that identifies the clusters"
  )
  expect_error(
    foresty_main(list(fy_test_logistic()), exposure = "no2",
                 cluster = c(1, 2, 3)),
    "3 values but the model was fitted to 4000 observations"
  )
  expect_error(
    foresty_main(list(fy_test_logistic()), exposure = "no2",
                 cluster = "nowhere"),
    "not a column of the data"
  )
})
