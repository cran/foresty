test_that("the result is a ggplot that takes layers, scales and themes", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2")
  expect_s3_class(x, "foresty")
  expect_s3_class(x, "gg")

  for (obj in list(x + ggplot2::labs(title = "Retitled"),
                   x + ggplot2::theme_minimal(base_size = 15),
                   x & ggplot2::theme_minimal(base_size = 15))) {
    expect_s3_class(obj, "foresty")
    # The estimates survive, so summary() and tidy() still work afterwards.
    expect_equal(nrow(fy_est(obj)), 1L)
  }
  expect_silent(ggplot2::ggplot_build(fy_forest_of(x)))
})

test_that("the table is on by default and `table = FALSE` takes it away", {
  with_table <- foresty_main(list(fy_test_logistic()), exposure = "no2")
  plain <- foresty_main(list(fy_test_logistic()), exposure = "no2",
                        table = FALSE)
  expect_s3_class(plain, "foresty")

  # The labels and the plot; the numbers are the third panel, drawn unless
  # `table = FALSE` says otherwise.
  expect_length(plain, 2L)
  expect_length(with_table, 3L)
})

test_that("`+` reaches the forest, not the table beside it", {
  # patchwork hands a ggplot element to the last plot it was given, so a
  # figure assembled left to right would send this to the table of numbers.
  x_range <- function(p) {
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x.range
  }
  wanted <- c(0.8, 2)   # a linear axis, so the limits are the values themselves

  plain <- foresty_main(list(fy_test_logistic()), exposure = "no2") +
    ggplot2::coord_cartesian(xlim = c(0.8, 2))
  expect_equal(x_range(plain[[length(plain)]]), wanted, tolerance = 0.1)

  tabled <- foresty_main(list(fy_test_logistic()), exposure = "no2",
                         table = TRUE) +
    ggplot2::coord_cartesian(xlim = c(0.8, 2))
  expect_equal(x_range(tabled[[length(tabled)]]), wanted, tolerance = 0.1)
})

test_that("an interaction is labelled by the levels of the modifier", {
  # One estimate per level, so the level names the row and no separate heading
  # is needed: the column reads Female and Male under the modifier's name,
  # not the exposure's name repeated.
  x <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "sex")

  labels <- fy_panel_text(x[[1]])
  expect_setequal(labels, c("sex", "Female", "Male"))

  # A categorical exposure has several rows to a level, so the modifier moves
  # to a panel of its own and the rows name the exposure's levels instead.
  fit <- glm(asthma ~ urbanicity + no2 + sex, family = binomial,
             data = foresty_cohort)
  expect_s3_class(
    foresty_interaction(fit, exposure = "urbanicity", interaction = "sex"),
    "patchwork"
  )
})

test_that("the table carries a p-value, and `columns` chooses the rest", {
  estimates <- fy_row_order(
    fy_result(foresty_main(list(fy_test_logistic()), exposure = "no2"))$estimates,
    NULL
  )
  columns <- fy_table_columns(estimates, "Odds ratio (95% CI)")
  expect_true("p" %in% attr(columns, "keys"))
  expect_true("p-value" %in% names(columns))

  chosen <- fy_choose_columns(columns, c("estimate", "n"))
  expect_equal(attr(chosen, "keys"), c("estimate", "n"))
})

test_that("summary reports the whole coefficient table", {
  x <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "sex")
  s <- summary(x)

  expect_s3_class(s, "summary.foresty")
  expect_true(is.matrix(s$coefficients))
  expect_equal(colnames(s$coefficients),
               c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
  expect_true("no2:sexMale" %in% rownames(s$coefficients))

  # The coefficients are on the scale the model was fitted on, as a model
  # summary is, while the estimates beside them are on the reported scale.
  # The table is of the updated model, the one carrying the interaction.
  expect_equal(unname(s$coefficients[, "Estimate"]),
               unname(coef(x)[rownames(s$coefficients)]),
               tolerance = 1e-10)
  expect_equal(s$estimates$estimate[1],
               exp(unname(s$coefficients["no2", "Estimate"])),
               tolerance = 1e-10)

  expect_output(print(s), "Coefficients:")
  expect_output(print(s), "Interaction")
})

test_that("a linear model summary reports t rather than z", {
  s <- summary(foresty_main(list(fy_test_linear()), exposure = "black_carbon"))
  expect_equal(colnames(s$coefficients)[3:4], c("t value", "Pr(>|t|)"))
})

test_that("tidy returns broom's column names", {
  x <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "sex")
  tidied <- broom::tidy(x)

  expect_s3_class(tidied, "data.frame")
  expect_true(all(c("term", "estimate", "std.error", "statistic", "p.value",
                    "conf.low", "conf.high") %in% names(tidied)))
  expect_equal(nrow(tidied), 2L)
  expect_type(tidied$term, "character")

  coefs <- broom::tidy(x, what = "coefficients")
  expect_equal(names(coefs),
               c("term", "estimate", "std.error", "statistic", "p.value",
                 "conf.low", "conf.high"))
  expect_equal(names(broom::tidy(x, what = "coefficients", conf.int = FALSE)),
               c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_true("no2:sexMale" %in% coefs$term)

  # The interval is formed by a different function from the standard error
  # beside it, so it is checked to be the same inference: the same covariance,
  # and the same choice between a z and a t.
  expect_equal(coefs$conf.low,
               coefs$estimate - stats::qnorm(0.975) * coefs$std.error)

  # A gaussian model is referred to a t on its residual degrees of freedom,
  # which is what confint() gives for one.
  linear <- fy_test_linear()
  lc <- broom::tidy(foresty_main(list(linear), exposure = "black_carbon"),
                    what = "coefficients")
  expect_equal(lc$conf.low, unname(stats::confint(linear)[, 1L]))
  expect_equal(lc$conf.high, unname(stats::confint(linear)[, 2L]))
})

test_that("glance describes the fit in one row", {
  g <- broom::glance(foresty_interaction(fy_test_logistic(), exposure = "no2",
                                  interaction = "sex"))
  expect_equal(nrow(g), 1L)
  expect_equal(g$measure, "OR")
  expect_equal(g$n, nrow(foresty_cohort))
  expect_equal(g$conf.level, 0.95)
  expect_false(g$robust)
  expect_true(is.finite(g$interaction.p.value))
})

test_that("glance says nothing of one model on a figure of several", {
  d <- foresty_cohort
  x <- foresty_main(
    list(glm(asthma ~ no2 + sex, family = binomial, data = d),
         glm(asthma ~ black_carbon + sex, family = binomial, data = d[1:2000, ])),
    exposure = c("no2", "black_carbon")
  )
  g <- broom::glance(x)

  # What the figure is holds for the whole of it; what a model was fitted to
  # does not, and reporting the first model's counts beside n_models = 2 says
  # they are the figure's.
  expect_equal(g$n_models, 2L)
  expect_equal(g$measure, "OR")
  expect_true(is.na(g$n))
  expect_true(is.na(g$events))

  # Naming the model gives that model's, as it does for every other method.
  expect_equal(broom::glance(x, model = 1)$n, nrow(d))
  expect_equal(broom::glance(x, model = 2)$n, 2000L)
})

test_that("the model methods pass through to the fit", {
  fit <- fy_test_logistic()
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")

  expect_equal(nobs(x), nrow(foresty_cohort))
  expect_true("no2:sexMale" %in% names(coef(x)))
  expect_equal(dim(vcov(x)), rep(length(coef(x)), 2))
  expect_match(deparse(formula(x)), "no2:sex", fixed = TRUE)
  expect_length(predict(x, type = "response"), nrow(foresty_cohort))
  expect_s3_class(model.frame(x), "data.frame")
})

test_that("methods ask which model is meant when a figure covers several", {
  d <- foresty_cohort
  x <- foresty_main(
    list(glm(asthma ~ no2 + sex, family = binomial, data = d),
         glm(asthma ~ black_carbon + sex, family = binomial, data = d)),
    exposure = c("no2", "black_carbon")
  )
  expect_error(coef(x), "`model` has to say which one")
  expect_true("no2" %in% names(coef(x, model = 1)))
  expect_true("black_carbon" %in% names(coef(x, model = 2)))
})
