test_that("the measure names the outcome it is a measure of", {
  expect_equal(fy_model_info(fy_test_logistic())$measure_label,
               "Odds ratio for asthma")
  expect_equal(fy_model_info(fy_test_logistic())$measure_name, "Odds ratio")
  expect_equal(fy_model_info(fy_test_linear())$measure_label,
               "Mean difference for no2")
})

test_that("a survival outcome is named by its event, not its time", {
  skip_if_not_installed("survival")
  fit <- survival::coxph(
    survival::Surv(followup_years, wheeze) ~ no2 + sex, data = foresty_cohort
  )
  expect_equal(fy_outcome_label(fit), "wheeze")
  expect_equal(fy_model_info(fit)$measure_label, "Hazard ratio for wheeze")
})

test_that("the header names the outcome and keeps the interval separate", {
  info <- fy_model_info(fy_test_logistic())
  expect_equal(fy_estimate_header(info, adjusted = TRUE, ci_level = 0.95),
               "Adjusted odds ratio for asthma (95% CI)")
  expect_equal(fy_estimate_header(info, adjusted = FALSE, ci_level = 0.95),
               "Odds ratio for asthma (95% CI)")
  expect_equal(
    fy_estimate_header(info, adjusted = TRUE, ci_level = 0.9, with_ci = FALSE),
    "Adjusted odds ratio for asthma"
  )

  # On the figure the heading takes the depth and the plot takes the width: the
  # interval goes on a line of its own and the rest is wrapped above it.
  estimates <- fy_row_order(
    fy_result(foresty_main(list(fy_test_logistic()), exposure = "no2"))$estimates,
    NULL
  )
  header <- names(fy_table_columns(estimates, "Adjusted odds ratio for asthma (95% CI)"))[1]
  expect_equal(header, "Adjusted odds\nratio for asthma\n(95% CI)")
})

test_that("the axis is linear, ratios included", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2")
  built <- ggplot2::ggplot_build(x)
  range <- built$layout$panel_params[[1]]$x.range

  # On a log axis the estimate would be stored as its logarithm and the range
  # would straddle zero; here it straddles one.
  expect_gt(range[1], 0.5)
  expect_true(1 >= range[1] && 1 <= range[2])
  expect_true(1 %in% built$layout$panel_params[[1]]$x$breaks)
})

test_that("the estimates are drawn in black", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2")
  layers <- ggplot2::ggplot_build(x)$data
  points <- layers[[length(layers)]]
  expect_true(all(points$colour == "black"))
})

test_that("no report is written unless one is asked for", {
  file <- file.path(tempdir(), "foresty-should-not-exist.html")
  unlink(file)
  foresty_interaction(fy_test_logistic(), exposure = "no2", interaction = "sex")
  expect_false(file.exists(file))
  expect_length(list.files(tempdir(), pattern = "^foresty-should-not-exist"), 0L)
})

test_that("the statistic is a signed z, not the squared one", {
  fit <- glm(asthma ~ no2 * sex + maternal_age,
             family = binomial, data = foresty_cohort)
  est <- fy_est(foresty_interaction(fit, exposure = "no2", interaction = "sex"))

  z <- log(est$estimate) / est$se
  expect_equal(est$statistic, z, tolerance = 1e-10)
  expect_true(all(est$statistic > 0))
})

test_that("the title says what the figure shows, and not the outcome twice", {
  x <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "sex")
  title <- gsub("\n", " ", fy_title(x), fixed = TRUE)

  # What the measure is a measure of is the outcome, so that is what follows
  # it; the exposure the effect is reported for comes after that.
  expect_match(
    title,
    "^Adjusted odds ratio for asthma associated with no2 within each level of sex"
  )
  expect_match(title, "one model containing their interaction term")
  expect_false(grepl("for asthma for", title, fixed = TRUE))

  # One line of text over the figure is enough; the subtitle is left free for
  # whatever the caller wants to put there.
  expect_null(fy_subtitle(x))

  # The axis carries the outcome, so nothing is lost by leaving it out above.
  expect_match(ggplot2::ggplot_build(fy_forest_of(x))$plot$labels$x,
               "Adjusted odds ratio for asthma", fixed = TRUE)
})

test_that("a figure with no interaction says so, over a column of exposures", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2",
                    labels = c(no2 = "NO2"))
  expect_equal(gsub("\n", " ", fy_title(x), fixed = TRUE),
               paste("Adjusted odds ratio for asthma associated with NO2,",
                     "from one model without an interaction term"))
  expect_null(fy_subtitle(x))

  # The rows are the exposures, and the column carrying them says so.
  expect_true(all(c("Exposure", "NO2") %in% fy_panel_text(x[[1]])))

  # Several models, none of them interacted, are counted.
  d <- foresty_cohort
  two <- foresty_main(
    list(glm(asthma ~ no2 + sex, family = binomial, data = d),
         glm(asthma ~ black_carbon + sex, family = binomial, data = d)),
    exposure = c("no2", "black_carbon")
  )
  expect_match(
    gsub("\n", " ", fy_title(two), fixed = TRUE),
    paste("^Adjusted odds ratio for asthma associated with no2 and",
          "black_carbon, from 2 models")
  )

  # `NA` is how a title is turned off; a string of one's own is drawn instead.
  expect_null(fy_title(foresty_main(list(fy_test_logistic()), "no2",
                                    title = NA)))
  expect_equal(fy_title(foresty_main(list(fy_test_logistic()), "no2",
                                     title = "Air pollution")),
               "Air pollution")
})

test_that("naming the exposure or the modifier labels it", {
  fit <- fy_test_logistic()

  x <- foresty_interaction(fit, exposure = c(NO2 = "no2"),
                           interaction = c(Sex = "sex"))
  est <- fy_est(x)
  expect_equal(unique(est$variable_label), "NO2")
  expect_equal(fy_result(x)$modifier_display, "Sex")
  # The variable itself is untouched: only what it is called changed.
  expect_equal(unique(est$variable), "no2")
  expect_equal(fy_result(x)$modifier, "sex")
  expect_true("Sex" %in% fy_panel_text(x[[1]]))

  # The block a combined figure gives it is the label too.
  expect_equal(unique(fy_est(foresty_combine(x))$block_label), "Sex")

  # The same in foresty_main().
  expect_equal(
    unique(fy_est(foresty_main(list(fit), exposure = c(NO2 = "no2")))$variable_label),
    "NO2"
  )

  # A name written beside the variable is the more specific of the two, so it
  # wins over `labels`; anything else in `labels` is left alone.
  both <- foresty_interaction(fit, exposure = c(NO2 = "no2"),
                              interaction = c(Sex = "sex"),
                              labels = c(no2 = "Nitrogen dioxide",
                                         sex = "Sex at birth"))
  expect_equal(unique(fy_est(both)$variable_label), "NO2")
  expect_equal(fy_result(both)$modifier_display, "Sex")
})

test_that("the heading over the row labels can be renamed or dropped", {
  x <- foresty_main(
    list(fy_test_logistic()), exposure = "no2",
    layout = foresty_layout("classic", headings = c(label = "Pollutant"))
  )
  expect_true("Pollutant" %in% fy_panel_text(x[[1]]))
  expect_false("Exposure" %in% fy_panel_text(x[[1]]))
})

# What things are called ------------------------------------------------------

test_that("the outcome can be named, and is named everywhere it appears", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2",
                    outcome = "incident asthma")

  expect_equal(fy_result(x)$measure_label, "Odds ratio for incident asthma")
  expect_equal(fy_axis_text(x), "Adjusted odds ratio for incident asthma")
  expect_match(gsub("\n", " ", fy_title(x), fixed = TRUE),
               "Adjusted odds ratio for incident asthma associated with no2")

  # And on the interaction figure, whose title is built out of the same phrase.
  y <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "sex", outcome = "incident asthma")
  expect_match(gsub("\n", " ", fy_title(y), fixed = TRUE),
               "^Adjusted odds ratio for incident asthma associated with no2")
})

test_that("naming no outcome leaves the measure on its own", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2", outcome = NA)
  expect_equal(fy_result(x)$measure_label, "Odds ratio")
  expect_equal(fy_axis_text(x), "Adjusted odds ratio")
  expect_false(grepl("asthma", fy_title(x), fixed = TRUE))
})

test_that("the label under the plot can be replaced or left off", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2",
                    xlab = "Odds ratio (95% CI)")
  expect_equal(fy_axis_text(x), "Odds ratio (95% CI)")

  # The title is the package's own either way: xlab is the axis and nothing
  # else.
  expect_match(fy_title(x), "asthma")

  none <- foresty_main(list(fy_test_logistic()), exposure = "no2", xlab = NA)
  expect_null(ggplot2::ggplot_build(fy_forest_of(none))$plot$labels$x)
})

test_that("a combined figure takes the outcome and the axis too", {
  fit <- fy_test_logistic()
  overall <- foresty_main(list(fit), exposure = "no2")
  by_sex <- foresty_interaction(fit, exposure = "no2", interaction = "sex")

  x <- foresty_combine(Overall = overall, Sex = by_sex,
                       outcome = "incident asthma")
  expect_match(fy_axis_text(x), "for incident asthma", fixed = TRUE)
  expect_match(fy_title(x), "incident asthma")

  y <- foresty_combine(Overall = overall, Sex = by_sex, xlab = "OR (95% CI)")
  expect_equal(fy_axis_text(y), "OR (95% CI)")
})
