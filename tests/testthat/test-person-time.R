test_that("a Cox model reports the person-time behind each subgroup", {
  skip_if_not_installed("survival")
  d <- foresty_cohort
  fit <- survival::coxph(
    survival::Surv(followup_years, wheeze) ~ no2 + sex, data = d
  )
  est <- fy_est(foresty_interaction(fit, exposure = "no2", interaction = "sex"))

  expect_true(all(is.finite(est$person_time)))
  expect_equal(sum(est$person_time), sum(d$followup_years), tolerance = 1e-8)
  expect_equal(est$person_time[1],
               sum(d$followup_years[d$sex == "Female"]), tolerance = 1e-8)
})

test_that("a counting-process outcome measures the interval, not the stop time", {
  skip_if_not_installed("survival")
  d <- foresty_cohort
  d$start <- 0.25
  fit <- survival::coxph(
    survival::Surv(start, followup_years, wheeze) ~ no2 + sex,
    data = d[d$followup_years > 0.25, ]
  )
  info <- fy_model_info(fit)
  kept <- d[d$followup_years > 0.25, ]
  expect_equal(info$person_time, sum(kept$followup_years - 0.25),
               tolerance = 1e-8)
})

test_that("a Poisson model with an offset reports rates and person-time", {
  d <- foresty_cohort
  set.seed(1)
  d$follow_up <- stats::runif(nrow(d), 1, 5)
  fit <- glm(asthma ~ no2 + sex + offset(log(follow_up)),
             family = poisson, data = d)

  # An offset is what makes a Poisson model a rate model, so its ratio is an
  # incidence rate ratio rather than a risk ratio.
  expect_equal(fy_model_info(fit)$measure, "IRR")

  est <- fy_est(foresty_interaction(fit, exposure = "no2", interaction = "sex"))
  expect_equal(sum(est$person_time), sum(d$follow_up), tolerance = 1e-6)
  expect_equal(est$person_time[1], sum(d$follow_up[d$sex == "Female"]),
               tolerance = 1e-6)
})

test_that("a Poisson model without an offset reports risk and no person-time", {
  fit <- glm(asthma ~ no2 + sex, family = poisson, data = foresty_cohort)
  expect_equal(fy_model_info(fit)$measure, "RR")
  expect_true(is.na(fy_est(foresty_main(list(fit), exposure = "no2"))$person_time))
})

test_that("a model with no time at all reports none", {
  est <- fy_est(foresty_main(list(fy_test_logistic()), exposure = "no2"))
  expect_true(is.na(est$person_time))
})

# The unit it is counted in ---------------------------------------------------

fy_rate_fit <- function() {
  d <- foresty_cohort
  set.seed(1)
  d$follow_up <- stats::runif(nrow(d), 1, 5)
  glm(asthma ~ no2 + sex + offset(log(follow_up)), family = poisson, data = d)
}

# The person-time column of the table beside the plot, as the figure writes it.
fy_person_time_column <- function(x, group = "modifier_label") {
  result <- fy_result(x)
  estimates <- fy_row_order(result$estimates, group)
  cells <- fy_table_cells(estimates, "Adjusted incidence rate ratio (95% CI)",
                          NULL, fy_as_layout("classic"),
                          person_time = result$person_time)
  heading <- grep("Person-", cells$headers$label, value = TRUE)
  expect_length(heading, 1L)
  at <- cells$headers$x[cells$headers$label == heading]
  list(heading = heading,
       values = cells$values$label[cells$values$x == at])
}

test_that("person-time is the total unless a unit says otherwise", {
  x <- foresty_interaction(fy_rate_fit(), exposure = "no2", interaction = "sex")
  column <- fy_person_time_column(x)

  expect_equal(column$heading, "Person-time")
  # A total is a count, written with its thousands separated.
  expect_true(all(grepl("^[0-9,]+$", column$values)))
})

test_that("a unit divides the person-time and says so over the column", {
  x <- foresty_interaction(fy_rate_fit(), exposure = "no2", interaction = "sex",
                           person_time = 1000)
  column <- fy_person_time_column(x)

  expect_equal(column$heading, "Person-time\n(per 1,000)")
  # Thousands of person-years, which are not counts and keep a decimal.
  expect_true(all(grepl("^[0-9,]+[.][0-9]$", column$values)))

  est <- fy_est(x)
  expect_equal(as.numeric(gsub(",", "", column$values)),
               round(est$person_time / 1000, 1))
  # The estimates themselves are untouched: the unit is how the column is
  # written and not what was fitted.
  expect_equal(sum(est$person_time), sum(exp(stats::model.offset(
    stats::model.frame(fy_rate_fit())
  ))), tolerance = 1e-6)
})

test_that("naming the unit names the column", {
  x <- foresty_interaction(fy_rate_fit(), exposure = "no2", interaction = "sex",
                           person_time = c(`Person-years (thousands)` = 1000))
  expect_equal(fy_person_time_column(x)$heading, "Person-years (thousands)")
})

test_that("the unit reaches summary() and the HTML report", {
  x <- foresty_interaction(fy_rate_fit(), exposure = "no2", interaction = "sex",
                           person_time = 1000)
  printed <- paste(utils::capture.output(print(summary(x))), collapse = "\n")
  expect_match(printed, "Person-time (per 1,000)", fixed = TRUE)

  skip_if_not_installed("gt")
  file <- fy_temp_html()
  foresty_report(x, file = file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "Person-time (per 1,000)", fixed = TRUE)
})

test_that("a combined figure keeps the unit its figures were drawn in", {
  fit <- fy_rate_fit()
  overall <- foresty_main(list(fit), exposure = "no2", person_time = 1000)
  by_sex <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                                person_time = 1000)

  combined <- foresty_combine(Overall = overall, Sex = by_sex)
  expect_equal(fy_result(combined)$person_time$unit, 1000)
  expect_equal(fy_person_time_column(combined, "block_label")$heading,
               "Person-time\n(per 1,000)")

  # And can be told another one.
  expect_equal(
    fy_result(foresty_combine(Overall = overall, Sex = by_sex,
                              person_time = 100))$person_time$unit,
    100
  )
})

test_that("a unit that is not one is refused", {
  fit <- fy_rate_fit()
  expect_error(foresty_main(list(fit), exposure = "no2", person_time = 0),
               "positive number")
  expect_error(foresty_main(list(fit), exposure = "no2",
                            person_time = c(1000, 100)),
               "positive number")
  expect_error(foresty_main(list(fit), exposure = "no2",
                            person_time = "1000"),
               "positive number")
})
