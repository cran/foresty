test_that("foresty_main returns one row per exposure across several models", {
  d <- foresty_cohort
  a <- glm(asthma ~ no2 + sex, family = binomial, data = d)
  b <- glm(asthma ~ black_carbon + sex, family = binomial, data = d)
  est <- fy_est(foresty_main(list(a, b), exposure = c("no2", "black_carbon")))

  expect_equal(nrow(est), 2L)
  expect_equal(est$variable, c("no2", "black_carbon"))
  expect_true(all(is.na(est$level)))
})

test_that("a continuous exposure is labelled by its name, with no unit invented", {
  est <- fy_est(foresty_main(list(fy_test_logistic()), exposure = "no2"))
  expect_equal(est$label, "no2")
  expect_false(grepl("unit", est$label))

  # An effect reported per some other increment says which, since the numbers
  # cannot be read without it, but no unit is invented for it: the package has
  # no way of knowing what the numbers in the column mean.
  ten <- fy_est(foresty_main(list(fy_test_logistic()), exposure = "no2",
                             contrast = 10))
  expect_equal(as.character(ten$label), "no2 (per 10)")
  expect_false(grepl("unit", ten$label))

  # The unit belongs in the label, and the increment is written after it.
  named <- fy_est(foresty_main(list(fy_test_logistic()), exposure = "no2",
                               labels = c(no2 = "NO2, ug/m3"), contrast = 10))
  expect_equal(as.character(named$label), "NO2, ug/m3 (per 10)")
})

test_that("`contrast = \"iqr\"` takes the increment from the data and says which", {
  fit <- fy_test_logistic()
  iqr <- stats::IQR(foresty_cohort$no2, na.rm = TRUE)

  est <- fy_est(foresty_main(list(fit), exposure = "no2", contrast = "iqr"))
  explicit <- fy_est(foresty_main(list(fit), exposure = "no2", contrast = iqr))
  expect_equal(est$estimate, explicit$estimate, tolerance = 1e-12)

  # The range that was used is written beside the exposure: an effect per
  # interquartile range cannot be compared with anything without it.
  expect_equal(as.character(est$label),
               paste0("no2 (per IQR, ",
                      format(iqr, digits = 3, trim = TRUE,
                             drop0trailing = TRUE), ")"))

  # It reaches a subgroup analysis the same way.
  by_sex <- fy_est(foresty_interaction(fit, exposure = "no2",
                                       interaction = "sex", contrast = "iqr"))
  expect_match(as.character(by_sex$variable_label[1L]), "per IQR", fixed = TRUE)
  expect_equal(by_sex$estimate[1L],
               fy_est(foresty_interaction(fit, exposure = "no2",
                                          interaction = "sex",
                                          contrast = iqr))$estimate[1L],
               tolerance = 1e-12)
})

test_that("an interquartile range is refused where there is none to take", {
  fit <- fy_test_logistic()

  # A categorical exposure has levels rather than a range, and the remedy is
  # the one that names two of them.
  expect_error(foresty_main(list(fit), exposure = "sex", contrast = "iqr"),
               "does not have")
  expect_error(foresty_main(list(fit), exposure = "no2", contrast = "sd"),
               "must be a number")
  expect_error(
    foresty_main(list(fit), exposure = "no2", contrast = "iqr", at = c(10, 20)),
    "only one of them can be given"
  )
})

test_that("`at` and `contrast` are two ways of saying the same thing", {
  fit <- fy_test_logistic()
  expect_error(
    foresty_main(list(fit), exposure = "no2", at = c(10, 20), contrast = 10),
    "only one of them can be given"
  )

  # `at` is not for splines alone: any continuous exposure can have its two
  # values named. Which two is a fact about the figure rather than about a row
  # of it, so it is written beside the exposure, where every row can be read
  # against it.
  x <- foresty_main(list(fit), exposure = c(NO2 = "no2"), at = c(10, 20),
                    exponentiate = FALSE)
  est <- fy_est(x)
  expect_equal(est$contrast_label, "10 \u2192 20")
  expect_equal(as.character(est$label), "NO2 (10 \u2192 20)")
  expect_true("NO2 (10 \u2192 20)" %in% fy_panel_text(x[[1]]))
  expect_equal(est$estimate, unname(coef(fit)["no2"]) * 10, tolerance = 1e-8)

  # A categorical exposure is compared level by level, so `contrast` says
  # nothing about it and says so rather than being ignored.
  urban <- glm(asthma ~ urbanicity + sex, family = binomial,
               data = foresty_cohort)
  expect_warning(
    foresty_main(list(urban), exposure = "urbanicity", contrast = 10),
    "categorical"
  )

  # Two of its levels can be named instead, and the row is counted over the
  # level the comparison is of rather than over a level of that name, which
  # there is none of.
  two <- fy_est(foresty_main(list(urban), exposure = "urbanicity",
                             at = c("Rural", "Urban")))
  expect_equal(two$contrast_label, "Rural \u2192 Urban")
  expect_equal(two$estimate, unname(exp(coef(urban)["urbanicityUrban"])),
               tolerance = 1e-8)
  expect_equal(two$n, sum(foresty_cohort$urbanicity == "Urban"))
})

test_that("`contrast` scales a continuous exposure", {
  fit <- fy_test_logistic()
  one <- fy_est(foresty_main(list(fit), exposure = "no2", contrast = 1))
  ten <- fy_est(foresty_main(list(fit), exposure = "no2", contrast = 10))

  expect_equal(ten$estimate, one$estimate^10, tolerance = 1e-8)
})

test_that("a categorical exposure gives a row per level, the reference shown", {
  fit <- glm(asthma ~ urbanicity + no2 + sex, family = binomial,
             data = foresty_cohort)
  est <- fy_est(foresty_main(list(fit), exposure = "urbanicity"))

  expect_equal(nrow(est), 3L)
  expect_equal(est$level, c("Rural", "Suburban", "Urban"))
  expect_equal(est$reference, c(TRUE, FALSE, FALSE))

  # The reference row is a definition, so it sits at the null with no interval.
  expect_equal(est$estimate[1], 1)
  expect_true(is.na(est$conf.low[1]))
  expect_true(all(is.finite(est$estimate[-1])))

  # The level names the row, and the variable is carried alongside for the
  # strip at the left of the figure rather than repeated on every row.
  expect_equal(est$label, c("Rural", "Suburban", "Urban"))
  expect_equal(unique(est$variable_label), "urbanicity")
})

test_that("a categorical exposure is counted level by level", {
  fit <- glm(asthma ~ urbanicity + no2 + sex, family = binomial,
             data = foresty_cohort)
  est <- fy_est(foresty_main(list(fit), exposure = "urbanicity"))

  expect_equal(sum(est$n), nrow(foresty_cohort))
  expect_equal(sum(est$events), sum(foresty_cohort$asthma))
  expect_equal(est$n, unname(table(foresty_cohort$urbanicity)[est$level]),
               ignore_attr = TRUE)
})

test_that("a categorical exposure crossed with a modifier gives every pairing", {
  fit <- glm(asthma ~ urbanicity + no2 + sex, family = binomial,
             data = foresty_cohort)
  est <- fy_est(foresty_interaction(fit, exposure = "urbanicity",
                                    interaction = "sex"))

  expect_equal(nrow(est), 6L)
  expect_equal(unique(est$modifier_level), c("Female", "Male"))
  expect_equal(sum(est$n), nrow(foresty_cohort))
})

test_that("`reference` reads every combination against one of them", {
  fit <- glm(asthma ~ urbanicity + sex + maternal_age, family = binomial,
             data = foresty_cohort)
  x <- foresty_interaction(fit, exposure = "urbanicity", interaction = "sex",
                           reference = c(urbanicity = "Rural", sex = "Female"))
  est <- fy_est(x)

  # The same six rows as before, but one reference among them rather than one
  # per subgroup: every other row is a comparison with that one.
  expect_equal(nrow(est), 6L)
  expect_equal(est$reference, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(est$estimate[1], 1)
  expect_true(is.na(est$conf.low[1]))
  expect_true(all(is.finite(est$estimate[-1])))
  expect_equal(sum(est$n), nrow(foresty_cohort))

  # And the numbers are the coefficients of the interaction model added up by
  # hand: Urban and Male against Rural and Female is the exposure term, the
  # modifier term and the interaction term together.
  b <- coef(glm(asthma ~ urbanicity * sex + maternal_age, family = binomial,
                data = foresty_cohort))
  expect_equal(
    est$estimate[est$modifier_level == "Male" & est$level == "Urban"],
    unname(exp(b["urbanicityUrban"] + b["sexMale"] +
                 b["urbanicityUrban:sexMale"])),
    tolerance = 1e-8
  )

  # The test beside the rows is the test of the interaction, which the choice
  # of reference group has nothing to do with.
  plain <- foresty_interaction(fit, exposure = "urbanicity",
                               interaction = "sex")
  expect_equal(fy_test(x)$p.value, fy_test(plain)$p.value)
})

test_that("`reference = TRUE` takes the first level of each", {
  fit <- glm(asthma ~ urbanicity + sex + maternal_age, family = binomial,
             data = foresty_cohort)
  named <- fy_est(foresty_interaction(
    fit, exposure = "urbanicity", interaction = "sex",
    reference = c(urbanicity = "Rural", sex = "Female")
  ))
  expect_equal(fy_est(foresty_interaction(fit, exposure = "urbanicity",
                                          interaction = "sex",
                                          reference = TRUE))$estimate,
               named$estimate)
})

test_that("a reference group needs two variables with levels, named", {
  fit <- glm(asthma ~ urbanicity + sex + maternal_age, family = binomial,
             data = foresty_cohort)

  # A continuous exposure has no levels to combine with anything, and the
  # refusal says what to do instead rather than reporting a slope as a group.
  expect_error(
    foresty_interaction(fy_test_logistic(), exposure = "no2",
                        interaction = "sex", reference = TRUE),
    "continuous"
  )
  # A level that is not one of the variable's, and a pair that does not say
  # which level belongs to which variable.
  expect_error(
    foresty_interaction(fit, exposure = "urbanicity", interaction = "sex",
                        reference = c(urbanicity = "Nowhere", sex = "Female")),
    "not a level"
  )
  expect_error(
    foresty_interaction(fit, exposure = "urbanicity", interaction = "sex",
                        reference = c("Rural", "Female")),
    "names a level"
  )
  # `at` says which comparison is made as well, and one of them would have to
  # be ignored.
  expect_error(
    foresty_interaction(fit, exposure = "urbanicity", interaction = "sex",
                        reference = TRUE, at = c("Rural", "Urban")),
    "only one of them"
  )
})

test_that("labels rename variables and modifier levels", {
  est <- fy_est(foresty_interaction(
    fy_test_logistic(), exposure = "no2", interaction = "sex",
    labels = c(no2 = "Nitrogen dioxide"),
    level_labels = c(Female = "Girls", Male = "Boys")
  ))
  expect_equal(est$label[1], "Nitrogen dioxide")
  expect_equal(est$modifier_label, c("Girls", "Boys"))
})

test_that("subgroup sizes add up to the analysed sample", {
  est <- fy_est(foresty_interaction(fy_test_logistic(), exposure = "no2",
                                    interaction = "sex"))
  expect_equal(sum(est$n), nrow(foresty_cohort))
  expect_equal(sum(est$events), sum(foresty_cohort$asthma))
})

test_that("rows with a missing value are dropped from the counts too", {
  d <- foresty_cohort
  d$no2[1:100] <- NA
  fit <- glm(asthma ~ no2 + sex + maternal_age, family = binomial, data = d)
  est <- fy_est(foresty_interaction(fit, exposure = "no2", interaction = "sex"))

  expect_equal(sum(est$n), nrow(d) - 100L)
})

test_that("an interaction that is absent gives similar subgroups and a large p", {
  # maternal_smoking was simulated with no effect on the exposure effect.
  x <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "maternal_smoking")
  est <- fy_est(x)
  expect_gt(fy_test(x)$p.value, 0.2)
  expect_equal(est$estimate[1], est$estimate[2], tolerance = 0.05)
})

test_that("an interaction that is present separates the subgroups", {
  # sex was simulated with the exposure effect about twice as large in boys.
  x <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "sex")
  est <- fy_est(x)
  expect_lt(fy_test(x)$p.value, 0.05)
  expect_lt(est$conf.high[1], est$conf.high[2])
})
