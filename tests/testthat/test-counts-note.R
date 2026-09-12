# A row of a forest plot is usually half of a comparison, and the columns of
# counts carry the other half beside it. These are the tests of which two
# groups those are, of the rows that compare no two groups and so carry one
# number, and of what the figure says about them.

# The cells of the table beside the plot, as the figure draws them.
fy_cells <- function(x, layout = fy_style("classic")) {
  cells <- fy_table_columns(fy_result(x)$estimates, "OR (95% CI)",
                            layout = layout)
  as.data.frame(cells, check.names = FALSE, stringsAsFactors = FALSE)
}

fy_said <- function(x) paste(fy_result(x)$counts_note, collapse = " ")

fy_comma <- function(x) formatC(as.integer(x), format = "d", big.mark = ",")

test_that("a categorical exposure carries its level and the reference level", {
  fit <- glm(asthma ~ urbanicity + sex, family = binomial, data = foresty_cohort)
  figure <- foresty_interaction(fit, exposure = "urbanicity",
                                interaction = "sex")

  d <- foresty_cohort[stats::complete.cases(
    foresty_cohort[, c("asthma", "urbanicity", "sex")]), ]
  females <- d[d$sex == "Female", ]
  sizes <- table(females$urbanicity)
  events <- tapply(females$asthma, females$urbanicity, sum)

  cells <- fy_cells(figure)
  # The reference row is the group the others are compared with rather than a
  # comparison of its own, so it carries one number.
  expect_equal(cells$N[1L], fy_comma(sizes[["Rural"]]))
  expect_equal(cells$Events[1L], fy_comma(events[["Rural"]]))
  # Every other row carries the pair its odds ratio came out of.
  expect_equal(cells$N[2L], paste0(fy_comma(sizes[["Suburban"]]), " vs ",
                                   fy_comma(sizes[["Rural"]])))
  expect_equal(cells$Events[2L], paste0(fy_comma(events[["Suburban"]]), " vs ",
                                        fy_comma(events[["Rural"]])))

  expect_match(fy_said(figure), "the group its estimate is compared with")
  expect_match(fy_said(figure), "rows the model was fitted to")
})

test_that("a multinomial fit of a continuous exposure counts its two levels", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex, data = foresty_cohort,
                        trace = FALSE)
  figure <- foresty_interaction(fit, exposure = "no2", interaction = "sex")

  counts <- table(foresty_cohort$wheeze_phenotype, foresty_cohort$sex)
  cells <- fy_cells(figure)
  # The two levels the row compares, and nothing else: the events column would
  # repeat the first of them, so it is left off.
  expect_equal(cells$N[1L], paste0(fy_comma(counts["Transient", "Female"]),
                                   " vs ", fy_comma(counts["None", "Female"])))
  expect_false("Events" %in% names(cells))

  # And the caution the counts cannot give: the estimate was not fitted on
  # those two groups alone.
  expect_match(fy_said(figure), "two levels of the outcome")
  expect_match(fy_said(figure), "did not come out of those groups alone")
})

test_that("a multinomial fit of a categorical exposure pairs both columns", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(wheeze_phenotype ~ urbanicity + sex,
                        data = foresty_cohort, trace = FALSE)
  figure <- foresty_main(list(fit), exposure = "urbanicity")

  sizes <- table(foresty_cohort$urbanicity)
  transient <- table(foresty_cohort$urbanicity,
                     foresty_cohort$wheeze_phenotype)[, "Transient"]

  cells <- fy_cells(figure)
  # The sizes are the two levels of the exposure whatever their outcome, and
  # the events are the people at the row's outcome level within each of them.
  expect_equal(cells$N[2L], paste0(fy_comma(sizes[["Suburban"]]), " vs ",
                                   fy_comma(sizes[["Rural"]])))
  expect_equal(cells$Events[2L], paste0(fy_comma(transient[["Suburban"]]),
                                        " vs ", fy_comma(transient[["Rural"]])))
  expect_match(fy_said(figure), "whatever their outcome")
})

test_that("a row that compares no two groups of people carries one number", {
  # A step along a continuous exposure is not two groups, so there is nothing
  # to pair and nothing to explain.
  fit <- fy_test_logistic()
  figure <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  cells <- fy_cells(figure)
  expect_false(any(grepl(" vs ", c(cells$N, cells$Events), fixed = TRUE)))
  expect_length(fy_result(figure)$counts_note, 0L)
  expect_length(fy_result(foresty_main(list(fit), exposure = "no2"))$counts_note,
                0L)
})

test_that("counts of the row alone is what the option asks for", {
  fit <- glm(asthma ~ urbanicity + sex, family = binomial, data = foresty_cohort)
  as_rows <- foresty_layout(counts = "row")
  figure <- foresty_interaction(fit, exposure = "urbanicity",
                                interaction = "sex", layout = as_rows)
  cells <- fy_cells(figure, as_rows)
  expect_false(any(grepl(" vs ", c(cells$N, cells$Events), fixed = TRUE)))

  # And what it says changes with it: the other half of every comparison is
  # then somewhere else on the figure, which is the thing to say.
  expect_match(fy_said(figure), "no row carries the total behind its own")
  expect_false(grepl("side by side", fy_said(figure), fixed = TRUE))

  expect_error(foresty_layout(counts = "both"), "should be one of")
})

test_that("one reference cell for the whole figure is what each row pairs with", {
  fit <- glm(asthma ~ urbanicity + sex, family = binomial, data = foresty_cohort)
  figure <- foresty_interaction(
    fit, exposure = "urbanicity", interaction = "sex",
    reference = c(urbanicity = "Rural", sex = "Female")
  )
  d <- foresty_cohort[stats::complete.cases(
    foresty_cohort[, c("asthma", "urbanicity", "sex")]), ]
  cell <- sum(d$sex == "Female" & d$urbanicity == "Rural")

  # Every row is compared with that one cell, so every row that has a
  # comparison carries its count -- the male rows included.
  est <- fy_result(figure)$estimates
  expect_equal(unique(stats::na.omit(est$n_compared)), cell)
  expect_true(all(is.na(est$n_compared[est$reference])))
})

test_that("nothing is said about columns the figure does not draw", {
  fit <- glm(asthma ~ urbanicity + sex, family = binomial, data = foresty_cohort)
  expect_length(
    fy_result(foresty_main(list(fit), exposure = "urbanicity",
                           table = FALSE))$counts_note,
    0L
  )
  expect_length(
    fy_result(foresty_main(list(fit), exposure = "urbanicity",
                           columns = c("estimate", "p")))$counts_note,
    0L
  )
})

test_that("a combined figure pairs its rows and says so once", {
  fit <- glm(asthma ~ urbanicity + sex, family = binomial, data = foresty_cohort)
  overall <- foresty_main(list(fit), exposure = "urbanicity")
  by_sex <- foresty_interaction(fit, exposure = "urbanicity",
                                interaction = "sex")
  combined <- foresty_combine(overall, by_sex)

  expect_true(any(grepl(" vs ", fy_cells(combined)$N, fixed = TRUE)))
  expect_equal(fy_result(combined)$counts_note, fy_result(overall)$counts_note)
  expect_false(anyDuplicated(fy_result(combined)$counts_note) > 0L)

  # The combined figure writes its own sentences rather than carrying over the
  # ones the figures it was made from wrote, so its own layout settles them.
  as_rows <- foresty_combine(overall, by_sex,
                             layout = foresty_layout(counts = "row"))
  expect_match(paste(fy_result(as_rows)$counts_note, collapse = " "),
               "no row carries the total behind its own")
})

test_that("the report holds the counts the way the figure holds them", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex, data = foresty_cohort,
                        trace = FALSE)
  file <- fy_temp_html()
  on.exit(unlink(file), add = TRUE)
  foresty_report(foresty_interaction(fit, exposure = "no2",
                                     interaction = "sex"), file = file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "637 vs 1,050", fixed = TRUE)
  expect_match(html, "did not come out of those groups alone")

  # The page writes N and Events whatever the figure was drawn with, so it has
  # a table to explain even where the figure had none.
  file2 <- fy_temp_html()
  on.exit(unlink(file2), add = TRUE)
  foresty_report(foresty_interaction(fit, exposure = "no2",
                                     interaction = "sex", table = FALSE),
                 file = file2)
  expect_match(paste(readLines(file2, warn = FALSE), collapse = "\n"),
               "two levels of the outcome")
})

test_that("a figure of numbers alone is left as it was given", {
  # foresty_data() is given the counts rather than counting anything, so it has
  # no second group to report and nothing to say about the ones it has.
  figure <- foresty_data(
    data.frame(label = c("Female", "Male"), estimate = c(1.2, 1.4),
               conf.low = c(1.0, 1.1), conf.high = c(1.5, 1.8),
               n = c(100L, 120L)),
    measure = "OR"
  )
  expect_null(fy_result(figure)$counts_note)
  expect_null(fy_result(figure)$counts_reading)
  expect_equal(fy_cells(figure)$N, c("100", "120"))
})
