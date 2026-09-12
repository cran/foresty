fy_combine_pieces <- function() {
  fit <- fy_test_logistic()
  list(
    overall = foresty_main(list(fit), exposure = "no2"),
    by_sex = foresty_interaction(fit, exposure = "no2", interaction = "sex"),
    by_smoking = foresty_interaction(fit, exposure = "no2",
                                     interaction = "maternal_smoking")
  )
}

test_that("the blocks are drawn one under the other, in the order given", {
  p <- fy_combine_pieces()
  x <- foresty_combine(Overall = p$overall, Sex = p$by_sex,
                       Smoking = p$by_smoking)
  est <- fy_est(x)

  expect_s3_class(x, "foresty")
  expect_equal(nrow(est), 5L)
  expect_equal(as.character(est$block_label),
               c("Overall", "Sex", "Sex", "Smoking", "Smoking"))
  expect_equal(unique(est$block_label), c("Overall", "Sex", "Smoking"))
  expect_equal(as.character(est$label),
               c("no2", "Female", "Male", "No", "Yes"))
})

test_that("nothing is re-estimated: every row is the row it was drawn from", {
  p <- fy_combine_pieces()
  x <- foresty_combine(p$overall, p$by_sex, p$by_smoking)
  est <- fy_est(x)

  expect_equal(est$estimate[1], fy_est(p$overall)$estimate)
  expect_equal(est$estimate[2:3], fy_est(p$by_sex)$estimate)
  expect_equal(est$conf.low[2:3], fy_est(p$by_sex)$conf.low)
  expect_equal(est$n[4:5], fy_est(p$by_smoking)$n)

  # The joint test travels with the block it belongs to, and the overall row
  # has none.
  expect_true(is.na(est$interaction_p[1]))
  expect_equal(unique(est$interaction_p[2:3]),
               fy_test(p$by_sex)$p.value)
  expect_equal(unique(est$interaction_p[4:5]),
               fy_test(p$by_smoking)$p.value)
})

test_that("an unnamed block is named for the thing it is of", {
  p <- fy_combine_pieces()
  est <- fy_est(foresty_combine(p$overall, p$by_sex, p$by_smoking))
  expect_equal(unique(est$block_label),
               c("Overall", "sex", "maternal_smoking"))

  # The modifier's label is used where one was given.
  fit <- fy_test_logistic()
  labelled <- foresty_interaction(fit, "no2", "sex",
                                  labels = c(sex = "Sex assigned at birth"))
  expect_equal(unique(fy_est(foresty_combine(labelled))$block_label),
               "Sex assigned at birth")

  # Two blocks of the same name would be drawn as one, so a repeat is numbered.
  est <- fy_est(foresty_combine(p$overall, p$overall))
  expect_equal(unique(est$block_label), c("Overall", "Overall 1"))
})

test_that("a block holding one row is that row, not a heading above one", {
  p <- fy_combine_pieces()
  x <- foresty_combine(Overall = p$overall, Sex = p$by_sex)
  labels <- fy_panel_text(x[[1]])

  # "Overall" is the row; "Sex" is the heading over the two under it. The
  # exposure's name is not repeated under "Overall".
  expect_true(all(c("Overall", "Sex", "Female", "Male") %in% labels))
  expect_false("no2" %in% labels)
  # "Overall" is not a subgroup, so the column carrying it is not headed as
  # though every row under it were one.
  expect_false("Subgroup" %in% labels)
  expect_true("Subgroup" %in% fy_panel_text(foresty_combine(Sex = p$by_sex)[[1]]))
})

test_that("the overall block is drawn as the figure's summary", {
  p <- fy_combine_pieces()
  x <- foresty_combine(Overall = p$overall, Sex = p$by_sex)

  # The overall estimate, and nothing else, is singled out.
  expect_equal(fy_est(x)$emphasis, c(TRUE, FALSE, FALSE))

  cells <- fy_label_cells(x)
  expect_equal(cells$face[cells$label == "Overall"], "bold")
  expect_true(all(cells$face[cells$label %in% c("Female", "Male")] == "plain"))

  # Squares for the subgroups and a filled diamond for the summary, each on an
  # interval of its own, so no polygon is drawn at all.
  expect_equal(sort(unname(fy_point_shapes(x))), c(22, 23))
  expect_null(fy_diamond_corners(x))
  expect_equal(nrow(fy_interval_ends(x)), 3L)

  # A layout asking for the wide summary diamond draws it from the interval
  # instead, which is a polygon and no point layer at all.
  wide <- foresty_combine(Overall = p$overall, Sex = p$by_sex,
                          layout = foresty_layout("classic",
                                                  emphasis_shape = "diamond"))
  expect_equal(sort(unname(fy_point_shapes(wide))), 22)
  corners <- fy_diamond_corners(wide)
  expect_equal(nrow(corners), 4L)

  est <- fy_est(wide)[1L, ]
  # The confidence limits at the two side vertices, the estimate at the apex.
  expect_equal(sort(corners$x), sort(c(est$conf.low, est$estimate,
                                       est$estimate, est$conf.high)))
  # Centred on its row, which is the top one of the four the figure lays out.
  expect_equal(mean(corners$y), 4)
  expect_equal(diff(range(corners$y)), 2 * fy_style("classic")$emphasis_height)

  # The interval is the diamond, so it is not drawn twice.
  expect_equal(nrow(fy_interval_ends(wide)), 2L)
})

test_that("the overall block is set apart from the subgroups", {
  p <- fy_combine_pieces()
  x <- foresty_combine(Overall = p$overall, Sex = p$by_sex)
  plain <- foresty_combine(Overall = p$overall, Sex = p$by_sex,
                           emphasize = NULL)

  # Three estimates and a heading over the subgroups. Singling the overall
  # estimate out takes no room of its own: it is the rule that sets it apart.
  expect_equal(fy_rows_of(x)$n, 4)
  expect_equal(fy_rows_of(plain)$n, 4)

  # The rule sits half a row above the heading under it, as every rule on the
  # figure does, so the summary row is the same distance from the rule above it
  # as from the one below. It is drawn whether or not the style rules between
  # subgroups.
  expect_equal(fy_rows_of(x)$separators, 3.5)
  expect_equal(fy_rows_of(x, layout = "jama")$separators, 3.5)
  expect_length(fy_rows_of(plain, layout = "jama")$separators, 0L)
})

test_that("which blocks are singled out can be said or refused", {
  p <- fy_combine_pieces()

  expect_equal(
    fy_est(foresty_combine(Overall = p$overall, Sex = p$by_sex,
                           emphasize = "Sex"))$emphasis,
    c(FALSE, TRUE, TRUE)
  )
  expect_true(all(!fy_est(foresty_combine(Overall = p$overall, Sex = p$by_sex,
                                          emphasize = NULL))$emphasis))
  expect_error(
    foresty_combine(Overall = p$overall, Sex = p$by_sex, emphasize = "Ovrall"),
    "does not carry"
  )

  # A figure of overall estimates alone has nothing to single one out from.
  expect_true(all(!fy_est(foresty_combine(A = p$overall, B = p$overall))$emphasis))
})

test_that("an overall block of several rows is not dressed as a summary", {
  d <- foresty_cohort
  # Emphasis says "this one row is what the rows below it are read against".
  # An overall block holding the levels of a categorical exposure is not one
  # summary but several estimates that happen to be unstratified, so drawing
  # every one of them in bold with a diamond would say the wrong thing.
  fit <- glm(asthma ~ urbanicity + sex + maternal_age, family = binomial,
             data = d)
  overall <- foresty_main(list(fit), exposure = "urbanicity")
  by_sex <- foresty_interaction(fit, exposure = "urbanicity",
                                interaction = "sex")
  expect_true(all(!fy_est(foresty_combine(Overall = overall,
                                          Sex = by_sex))$emphasis))

  # Asking for it by name or with TRUE still gets it, the default being a
  # judgement about what reads well rather than a refusal.
  named <- fy_est(foresty_combine(Overall = overall, Sex = by_sex,
                                  emphasize = "Overall"))
  expect_true(all(named$emphasis[named$block == "Overall"]))
  expect_true(all(!named$emphasis[named$block == "Sex"]))
  expect_true(all(fy_est(foresty_combine(Overall = overall, Sex = by_sex,
                                         emphasize = TRUE))$emphasis[1:3]))

  # A single-row overall block is still singled out, which is the usual figure.
  plain <- foresty_main(list(fy_test_logistic()), exposure = "no2")
  single <- foresty_combine(
    Overall = plain,
    Sex = foresty_interaction(fy_test_logistic(), exposure = "no2",
                              interaction = "sex")
  )
  expect_equal(fy_est(single)$emphasis, c(TRUE, FALSE, FALSE))
})

test_that("the interaction p-value is written once against each block", {
  p <- fy_combine_pieces()
  x <- foresty_combine(Overall = p$overall, Sex = p$by_sex,
                       Smoking = p$by_smoking, table = TRUE)
  estimates <- fy_row_order(fy_result(x)$estimates, "block_label")
  columns <- fy_table_columns(estimates, "Odds ratio (95% CI)")

  written <- columns[["p for\ninteraction"]]
  expect_equal(written[c(1, 3, 5)], c("", "", ""))
  expect_equal(written[2], fy_format_p(fy_test(p$by_sex)$p.value))
  expect_equal(written[4], fy_format_p(fy_test(p$by_smoking)$p.value))
})

test_that("a combined figure of main results alone carries no empty columns", {
  d <- foresty_cohort
  a <- foresty_main(list(glm(asthma ~ no2 + sex, family = binomial, data = d)),
                    exposure = "no2")
  b <- foresty_main(
    list(glm(asthma ~ black_carbon + sex, family = binomial, data = d)),
    exposure = "black_carbon"
  )
  # Two exposures, so two figures rather than two blocks of one.
  figures <- foresty_combine(NO2 = a, `Black carbon` = b)
  expect_s3_class(figures, "foresty_figures")
  expect_equal(names(figures), c("no2", "black_carbon"))

  for (figure in figures) {
    est <- fy_est(figure)
    expect_null(est$interaction_p)
    expect_null(est$modifier_level)
    expect_equal(nrow(est), 1L)
  }
  expect_equal(as.character(fy_est(figures[[1L]])$label), "no2")
  expect_equal(as.character(fy_est(figures[[2L]])$label), "black_carbon")
})

test_that("a figure per exposure, each carrying its own blocks", {
  d <- foresty_cohort
  fit_no2 <- glm(asthma ~ no2 + sex, family = binomial, data = d)
  fit_bc <- glm(asthma ~ black_carbon + sex, family = binomial, data = d)

  figures <- foresty_combine(
    Overall = foresty_main(list(fit_no2, fit_bc),
                           exposure = c(NO2 = "no2",
                                        `Black carbon` = "black_carbon")),
    Sex = foresty_interaction(fit_no2, "no2", "sex")
  )
  expect_equal(names(figures), c("NO2", "Black carbon"))

  # The overall estimate and the two levels of sex on the first; the overall
  # estimate alone on the second, which had no subgroup analysis of its own.
  expect_equal(nrow(fy_est(figures[["NO2"]])), 3L)
  expect_equal(nrow(fy_est(figures[["Black carbon"]])), 1L)
  expect_equal(fy_result(figures[["NO2"]])$blocks, c("Overall", "Sex"))
  expect_equal(fy_result(figures[["Black carbon"]])$blocks, "Overall")

  # Each figure carries the model it was drawn from and not the other one.
  expect_equal(fy_result(figures[["Black carbon"]])$exposure, "black_carbon")
  expect_length(fy_infos(figures[["Black carbon"]]), 1L)

  # The block with no subgroups in it has no column of interaction p-values.
  expect_null(fy_est(figures[["Black carbon"]])$interaction_p)
  expect_false(is.null(fy_est(figures[["NO2"]])$interaction_p))

  # Each is titled with its own exposure.
  expect_match(gsub("\n", " ", fy_title(figures[["NO2"]]), fixed = TRUE),
               "associated with NO2,", fixed = TRUE)
  expect_match(gsub("\n", " ", fy_title(figures[["Black carbon"]]),
                    fixed = TRUE),
               "associated with Black carbon,", fixed = TRUE)

  # One exposure is one figure, as it always was.
  expect_s3_class(foresty_combine(Overall = foresty_main(list(fit_no2), "no2")),
                  "foresty")
})

test_that("figures that are not comparable are refused", {
  p <- fy_combine_pieces()
  linear <- foresty_main(list(fy_test_linear()), exposure = "black_carbon")

  expect_error(foresty_combine(p$overall, linear), "different effect measures")
  expect_error(
    foresty_combine(p$overall,
                    foresty_main(list(fy_test_logistic()), "no2",
                                 ci_level = 0.9)),
    "different confidence levels"
  )
  expect_error(foresty_combine(), "at least one figure")
  expect_error(foresty_combine(p$overall, fy_test_logistic()),
               "argument 2 is of class")
})

test_that("figures of different outcomes are a figure of no one outcome", {
  d <- foresty_cohort
  asthma <- foresty_main(
    list(glm(asthma ~ no2 + sex, family = binomial, data = d)),
    exposure = "no2"
  )
  wheeze <- foresty_main(
    list(glm(wheeze ~ no2 + sex, family = binomial, data = d)),
    exposure = "no2"
  )

  # Two outcomes on one axis: naming whichever came first would head the
  # figure with an outcome half its rows are not about.
  both <- foresty_combine(Asthma = asthma, Wheeze = wheeze)
  expect_false(grepl("asthma", fy_axis_text(both), fixed = TRUE))
  expect_equal(fy_axis_text(both), "Adjusted odds ratio")

  # One outcome throughout is still named, and one named by hand is used
  # whatever the figures were of.
  same <- foresty_combine(A = asthma, B = asthma)
  expect_equal(fy_axis_text(same), "Adjusted odds ratio for asthma")
  named <- foresty_combine(Asthma = asthma, Wheeze = wheeze,
                           outcome = "any respiratory outcome")
  expect_equal(fy_axis_text(named),
               "Adjusted odds ratio for any respiratory outcome")
})

test_that("the combined figure carries its estimates onward", {
  p <- fy_combine_pieces()
  x <- foresty_combine(Overall = p$overall, Sex = p$by_sex)

  tidied <- broom::tidy(x)
  expect_equal(nrow(tidied), 3L)
  expect_equal(tidied$block, c("Overall", "Sex", "Sex"))
  expect_equal(broom::glance(x)$n_models, 2L)

  # Several models behind one figure, so summary() asks which is meant.
  expect_error(summary(x), "`model` has to say which")
  expect_s3_class(summary(x, model = 1), "summary.foresty")
})

test_that("the title names the measure, the outcome and the exposure", {
  p <- fy_combine_pieces()
  title <- gsub("\n", " ", fy_title(foresty_combine(p$overall, p$by_sex)),
                fixed = TRUE)
  # The measure is a measure of the outcome, and the exposure follows it. The
  # blocks were labelled one figure at a time, so the exposure is named once,
  # under the first label it was drawn with, rather than twice under two
  # spellings.
  expect_equal(
    title,
    paste("Adjusted odds ratio for asthma associated with no2,",
          "overall and within each subgroup")
  )
  expect_equal(lengths(regmatches(title, gregexpr("no2", title,
                                                  fixed = TRUE))), 1L)

  # Without an overall block there is nothing overall about it.
  expect_match(gsub("\n", " ", fy_title(foresty_combine(p$by_sex)), fixed = TRUE),
               "within each subgroup$")

  # An effect reported per 10 says so, since the blocks have no room to: their
  # rows are the names of the subgroups. The exposure is named once, under the
  # first label it was drawn with, rather than once per block.
  fit <- fy_test_logistic()
  per_ten <- foresty_combine(
    Overall = foresty_main(list(fit), exposure = c(NO2 = "no2"), contrast = 10),
    Sex = foresty_interaction(fit, exposure = "no2", interaction = "sex",
                              contrast = 10)
  )
  expect_equal(
    gsub("\n", " ", fy_title(per_ten), fixed = TRUE),
    paste("Adjusted odds ratio for asthma associated with NO2 (per 10),",
          "overall and within each subgroup")
  )

  # A journal puts that in the caption.
  expect_null(fy_title(foresty_combine(p$overall, p$by_sex, layout = "jama")))
})
