# The estimates a figure is drawn from when they did not come out of a model:
# an overall row read against two blocks of subgroups, which is the shape
# foresty_app() draws and the shape foresty_data() has to reproduce.
fy_data_rows <- function() {
  data.frame(
    subgroup  = c("Overall", "Female", "Male", "Under 35", "35 and over"),
    block     = c("Overall", "Sex", "Sex", "Maternal age", "Maternal age"),
    overall   = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    estimate  = c(1.24, 1.05, 1.48, 1.11, 1.39),
    conf.low  = c(1.08, 0.86, 1.21, 0.90, 1.14),
    conf.high = c(1.42, 1.28, 1.81, 1.37, 1.69),
    n         = c(4000, 2009, 1991, 1832, 2168),
    events    = c(802, 327, 475, 341, 461),
    p_int     = c(NA, 0.012, NA, 0.106, NA),
    stringsAsFactors = FALSE
  )
}

fy_data_figure <- function(...) {
  args <- utils::modifyList(
    list(data = fy_data_rows(), label = "subgroup", group = "block",
         emphasis = "overall", interaction_p = "p_int",
         measure = "OR", outcome = "asthma"),
    list(...)
  )
  do.call(foresty_data, args)
}

# The columns of the table beside the plot, as the figure would draw them.
fy_data_columns <- function(x, columns = NULL) {
  estimates <- fy_result(x)$estimates
  group <- if ("block_label" %in% names(estimates)) "block_label" else NULL
  fy_choose_columns(
    fy_table_columns(fy_row_order(estimates, group), "Odds ratio (95% CI)"),
    columns
  )
}

test_that("a figure is drawn from estimates that never met a model", {
  x <- fy_data_figure()

  expect_s3_class(x, "foresty")
  expect_s3_class(x, "patchwork")
  # The estimates are the ones handed over, in the order they were handed over.
  drawn <- as.data.frame(x)
  expect_equal(drawn$estimate, fy_data_rows()$estimate)
  expect_equal(as.character(drawn$label), fy_data_rows()$subgroup)
  expect_equal(drawn$n, fy_data_rows()$n)
})

test_that("the columns are found by name where they are not named", {
  # A frame that came out of broom::tidy(conf.int = TRUE) needs nothing said
  # about it, and neither does one calling the same things something else.
  plain <- data.frame(term = c("A", "B"), estimate = c(0.8, 1.3),
                      conf.low = c(0.6, 1.1), conf.high = c(1.1, 1.6),
                      stringsAsFactors = FALSE)
  named <- data.frame(study = c("A", "B"), est = c(0.8, 1.3),
                      lcl = c(0.6, 1.1), ucl = c(1.1, 1.6),
                      stringsAsFactors = FALSE)

  expect_equal(as.data.frame(foresty_data(plain))$estimate,
               as.data.frame(foresty_data(named))$estimate)
  expect_equal(as.character(as.data.frame(foresty_data(named))$label),
               c("A", "B"))
})

test_that("a tibble and a data.table are the data frames they are", {
  rows <- fy_data_rows()
  from_frame <- as.data.frame(fy_data_figure())

  if (requireNamespace("tibble", quietly = TRUE)) {
    x <- foresty_data(tibble::as_tibble(rows), label = "subgroup",
                      group = "block", emphasis = "overall",
                      interaction_p = "p_int", measure = "OR",
                      outcome = "asthma")
    expect_equal(as.data.frame(x)$estimate, from_frame$estimate)
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    x <- foresty_data(data.table::as.data.table(rows), label = "subgroup",
                      group = "block", emphasis = "overall",
                      interaction_p = "p_int", measure = "OR",
                      outcome = "asthma")
    expect_equal(as.data.frame(x)$estimate, from_frame$estimate)
  }
})

test_that("the blocks, the emphasis and the interaction test are drawn", {
  x <- fy_data_figure()
  estimates <- fy_result(x)$estimates

  expect_equal(levels(estimates$block_label),
               c("Overall", "Sex", "Maternal age"))
  # The overall row is the one drawn apart from the subgroups read against it.
  expect_equal(estimates$emphasis, c(TRUE, FALSE, FALSE, FALSE, FALSE))

  columns <- fy_data_columns(x)
  expect_true("p for\ninteraction" %in% names(columns))
  # One test covers the block it was taken across, so it is written once.
  expect_equal(columns[["p for\ninteraction"]],
               c("", "0.012", "", "0.106", ""))
  # N and events came with the data, and are drawn; there are no p-values for
  # the rows themselves, so there is no empty column of them taking width.
  expect_true(all(c("N", "Events") %in% names(columns)))
  expect_false("p-value" %in% names(columns))
})

test_that("a row that is a definition is drawn as one", {
  rows <- data.frame(
    level = c("Rural", "Suburban", "Urban"),
    estimate = c(1, 1.07, 1.22),
    conf.low = c(NA, 0.76, 0.86),
    conf.high = c(NA, 1.50, 1.72),
    stringsAsFactors = FALSE
  )
  # No `reference` column: a row with no interval sitting exactly on the null
  # is a reference level rather than an estimate that failed.
  x <- foresty_data(rows, label = "level", measure = "OR")
  expect_equal(fy_result(x)$estimates$reference, c(TRUE, FALSE, FALSE))
  expect_match(fy_data_columns(x)[[1L]][1L], "reference", fixed = TRUE)

  # An interval that is simply missing is not one.
  rows$estimate[1L] <- 1.3
  y <- foresty_data(rows, label = "level", measure = "OR")
  expect_equal(fy_result(y)$estimates$reference, c(FALSE, FALSE, FALSE))
})

test_that("what the estimates are is said, and decides which side of the null", {
  ratio <- fy_data_figure()
  expect_true(fy_result(ratio)$exponentiate)
  expect_equal(fy_result(ratio)$measure_label, "Odds ratio for asthma")

  difference <- fy_data_figure(measure = "MD", outcome = "FEV1 (mL)")
  expect_false(fy_result(difference)$exponentiate)
  expect_equal(fy_result(difference)$measure_label,
               "Mean difference for FEV1 (mL)")

  # A measure the package does not know is drawn under the name it was given,
  # and says for itself which side of the null it sits on.
  own <- fy_data_figure(measure = "Prevalence ratio", ratio = TRUE)
  expect_equal(fy_result(own)$measure_label, "Prevalence ratio for asthma")
  expect_true(fy_result(own)$exponentiate)
  expect_false(fy_result(fy_data_figure(measure = "Risk difference",
                                        ratio = FALSE))$exponentiate)

  # And is refused without it: a difference drawn about 1 is a wrong figure,
  # and which side of the null a name of one's own sits on cannot be read off
  # the name.
  expect_error(fy_data_figure(measure = "Standardised mean difference"),
               "`ratio` has to say")
  expect_error(fy_data_figure(measure = "Prevalence ratio"), "ratio = TRUE")
})

test_that("columns that cannot be the ones they were taken for are refused", {
  rows <- data.frame(
    level = c("A", "B"), estimate = c(1.2, 10), conf.low = c(1.0, 1.1),
    conf.high = c(1.5, 2.0), stringsAsFactors = FALSE
  )
  # The mark would sit off the end of its own bar, which reads as a finding
  # rather than as a pair of columns read for each other.
  expect_error(foresty_data(rows, label = "level"), "outside their own")
  expect_error(foresty_data(rows, label = "level"), "row 2")

  # A block every row is in, or none: a row belonging to no block is drawn
  # under a heading reading NA.
  blocked <- data.frame(
    level = c("A", "B"), estimate = c(1.2, 1.3), conf.low = c(1.0, 1.1),
    conf.high = c(1.5, 1.6), block = c("Sex", NA), stringsAsFactors = FALSE
  )
  expect_error(foresty_data(blocked, label = "level", group = "block"),
               "belong to no block")
})

test_that("a column that is not there is said to be, and named", {
  rows <- data.frame(name = "A", value = 1.2, low = 1.0, high = 1.5,
                     stringsAsFactors = FALSE)

  # Nothing looks like an estimate, so the message says what is needed, how to
  # name it, and what the data actually holds.
  expect_error(foresty_data(rows), "conf\\.low")
  expect_error(foresty_data(rows), "\"value\"")

  # A name that was given and is not there is a mistake rather than a column
  # this figure does not carry.
  expect_error(
    foresty_data(rows, estimate = "effect", conf.low = "low",
                 conf.high = "high"),
    "names a column that is not in `data`"
  )

  expect_error(
    foresty_data(rows, estimate = "value", conf.low = "high",
                 conf.high = "low"),
    "wrong way round"
  )
  expect_error(foresty_data(rows[0, ]), "no rows")
  expect_error(foresty_data(1:3), "must be the estimates as a data frame")
})

test_that("what belongs to a model is refused, and the estimates are not", {
  x <- fy_data_figure()

  # Every one of these is a property of a fitted model, and this figure was
  # drawn from a table of numbers.
  expect_error(summary(x), "foresty_data\\(\\)")
  expect_error(foresty_report(x, tempfile(fileext = ".html")),
               "no fitted model behind it")
  expect_error(stats::coef(x), "no fitted model behind it")

  # The estimates are the figure's own and are handed back as they always are.
  expect_s3_class(as.data.frame(x), "data.frame")
  expect_equal(nrow(as.data.frame(x)), 5L)
})

test_that("the figure takes a style, and the columns it is asked for", {
  x <- fy_data_figure(layout = "jama")
  expect_s3_class(x, "foresty")

  chosen <- fy_data_columns(fy_data_figure(), c("estimate", "n"))
  expect_equal(attr(chosen, "keys"), c("estimate", "n"))
})

# Every mark of a figure, rather than the last layer of them: a figure with an
# emphasised row draws its marks in two layers, one for that row and one for
# the rest, and a color given to the data has to reach both.
fy_data_marks <- function(x) {
  data <- ggplot2::ggplot_build(x)$data
  hit <- Filter(
    function(d) all(c("shape", "fill", "colour") %in% names(d)) && nrow(d) > 0L,
    data
  )
  do.call(rbind, lapply(hit, function(d) d[, c("shape", "fill", "colour")]))
}

test_that("the rows are drawn in the colors the data carries", {
  rows <- fy_data_rows()
  rows$color_group <- c("red", "#D95F02", "#D95F02", "#7570B3", "#7570B3")
  x <- fy_data_figure(data = rows, color = "color_group")

  marks <- fy_data_marks(x)
  expect_equal(sort(unique(marks$fill)),
               sort(c("#7570B3", "#D95F02", "red")))
  # The interval is drawn in the color of the mark on it.
  expect_equal(marks$colour, marks$fill)
  expect_equal(sort(unique(fy_bars(x)$colour)),
               sort(c("#7570B3", "#D95F02", "red")))

  # The wide summary diamond is the emphasised row drawn as a polygon rather
  # than as a mark, and it takes the row's color too.
  y <- fy_data_figure(data = rows, color = "color_group",
                      layout = foresty_layout("classic",
                                              emphasis_shape = "diamond"))
  built <- ggplot2::ggplot_build(y)$data
  expect_true(any(vapply(built, function(d) "red" %in% d$fill, logical(1))))
})

test_that("a column of category names is drawn in the layout's palette", {
  rows <- fy_data_rows()
  rows$color_group <- c("Overall", "Sex", "Sex", "Age", "Age")
  x <- fy_data_figure(data = rows, color = "color_group",
                      layout = foresty_layout("classic", colors = "Set1"))

  # The categories take their colors in the order they first appear, so that
  # the figure reads down the palette as it reads down the rows.
  expect_equal(sort(unique(fy_data_marks(x)$fill)),
               sort(foresty_colors("Set1")[1:3]))
})

test_that("a color the data gives a row is the color the row is drawn in", {
  rows <- fy_data_rows()
  rows$color_group <- c(NA, "#D95F02", "#D95F02", NA, NA)
  black <- fy_as_layout("classic")$palette$estimate

  # A row saying nothing is drawn in the one color the rest of the figure
  # would have been drawn in.
  x <- fy_data_figure(data = rows, color = "color_group")
  expect_equal(sort(unique(fy_data_marks(x)$fill)), sort(c(black, "#D95F02")))

  # Naming the column is what the figure is colored by, whatever `color_by`
  # would have worked out from the labels instead.
  y <- fy_data_figure(data = rows, color = "color_group",
                      layout = foresty_layout("classic",
                                              color_by = "category",
                                              colors = "Set1"))
  expect_equal(sort(unique(fy_data_marks(y)$fill)), sort(c(black, "#D95F02")))

  # And a figure that asked for none of this is drawn exactly as it was.
  expect_true(all(fy_data_marks(fy_data_figure())$fill == black))
})

test_that("a reference row stays hollow however the rows are colored", {
  cells <- data.frame(
    ecog      = c("0-1", ">=2"),
    estimate  = c(1.00, 1.94),
    conf.low  = c(NA, 1.42),
    conf.high = c(NA, 2.65),
    reference = c(TRUE, FALSE),
    color     = c("red", "blue"),
    stringsAsFactors = FALSE
  )
  marks <- fy_data_marks(
    foresty_data(cells, label = "ecog", reference = "reference",
                 color = "color", measure = "HR")
  )

  # The reference level was not estimated, so its mark is hollow and only its
  # outline carries the color it was given.
  expect_true("white" %in% marks$fill)
  expect_true("blue" %in% marks$fill)
  expect_true("red" %in% marks$colour)
})

test_that("a column of colors is colors, or names, but not both", {
  rows <- fy_data_rows()
  rows$color_group <- c("red", "Sex", "Sex", "Age", "Age")
  expect_error(fy_data_figure(data = rows, color = "color_group"),
               "colors and names of categories both")

  expect_error(fy_data_figure(color = "nosuch"), "not in `data`")
})
