test_that("every style draws", {
  fit <- glm(asthma ~ urbanicity + no2 + sex, family = binomial,
             data = foresty_cohort)

  for (style in c("classic", "jama", "nejm", "lancet", "bmj", "revman")) {
    x <- foresty_interaction(fit, exposure = "urbanicity",
                             interaction = "sex", table = TRUE,
                             layout = style)
    expect_s3_class(x, "foresty")
    # Building is what catches a scale or a layer that cannot be drawn.
    # ggplot2 can issue device-specific drawing warnings on Unix, so this
    # check is about whether every style remains drawable, not warning-free.
    expect_no_error(print(x))
    expect_equal(nrow(fy_est(x)), 6L)
  }
})

test_that("a layout is a name, an object, or nothing at all", {
  expect_s3_class(foresty_layout("jama"), "foresty_layout")
  expect_equal(fy_as_layout(NULL)$style, "classic")
  expect_equal(fy_as_layout("bmj")$style, "bmj")
  expect_equal(fy_as_layout(foresty_layout("nejm"))$style, "nejm")

  expect_error(foresty_layout("vancouver"), "should be one of")
  expect_error(fy_as_layout(1), "must be the name of a style")
})

test_that("a style is changed where it needs to be and kept elsewhere", {
  x <- foresty_layout("jama", color = "#B24745", base_size = 12)

  expect_equal(x$palette$estimate, "#B24745")
  expect_equal(x$palette$interval, "#B24745")
  expect_equal(x$base_size, 12)
  # Untouched by the change, and still the journal's.
  expect_equal(x$table_side, "left")
  expect_equal(x$p_format, "jama")

  expect_equal(foresty_layout("classic", palette = c(null = "red"))$palette$null,
               "red")
  expect_error(foresty_layout("classic", palette = c(shadow = "red")),
               "does not draw")
  expect_error(foresty_layout("classic", headings = c(weight = "Weight")),
               "does not draw")
})

test_that("the color of the estimates is the one the layout was given", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2",
                    layout = foresty_layout("classic", color = "#B24745"))
  layers <- ggplot2::ggplot_build(x)$data
  points <- layers[[length(layers)]]

  expect_true(all(points$fill == "#B24745"))
  # `colour` is what ggplot2 names the column of built data, whatever the
  # package spells the setting.
  expect_true(all(points$colour == "#B24745"))
})

test_that("the rows are drawn in a color apiece when the layout asks", {
  fit <- fy_test_logistic()
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           layout = foresty_layout("classic",
                                                   color_by = "category"))
  points <- fy_marks(x)

  # One color per subgroup, from Dark2, and the interval drawn in the color
  # of the mark on it.
  expect_equal(sort(unique(points$fill)), sort(foresty_colors("Dark2")[1:2]))
  expect_equal(points$colour, points$fill)
  expect_equal(sort(unique(fy_bars(x)$colour)),
               sort(foresty_colors("Dark2")[1:2]))

  # The reference level of a categorical exposure is hollow whatever the
  # figure is colored by: it was not estimated.
  y <- foresty_main(list(fit), exposure = "maternal_smoking",
                    layout = foresty_layout("classic", color_by = "category",
                                            colors = "Set1"))
  marks <- fy_marks(y)
  expect_true("white" %in% marks$fill)
  expect_true(foresty_colors("Set1")[2] %in% marks$fill)

  # And a figure that asked for none of this is drawn exactly as it was.
  expect_true(all(fy_marks(foresty_interaction(fit, exposure = "no2",
                                               interaction = "sex"))$fill ==
                    "black"))
})

test_that("a palette is named, or the colors are given, or neither", {
  expect_length(foresty_colors("Dark2"), 8L)
  expect_equal(foresty_colors("Dark2", start = 3)[1L],
               foresty_colors("Dark2")[3L])
  # Starting late wraps round rather than running out.
  expect_equal(sort(foresty_colors("Dark2", start = 7)),
               sort(foresty_colors("Dark2")))
  expect_length(foresty_colors("Set2", n = 12), 12L)

  expect_equal(fy_layout_colors(NULL), foresty_colors("Dark2"))
  expect_equal(fy_layout_colors("Set1"), foresty_colors("Set1"))
  expect_equal(fy_layout_colors(c("red", "blue")), c("red", "blue"))
  expect_error(foresty_layout("classic", colors = "Viridis"), "palette")
  # Every color of a set of them, not only the first: one that is not a color
  # is drawn as black rather than refused, so the row it was meant for comes
  # out in the wrong color and nothing says so.
  expect_error(foresty_layout("classic", colors = c("#1B9E77", "burgundy")),
               "palette")
  expect_silent(foresty_layout("classic", colors = c("#1B9E77", "red")))
})

test_that("a setting that could not be drawn is refused where it was named", {
  # Each of these reaches grid or ggplot2 unread otherwise, and comes back as
  # a complaint about something the caller never named -- or, for a color, as
  # a figure quietly drawn in black.
  expect_error(foresty_layout("classic", palette = c(estimate = "burgundy")),
               "not a color")
  expect_error(foresty_layout("classic", color = "burgundy"), "not a color")
  expect_error(foresty_layout("classic", null_line = "banana"), "should be one")
  expect_error(foresty_layout("classic", family = 12), "family")
  expect_error(foresty_layout("classic", decimal_mark = 12), "decimal_mark")
  expect_error(foresty_layout("classic", ci_separator = 12), "ci_separator")
  expect_error(foresty_layout("classic", point_shape = 99), "point_shape")
  expect_error(foresty_layout("classic", point_shape = "square"),
               "plotting symbol")
  expect_error(foresty_layout("classic", emphasis_shape = 99),
               "emphasis_shape")
  expect_error(foresty_layout("classic", plot_width = "wide"), "plot_width")
  expect_error(foresty_layout("classic", plot_width = -1), "plot_width")
  expect_error(foresty_layout("classic", headings = c(n = NA_character_)),
               "headings")
  expect_error(foresty_layout("classic", headings = c(n = "N", n = "No.")),
               "headings")

  # What is drawable still is: a share, a width, a unit, a single character as
  # a plotting symbol, and an empty heading, which is how a column is left
  # unheaded.
  expect_silent(foresty_layout("classic", plot_width = 0.6))
  expect_silent(foresty_layout("classic", plot_width = 8))
  expect_silent(foresty_layout("classic", plot_width = grid::unit(8, "cm")))
  expect_silent(foresty_layout("classic", point_shape = "+"))
  expect_silent(foresty_layout("classic", family = "serif"))
  expect_silent(foresty_layout("classic", headings = c(n = "")))
})

test_that("the plot can be drawn on a ggplot2 theme of its own", {
  fit <- fy_test_logistic()
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           layout = foresty_layout("classic", theme = "bw"))
  expect_silent(print(x))

  # The theme reaches the plot, and the grid stays the layout's to say.
  built <- ggplot2::ggplot_build(x)
  panel <- if (inherits(x, "patchwork")) x[[length(x)]] else x
  theme <- ggplot2::ggplot_build(panel)$plot$theme
  expect_s3_class(theme$panel.background, "element_rect")
  expect_s3_class(theme$panel.grid.major.x, "element_blank")

  expect_error(foresty_layout("classic", theme = "sepia"), "ggplot2 theme")
  expect_s3_class(foresty_layout("classic", theme = ggplot2::theme_bw()),
                  "foresty_layout")
})

test_that("p-values are written the way the journal asks", {
  expect_equal(fy_format_p(c(0.0004, 0.0231, 0.005, 0.995, NA)),
               c("<0.001", "0.023", "0.005", "0.995", ""))
  # No leading zero, three places below 0.01 and two from there up, so that
  # .01 itself is written as the journal writes it.
  expect_equal(fy_format_p(c(0.0004, 0.0231, 0.005, 0.995, NA), "jama"),
               c("<.001", ".02", ".005", ">.99", ""))
  expect_equal(fy_format_p(c(0.0099, 0.01, 0.0101), "jama"),
               c(".010", ".01", ".01"))
  # The Lancet's decimal point reaches the numbers as well as the p-values.
  expect_equal(fy_format_p(0.0231, "default", "·"), "0·023")
  expect_equal(fy_format_number(1.5, 2, "·"), "1·50")
})

test_that("the interval is written in the layout's own brackets", {
  estimates <- fy_row_order(
    fy_result(foresty_main(list(fy_test_logistic()), exposure = "no2"))$estimates,
    NULL
  )

  classic <- fy_table_columns(estimates, "Odds ratio (95% CI)")
  expect_match(classic[[1L]], "^[0-9.]+ \\([0-9.]+-[0-9.]+\\)$")

  revman <- fy_table_columns(estimates, "Odds ratio (95% CI)",
                             fy_style("revman"))
  expect_match(revman[[1L]], "^[0-9.]+ \\[[0-9.]+, [0-9.]+\\]$")
})

test_that("a hyphen is not used between negative numbers", {
  negative <- data.frame(estimate = -0.4, conf.low = -0.9, conf.high = 0.1)
  positive <- data.frame(estimate = 1.4, conf.low = 0.9, conf.high = 2.1)

  expect_equal(fy_ci_separator(negative, fy_style("classic")), " to ")
  expect_equal(fy_ci_separator(positive, fy_style("classic")), "-")
  # A layout that names a separator keeps it whatever the numbers are.
  expect_equal(fy_ci_separator(negative, fy_style("revman")), ", ")
})

test_that("the headings of the columns are the layout's", {
  estimates <- fy_row_order(
    fy_result(foresty_main(list(fy_test_logistic()), exposure = "no2"))$estimates,
    NULL
  )

  jama <- fy_table_columns(estimates, "Odds ratio (95% CI)", fy_style("jama"))
  expect_true("P value" %in% names(jama))
  expect_true("No." %in% names(jama))

  # The estimate's own heading is written from the model unless it is replaced,
  # which is the way to keep a long one from taking the width of the figure.
  renamed <- fy_table_columns(
    estimates, "Adjusted odds ratio for asthma (95% CI)",
    foresty_layout("classic", headings = c(estimate = "aOR (95% CI)"))
  )
  expect_equal(names(renamed)[1L], "aOR (95% CI)")
})

test_that("an interval running past the limits is drawn with an arrow", {
  estimates <- data.frame(
    estimate = c(1.2, 3.5), conf.low = c(0.9, 1.4), conf.high = c(1.6, 8.2)
  )
  clipped <- fy_clip_estimates(estimates, c(0.5, 2))

  expect_equal(clipped$clip_high, c(FALSE, TRUE))
  expect_equal(clipped$clip_low, c(FALSE, FALSE))
  expect_equal(clipped$high, c(1.6, 2))
  # The estimate itself is outside the plot, so no mark is drawn for it.
  expect_equal(clipped$estimate_drawn, c(1.2, NA))

  # Without limits nothing is trimmed and nothing carries an arrow.
  whole <- fy_clip_estimates(estimates, NULL)
  expect_equal(whole$high, estimates$conf.high)
  expect_false(any(whole$clip_high | whole$clip_low))
})

test_that("the subgroup goes on a row of its own, or in a column of its own", {
  fit <- glm(asthma ~ urbanicity + no2 + sex, family = binomial,
             data = foresty_cohort)

  by_row <- foresty_interaction(fit, exposure = "urbanicity",
                                interaction = "sex", table = TRUE)
  by_column <- foresty_interaction(
    fit, exposure = "urbanicity", interaction = "sex", table = TRUE,
    layout = foresty_layout("classic", group_position = "column")
  )

  # The column of subgroups is a panel of its own; the rows are not.
  expect_length(by_column, length(by_row) + 1L)
})

test_that("a journal's layout leaves the title to the caption", {
  fit <- fy_test_logistic()

  classic <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  jama <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                              layout = "jama")

  expect_match(
    gsub("\n", " ", fy_title(classic), fixed = TRUE),
    "^Adjusted odds ratio for asthma associated with no2 within each level of sex"
  )
  expect_null(fy_title(jama))
  expect_null(fy_subtitle(jama))

  # The modifier still names the column of its levels, whatever the layout.
  expect_true("sex" %in% fy_panel_text(jama[[1]]))

  # One passed by hand is drawn whatever the layout.
  titled <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                                layout = "jama", title = "Effect by sex")
  expect_equal(fy_title(titled), "Effect by sex")

  # And the same holds of foresty_main().
  expect_null(fy_title(foresty_main(list(fit), exposure = "no2",
                                    layout = "jama")))
})

test_that("the plot takes what the columns leave, or a share of the figure", {
  fit <- glm(asthma ~ urbanicity + no2 + sex, family = binomial,
             data = foresty_cohort)
  args <- list(fit, exposure = "urbanicity", interaction = "sex",
               table = TRUE)

  filling <- do.call(foresty_interaction, args)
  # The plot is the one panel measured in nulls, so widening the figure widens
  # it and leaves the columns of text as they were.
  widths <- filling$patches$layout$widths
  expect_equal(as.character(grid::unitType(widths)),
               c("cm", "null", "cm"))

  shared <- do.call(foresty_interaction, c(args, list(
    layout = foresty_layout("classic", plot_width = 0.6)
  )))
  widths <- shared$patches$layout$widths
  expect_true(all(grid::unitType(widths) == "null"))
  # Three fifths of the figure to the plot, two to the columns beside it.
  values <- as.numeric(widths)
  expect_equal(values[2L] / sum(values), 0.6, tolerance = 1e-6)
})

test_that("the column headings sit on the rule under them", {
  layout <- fy_style("classic")
  headers <- c("N", "Interaction\np-value")
  geo <- fy_geometry(4, headers, layout)

  # The rows are the whole of the panel, so the figure fills the height it is
  # drawn at whatever it carries.
  expect_equal(geo$y_max, geo$top)
  expect_equal(geo$y_min, geo$bottom)

  # Two lines of heading, drawn upwards from just above the top of the panel,
  # and the band is no deeper than they are: measured in points, it is the same
  # band on a figure of four rows and on one of forty.
  expect_equal(geo$header_lines, 2L)
  expect_equal(geo$header_pt, fy_geometry(40, headers, layout)$header_pt)
  expect_gt(geo$header_pt, 2 * layout$base_size)
  expect_lt(geo$header_pt, 3 * layout$base_size)
  expect_gte(geo$pad_top_pt, geo$header_pt)

  # Nothing to head, nothing to rule off.
  bare <- fy_geometry(4, character(0), layout)
  expect_equal(bare$header_lines, 0L)
  expect_equal(bare$header_pt, 0)
})

test_that("a column is as wide as the text in it, not as its character count", {
  # A decimal point is half the width of a digit and a bracket narrower still,
  # so the interval is measured shorter than its sixteen characters.
  expect_lt(fy_em("1.03 (1.01-1.06)"), 16 * 0.55)
  expect_gt(fy_em("1.03 (1.01-1.06)"), 6)
  # The longest line is the width of the string.
  expect_equal(fy_em("Interaction\np"), fy_em("Interaction"))
  # Bold is set a little wider, and a character the table does not name is
  # measured by the columns it takes.
  expect_gt(fy_em("Events", "bold"), fy_em("Events"))
  expect_equal(fy_em("年齢"), 2)
  expect_equal(fy_em(character(0)), 0)
})

test_that("the band above the panel is drawn outside it", {
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2", table = TRUE)

  # The column headings and the rule over them are drawn above the panel, so
  # nothing is clipped to it, and the coord is left the default one it replaces
  # so that adding one by hand is not answered with a note.
  for (i in seq_along(x)) {
    coord <- ggplot2::ggplot_build(x[[i]])$plot$coordinates
    expect_equal(coord$clip, "off")
    expect_true(isTRUE(coord$default))
  }
  expect_silent(x + ggplot2::coord_cartesian(xlim = c(0.8, 2)))
})

test_that("the rows of every panel line up", {
  # The panels are separate plots, so nothing but a shared scale keeps a
  # number beside the interval it belongs to.
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2", table = TRUE)
  ranges <- lapply(seq_along(x), function(i) {
    ggplot2::ggplot_build(x[[i]])$layout$panel_params[[1L]]$y.range
  })

  for (range in ranges[-1L]) {
    expect_equal(range, ranges[[1L]])
  }
})

test_that("a figure testing an interaction drops the p-value of each row", {
  fit <- fy_test_logistic()
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  estimates <- fy_row_order(fy_result(x)$estimates, "modifier_label")

  # Two columns of p-values side by side are read for each other, so only the
  # test of the interaction is drawn.
  columns <- names(fy_choose_columns(
    fy_table_columns(estimates, "Odds ratio (95% CI)"), NULL
  ))
  expect_true("p for\ninteraction" %in% columns)
  expect_false("p-value" %in% columns)

  # Naming it brings it back.
  named <- names(fy_choose_columns(
    fy_table_columns(estimates, "Odds ratio (95% CI)"),
    c("estimate", "p", "interaction_p")
  ))
  expect_true(all(c("p-value", "p for\ninteraction") %in% named))

  # A figure with no interaction to test carries it as it always did.
  plain <- fy_row_order(fy_result(foresty_main(list(fit), "no2"))$estimates,
                        NULL)
  expect_true("p-value" %in% names(fy_choose_columns(
    fy_table_columns(plain, "Odds ratio (95% CI)"), NULL
  )))
})

test_that("the plot keeps a share of the figure when the table is wide", {
  # A column of text is as wide as what it holds, and the plot is given what
  # they leave; how much that is depends on the width the figure is drawn at,
  # which is only known when it is drawn.
  x <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "sex", table = TRUE,
                           layout = foresty_layout("classic", base_size = 16))
  built <- x$patches$layout$widths
  expect_equal(sum(grid::unitType(built) == "null"), 1L)

  text <- sum(grid::convertWidth(built[grid::unitType(built) != "null"], "cm",
                                 valueOnly = TRUE))

  # Wide enough for both, and the widths are what the panels asked for.
  wide <- fy_with_device_width(text * 4, function() fy_floor_plot_width(x))
  expect_equal(as.numeric(wide$patches$layout$widths),
               as.numeric(built))

  # Too narrow, and every width becomes a share of the figure instead, the
  # plot taking the floor and the columns of text dividing the rest.
  narrow <- fy_with_device_width(text * 1.05, function() fy_floor_plot_width(x))
  widths <- narrow$patches$layout$widths
  expect_true(all(grid::unitType(widths) == "null"))
  share <- as.numeric(widths)
  expect_equal(share[grid::unitType(built) == "null"] / sum(share), 0.25,
               tolerance = 1e-6)
})

test_that("the floor leaves alone a figure whose widths the caller fixed", {
  fit <- fy_test_logistic()
  # A share of the figure asked for outright: every width is already a share,
  # so there is nothing left over to be squeezed.
  fixed <- foresty_interaction(
    fit, exposure = "no2", interaction = "sex", table = TRUE,
    layout = foresty_layout("classic", base_size = 16, plot_width = 0.6)
  )
  expect_identical(
    fy_with_device_width(1, function() fy_floor_plot_width(fixed)), fixed
  )

  # And the floor turned off leaves the plot whatever is left, however little.
  off <- foresty_interaction(
    fit, exposure = "no2", interaction = "sex", table = TRUE,
    layout = foresty_layout("classic", base_size = 16, min_plot_width = 0)
  )
  expect_identical(
    fy_with_device_width(1, function() fy_floor_plot_width(off)), off
  )
})

test_that("the numbers under the axis are written to as few decimals as tell them apart", {
  layout <- fy_style("classic")

  # The estimates are written to two decimals, but breaks 0.2 apart need one,
  # and a label a third narrower is one that goes on fitting.
  expect_equal(fy_axis_labels(c(0.4, 0.6, 0.8, 1.0, 1.2), layout),
               c("0.4", "0.6", "0.8", "1.0", "1.2"))
  expect_equal(fy_axis_labels(c(1, 2, 3), layout), c("1", "2", "3"))

  # Where they do not, the decimals that tell them apart are kept.
  expect_equal(fy_axis_labels(c(0.95, 1, 1.05), layout),
               c("0.95", "1.00", "1.05"))
})

test_that("the null is one break rather than two", {
  # breaks_pretty() finds 1 as well, and comparing doubles with != let both
  # through to be drawn one on top of the other.
  scale <- fy_x_scale(1, NULL, fy_style("classic"))
  breaks <- scale[["breaks"]](c(0.32, 1.38))
  expect_equal(anyDuplicated(round(breaks, 8)), 0L)
  expect_true(1 %in% breaks)
})
