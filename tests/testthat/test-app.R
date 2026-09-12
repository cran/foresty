# The app's tests drive the Shiny server function and re-run the R code it
# writes, which is the slowest file in the suite by a wide margin. Every
# test here is skipped on CRAN so that the check stays well inside the time
# CRAN allows it; they run in full locally and on every CI platform, where
# `NOT_CRAN` is set.

# The controls as the app leaves them, so that a test setting one of them is
# setting one of them and not also filling in the rest.
#
# The controls belonging to an exposure carry its name, since each exposure has
# a set of its own: `contrast_kind_no2` and `color_maternal_age`.
fy_app_defaults <- function(...) {
  utils::modifyList(
    list(exposure = "no2", modifier = character(0), overall = TRUE,
         combine = TRUE, style = "classic", table = TRUE,
         color_by = "none", palette = "Dark2", colors_hex = "",
         columns = character(0), counts_row = FALSE, test = "lrt",
         scale = "ratio",
         ci_level = 0.95, digits = 2, use_xlim = FALSE, xlim_low = 0.5,
         xlim_high = 2, title = "", no_title = FALSE, no_subtitle = FALSE,
         width = 10, height = 6, dpi = 300),
    list(...)
  )
}

fy_app_fit <- function() {
  d <- foresty_cohort
  d$male <- as.numeric(d$sex == "Male")
  glm(asthma ~ no2 + male + urbanicity + maternal_age, family = binomial,
      data = d)
}

test_that("the variables are sorted into what each of them can be asked to do", {
  skip_on_cran()
  info <- fy_model_info(fy_app_fit())
  v <- fy_app_variables(info)

  # The outcome is not one of the model's variables to draw, and every variable
  # entering as a term of its own can be the exposure.
  expect_false("asthma" %in% v$all)
  expect_equal(v$exposure, c("male", "maternal_age", "no2", "urbanicity"))

  # A flag stored as a number has levels, so it can be a modifier; a continuous
  # variable cannot, and the reason is the guard's own sentence rather than a
  # second copy of the rule kept by the app.
  expect_equal(v$modifier, c("male", "urbanicity"))
  expect_true(is.na(v$refusal[["male"]]))
  expect_match(v$refusal[["maternal_age"]], "cut\\(")

  # `contrast` and `at` are about a continuous exposure, so the app has to know
  # which of them are. A flag stored as 0/1 is a number and has a range, even
  # though it has levels as well.
  expect_equal(v$continuous, c("male", "maternal_age", "no2"))
  expect_false("urbanicity" %in% v$continuous)
})

test_that("a variable that cannot be a modifier is still offered, in its own group", {
  skip_on_cran()
  v <- fy_app_variables(fy_model_info(fy_app_fit()))
  choices <- fy_app_modifier_choices(v)

  # The overall effect is not among them: it is not a modifier but the absence
  # of one, and it has a checkbox of its own.
  expect_named(choices, c("Can be a modifier", "Cannot be a modifier"))
  expect_equal(unname(choices[["Cannot be a modifier"]]),
               c("maternal_age", "no2"))
})

test_that("the columns are offered in words rather than by argument name", {
  skip_on_cran()
  info <- fy_model_info(fy_app_fit())
  choices <- fy_app_column_choices(info)

  expect_equal(unname(choices), c("estimate", "n", "events", "interaction_p"))
  expect_true("Events" %in% names(choices))
  expect_match(names(choices)[1L], "confidence interval")
  # The p-value of each row is not offered: beside a p for the interaction it
  # is the column a reader takes for the other one.
  expect_false("p" %in% choices)
  # There is no person-time behind a logistic model, so no column of it.
  expect_false("person_time" %in% choices)
})

test_that("a narrow panel scrolls the figure rather than squeezing the plot", {
  skip_on_cran()
  # Text columns take fixed centimetres; the plot takes what they leave. If the
  # on-screen device followed the browser width, a narrow window would shrink
  # the axis between those columns. The figure is drawn at the export size and
  # the panel scrolls past it instead.
  expect_match(fy_app_css(), "fy-figure\\{[^}]*overflow-x:auto", perl = TRUE)
})

test_that("person-time is offered, and drawn, where the model carries it", {
  skip_on_cran()
  d <- foresty_cohort
  set.seed(1)
  d$follow_up <- stats::runif(nrow(d), 1, 5)
  fit <- glm(asthma ~ no2 + sex + offset(log(follow_up)), family = poisson,
             data = d)

  choices <- fy_app_column_choices(fy_model_info(fit))
  expect_true("person_time" %in% choices)
  expect_true("Person-time" %in% names(choices))

  # And the figure the app would draw with it carries the column.
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex",
                           columns = c("estimate", "n", "person_time"))
  expect_true(all(is.finite(as.data.frame(x)$person.time)))
})

test_that("the app object is built without being run", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("the code tab writes only what was moved off its default", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    # The model is named with the name the caller knows it by, so the call can
    # be pasted next to it. The difference each estimate is for is written even
    # where it is one unit, since that is a question the app asked and the
    # figure answers.
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE))
    expect_equal(
      output$code,
      paste0("foresty_interaction(my_fit, exposure = \"no2\", ",
             "interaction = \"male\", \n    contrast = 1)")
    )

    # A style is named on its own where nothing else about the layout moved,
    # rather than written as a foresty_layout() call with one argument.
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE,
                                               style = "jama"))
    expect_match(output$code, "layout = \"jama\"", fixed = TRUE)

    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE,
                                               style = "jama", digits = 3))
    expect_match(output$code, "foresty_layout(\"jama\", digits = 3)",
                 fixed = TRUE)

    # No modifier is the overall effect, which is foresty_main() and takes its
    # model in a list.
    do.call(session$setInputs, fy_app_defaults())
    expect_equal(output$code,
                 "foresty_main(list(my_fit), exposure = \"no2\", contrast = 1)")
  })
})

test_that("the difference in the exposure is said once, and one way", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)

  shiny::testServer(app, {
    # `at` and `contrast` both say which two values are compared, so the call
    # carries whichever the app is showing and never the two of them.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "at", at_from_no2 = 10,
                            at_to_no2 = 20, contrast_no2 = 10))
    expect_match(output$code, "at = c(10, 20)", fixed = TRUE)
    expect_false(grepl("contrast", output$code, fixed = TRUE))

    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "number", contrast_no2 = 10))
    expect_match(output$code, "contrast = 10", fixed = TRUE)
    expect_false(grepl("at = ", output$code, fixed = TRUE))

    # The interquartile range is the package's own, taken from the data rather
    # than worked out here and pasted in as a number, so the code says what was
    # asked for.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "iqr"))
    expect_match(output$code, "contrast = \"iqr\"", fixed = TRUE)
    expect_null(figure()$error)
    expect_match(as.character(fy_result(figure()$value)$estimates$variable_label[1L]),
                 "per IQR", fixed = TRUE)

    # One unit is written as the one unit it is, and the figure says so, since
    # a row saying nothing at all reads as though the question had not been
    # asked.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "unit", contrast_no2 = 10))
    expect_match(output$code, "contrast = 1", fixed = TRUE)
    expect_null(figure()$error)
    expect_match(as.character(fy_result(figure()$value)$estimates$variable_label[1L]),
                 "per 1", fixed = TRUE)
  })
})

test_that("each exposure is asked separately, and keeps its own answer", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    # One exposure per interquartile range and the other per decade: the two
    # controls are two controls and neither of them answers for the other.
    do.call(session$setInputs,
            fy_app_defaults(exposure = c("no2", "maternal_age"),
                            overall = TRUE, combine = FALSE,
                            contrast_kind_no2 = "iqr",
                            contrast_kind_maternal_age = "number",
                            contrast_maternal_age = 10))

    code <- output$code
    expect_match(code, "list(exposure = \"no2\", contrast = \"iqr\"",
                 fixed = TRUE)
    expect_match(code, "list(exposure = \"maternal_age\", contrast = 10",
                 fixed = TRUE)

    # And the figures are drawn that way: the one says which range it used, the
    # other says the increment.
    expect_match(as.character(as.data.frame(figures()[[1L]]$value)$label[1L]),
                 "per IQR", fixed = TRUE)
    expect_match(as.character(as.data.frame(figures()[[2L]]$value)$label[1L]),
                 "per 10", fixed = TRUE)
  })
})

test_that("an exposure can be given a color of its own", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            color_no2 = "#B24745"))
    expect_match(output$code, "color = \"#B24745\"", fixed = TRUE)
    expect_null(figure()$error)

    # Two exposures are two colors, so the layout stops being something the
    # figures share and moves into the list they are drawn from.
    do.call(session$setInputs,
            fy_app_defaults(exposure = c("no2", "maternal_age"),
                            overall = TRUE, combine = FALSE,
                            color_no2 = "#B24745",
                            color_maternal_age = "#1A476F"))
    code <- output$code
    expect_match(code, "color = \"#B24745\"", fixed = TRUE)
    expect_match(code, "color = \"#1A476F\"", fixed = TRUE)

    env <- new.env(parent = environment())
    # The script says what the interaction test came to; the test log need not.
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    expect_length(env$figures, 2L)

    # A color no menu holds is typed as a hex code instead.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            color_no2 = "hex", color_hex_no2 = "2E7D32"))
    expect_match(output$code, "color = \"#2E7D32\"", fixed = TRUE)
    expect_null(figure()$error)

    # Half a code is not a color, and is left as though nothing had been
    # chosen rather than drawn as an error. Three figures are a color, since
    # that is a hex code too.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            color_no2 = "hex", color_hex_no2 = "#2E7D3"))
    expect_false(grepl("color = ", output$code, fixed = TRUE))
    expect_null(figure()$error)

    # The style's own color is written by leaving it out.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            color_no2 = "", color_maternal_age = ""))
    expect_false(grepl("color", output$code, fixed = TRUE))
  })
})

test_that("an exposure is asked about with the values it actually takes", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  variables <- fy_app_variables(fy_model_info(fy_app_fit()))
  ends <- signif(range(foresty_cohort$no2), 4)

  expect_equal(variables$range[["no2"]], ends)
  # A categorical variable has no range, and is asked about differently.
  expect_null(variables$range[["urbanicity"]])

  html <- as.character(fy_app_exposure_ui("no2", "no2", TRUE, list(),
                                          range = variables$range[["no2"]]))
  # Which way round the comparison runs is written with an arrow, since
  # "from ... to ..." spread over a line of prose is where a reader gets it
  # wrong.
  expect_match(html, paste0("Lowest value ", fy_arrow, " Highest value"),
               fixed = TRUE)
  expect_match(html, "Reverse the direction", fixed = TRUE)
  expect_match(html, paste0(fy_arrow, " To value"), fixed = TRUE)
  expect_match(html, paste0("First quartile ", fy_arrow, " Third quartile"),
               fixed = TRUE)
  expect_match(html, paste0("runs from ", ends[1L], " to ", ends[2L]),
               fixed = TRUE)
  # The colors are offered by their place in the palette as well as by name.
  expect_match(html, "Dark2 3", fixed = TRUE)

  expect_match(
    as.character(fy_app_exposure_ui("urbanicity", "urbanicity", FALSE, list())),
    "its comparisons are its levels", fixed = TRUE
  )
})

test_that("the two ends of an exposure are a difference it can be reported for", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)
  range <- signif(range(foresty_cohort$no2), 4)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "range"))
    expect_match(output$code,
                 paste0("at = c(", range[1L], ", ", range[2L], ")"),
                 fixed = TRUE)
    expect_null(figure()$error)
    # The figure says which two values it compared, since the numbers mean
    # nothing without them.
    expect_match(
      as.character(fy_result(figure()$value)$estimates$variable_label[1L]),
      paste0(range[1L], " \u2192 ", range[2L]), fixed = TRUE
    )

    # The other way up is the same comparison reversed, which is how a
    # protective effect is drawn as one.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "range",
                            contrast_reverse_no2 = TRUE))
    expect_match(output$code,
                 paste0("at = c(", range[2L], ", ", range[1L], ")"),
                 fixed = TRUE)
    expect_null(figure()$error)

  })
})

test_that("two quantiles of an exposure are a difference it can be reported for", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)
  quartiles <- signif(unname(stats::quantile(foresty_cohort$no2,
                                             probs = c(0.25, 0.75))), 4)
  deciles <- signif(unname(stats::quantile(foresty_cohort$no2,
                                           probs = c(0.1, 0.9))), 4)

  shiny::testServer(app, {
    # The quartiles to begin with, and as `at` rather than as `contrast`: these
    # are two values of the exposure, not a width to report an effect per.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "quantile",
                            quantile_low_no2 = 0.25, quantile_high_no2 = 0.75))
    expect_match(output$code,
                 paste0("at = c(", quartiles[1L], ", ", quartiles[2L], ")"),
                 fixed = TRUE)
    expect_false(grepl("contrast", output$code, fixed = TRUE))
    expect_null(figure()$error)
    expect_match(
      as.character(fy_result(figure()$value)$estimates$variable_label[1L]),
      paste0(quartiles[1L], " \u2192 ", quartiles[2L]), fixed = TRUE
    )

    # And any other pair, since which two quantiles are compared is the reader's
    # question rather than the package's.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "quantile",
                            quantile_low_no2 = 0.1, quantile_high_no2 = 0.9))
    expect_match(output$code,
                 paste0("at = c(", deciles[1L], ", ", deciles[2L], ")"),
                 fixed = TRUE)
    expect_null(figure()$error)

    # A probability that is not one is left to fall back on the plain one unit
    # rather than drawn as an estimate of nothing.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "quantile",
                            quantile_low_no2 = 0.25, quantile_high_no2 = 4))
    expect_false(grepl("at = ", output$code, fixed = TRUE))
    expect_null(figure()$error)

    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "quantile",
                            quantile_low_no2 = 0.25, quantile_high_no2 = 0.75,
                            contrast_reverse_no2 = TRUE))
    expect_match(output$code,
                 paste0("at = c(", quartiles[2L], ", ", quartiles[1L], ")"),
                 fixed = TRUE)
  })
})

test_that("a splined exposure can be reported between two quantiles", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("splines")
  d <- foresty_cohort
  d$male <- as.numeric(d$sex == "Male")
  my_fit <- glm(asthma ~ splines::ns(no2, 3) + male + maternal_age,
                family = binomial, data = d)
  app <- foresty_app(my_fit, launch = FALSE)
  quartiles <- signif(unname(stats::quantile(d$no2, probs = c(0.25, 0.75))), 4)

  shiny::testServer(app, {
    # An interquartile range is a step along a slope, and a splined exposure has
    # none: it is refused. Two quantiles of it are two values, so the same two
    # numbers can be compared on the curve the model has.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "iqr"))
    expect_match(figure()$error, "not a single number")

    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            contrast_kind_no2 = "quantile",
                            quantile_low_no2 = 0.25, quantile_high_no2 = 0.75))
    expect_null(figure()$error)
    expect_match(output$code,
                 paste0("at = c(", quartiles[1L], ", ", quartiles[2L], ")"),
                 fixed = TRUE)
  })
})

test_that("a splined exposure starts on a question it can answer", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("splines")
  d <- foresty_cohort
  d$male <- as.numeric(d$sex == "Male")
  my_fit <- glm(asthma ~ splines::ns(no2, 3) + male + maternal_age,
                family = binomial, data = d)
  info <- fy_model_info(my_fit)
  v <- fy_app_variables(info)

  # The exposure is known to enter through more than one coefficient, and the
  # one that does not is not confused with it.
  expect_equal(v$splined, "no2")
  expect_true("maternal_age" %in% v$continuous)
  expect_false("maternal_age" %in% v$splined)

  # So the menu offers only the comparisons it can be asked for: naming an
  # increment of it is not one of them.
  ui <- fy_app_exposure_ui("no2", "no2", continuous = TRUE, input = list(),
                           range = c(1, 40), splined = TRUE)
  rendered <- as.character(shiny::tagList(ui))
  expect_false(grepl("One unit increase", rendered, fixed = TRUE))
  expect_false(grepl("One interquartile range increase", rendered, fixed = TRUE))
  expect_true(grepl("Two values I name", rendered, fixed = TRUE))
  expect_true(grepl("more than one coefficient", rendered, fixed = TRUE))

  # And nothing has to be clicked for the figure to be drawn: the default is
  # the two quartiles rather than the one unit a plain exposure starts on.
  quartiles <- signif(unname(stats::quantile(d$no2, probs = c(0.25, 0.75))), 4)
  app <- foresty_app(my_fit, launch = FALSE)
  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE))
    expect_null(figure()$error)
    expect_match(output$code,
                 paste0("at = c(", quartiles[1L], ", ", quartiles[2L], ")"),
                 fixed = TRUE)
  })
})

test_that("the app draws a multinomial fit and can move its outcome reference", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("nnet")
  my_fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                           data = foresty_cohort, trace = FALSE)
  v <- fy_app_variables(fy_model_info(my_fit))
  expect_equal(v$outcome_levels, c("None", "Transient", "Persistent"))
  expect_equal(v$outcome_reference, "None")

  app <- foresty_app(my_fit, launch = FALSE)
  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "sex", overall = FALSE))
    expect_null(figure()$error)
    # Left on the level the model was fitted against, the call says nothing.
    expect_false(grepl("outcome_reference", output$code, fixed = TRUE))
    est <- as.data.frame(figure()$value)
    expect_equal(est$outcome_reference, rep("None", 4L))

    session$setInputs(outcome_reference = "Transient")
    expect_null(figure()$error)
    expect_match(output$code, "outcome_reference = \"Transient\"", fixed = TRUE)
    est <- as.data.frame(figure()$value)
    expect_equal(unique(est$outcome_level), c("None", "Persistent"))

    # The level everything is read against, drawn as a row of its own.
    session$setInputs(outcome_reference_row = TRUE)
    expect_null(figure()$error)
    expect_match(output$code, "outcome_reference_row = TRUE", fixed = TRUE)
    est <- as.data.frame(figure()$value)
    expect_equal(unique(est$outcome_level), c("Transient", "None", "Persistent"))
    expect_equal(est$estimate[est$reference], c(1, 1))
  })
})

test_that("the outcome reference row is not written for a one-equation fit", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  # The box belongs to a model that has such a level; one that has not is never
  # shown it, and a stale input cannot write the argument for it.
  app <- foresty_app(fy_app_fit(), launch = FALSE)
  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            outcome_reference_row = TRUE))
    expect_null(figure()$error)
    expect_false(grepl("outcome_reference", output$code, fixed = TRUE))
  })
})

test_that("the rows can be colored by their category rather than the exposure", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            color_by = "category", palette = "Dark2"))
    expect_match(output$code, "color_by = \"category\"", fixed = TRUE)
    expect_match(output$code, "colors = \"Dark2\"", fixed = TRUE)
    expect_null(figure()$error)
    expect_equal(sort(unique(fy_marks(figure()$value)$fill)),
                 sort(foresty_colors("Dark2")[1:2]))

    # Colors of your own are typed as hex codes, which is how a figure is
    # drawn to match colors no palette holds. They win over the palette, since
    # having typed them is the answer to the question the menu asks.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            color_by = "category", palette = "Dark2",
                            colors_hex = "#B24745, 1A476F"))
    expect_match(output$code, "colors = c(\"#B24745\", \"#1A476F\")",
                 fixed = TRUE)
    expect_equal(sort(unique(fy_marks(figure()$value)$fill)),
                 sort(c("#B24745", "#1A476F")))

    # A code half typed is not a color yet, so the palette is drawn rather
    # than an error.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            color_by = "category", palette = "Dark2",
                            colors_hex = "#B247"))
    expect_match(output$code, "colors = \"Dark2\"", fixed = TRUE)
    expect_null(figure()$error)

    # A figure whose rows carry colors of their own has no one color to be
    # drawn in, so the exposure's own is left out rather than written and
    # overridden.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            color_by = "row", color_no2 = "#B24745"))
    expect_false(grepl("color = ", output$code, fixed = TRUE))
    expect_match(output$code, "color_by = \"row\"", fixed = TRUE)
  })
})

test_that("the subtitle naming the exposure can be turned off on its own", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)

  shiny::testServer(app, {
    # Several figures are told apart by the exposure under each title, which is
    # the subtitle.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = TRUE))
    expect_match(output$code, "subtitle = \"Exposure: no2", fixed = TRUE)

    # A title of its own says it already on a figure whose rows are all of one
    # exposure, so the line can go while the title stays.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = TRUE, no_subtitle = TRUE))
    expect_false(grepl("subtitle", output$code, fixed = TRUE))
    expect_false(grepl("title = NA", output$code, fixed = TRUE))
    expect_null(figure()$error)
  })
})

test_that("the scale is a choice between the two things it is a choice between", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE, scale = "log"))
    expect_match(output$code, "exponentiate = FALSE", fixed = TRUE)
    expect_false(isTRUE(fy_result(figure()$value)$exponentiate))

    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE,
                                               scale = "ratio"))
    expect_false(grepl("exponentiate", output$code, fixed = TRUE))
    expect_true(isTRUE(fy_result(figure()$value)$exponentiate))
  })
})

test_that("only the two tests the package reports are offered", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE, test = "wald"))
    expect_match(output$code, "test = \"wald\"", fixed = TRUE)
    expect_equal(fy_result(figure()$value)$interaction_test$test,
                 "Wald chi-square")

    # The likelihood ratio test is the package's default, so it is written by
    # leaving it out.
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE, test = "lrt"))
    expect_false(grepl("test = ", output$code, fixed = TRUE))
  })
})

test_that("the code shown is the code that drew the figure", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            scale = "log", ci_level = 0.9,
                            contrast_kind_no2 = "number", contrast_no2 = 10))
    drawn <- figure()
    expect_null(drawn$error)

    # Evaluating what the tab shows has to reproduce what the tab is beside,
    # which is what stops the two of them drifting apart.
    pasted <- eval(parse(text = output$code))
    expect_equal(as.data.frame(pasted)$estimate,
                 as.data.frame(drawn$value)$estimate, tolerance = 1e-10)
    expect_equal(as.data.frame(pasted)$conf.low,
                 as.data.frame(drawn$value)$conf.low, tolerance = 1e-10)
  })
})

test_that("a modifier the guards refuse shows the remedy in place of a figure", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs, fy_app_defaults(modifier = "maternal_age",
                                               overall = FALSE))
    state <- figure()
    expect_null(state$value)
    expect_match(state$error, "cut\\(")

    # And the app recovers: choosing one that works draws it.
    do.call(session$setInputs, fy_app_defaults(modifier = "urbanicity",
                                               overall = FALSE))
    state <- figure()
    expect_null(state$error)
    expect_equal(nrow(as.data.frame(state$value)), 3L)
  })
})

test_that("the menus come to every exposure against every modifier", {
  skip_on_cran()
  # Two exposures and two modifiers, with the overall effect asked for beside
  # them rather than instead of them, are six figures.
  pairs <- fy_app_pairs(list(exposure = c("no2", "male"),
                             modifier = "urbanicity", overall = TRUE))
  expect_length(pairs, 4L)
  expect_equal(vapply(pairs, fy_app_pair_label, character(1)),
               c("no2, overall", "no2 by urbanicity",
                 "male, overall", "male by urbanicity"))
  # No modifier is NULL rather than "", everywhere but in the menu.
  expect_null(pairs[[1L]]$modifier)

  # Without the overall effect there is one figure per pair.
  expect_equal(
    vapply(fy_app_pairs(list(exposure = "no2", modifier = c("male", "urbanicity"),
                             overall = FALSE)),
           fy_app_pair_label, character(1)),
    c("no2 by male", "no2 by urbanicity")
  )

  # And with no modifier chosen, the overall effect is what there is to draw,
  # whether or not the box was ticked.
  expect_equal(fy_app_pairs(list(exposure = "no2", overall = FALSE)),
               list(list(exposure = "no2", modifier = NULL)))
  # Nothing is drawn until an exposure is chosen.
  expect_length(fy_app_pairs(list(exposure = NULL, modifier = "male")), 0L)
})

test_that("the text size chosen is written into the layout", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs, fy_app_defaults(base_size = 18))
    expect_match(output$code, "foresty_layout(\"classic\", ", fixed = TRUE)
    expect_match(output$code, "base_size = 18", fixed = TRUE)

    # And the figure beside the tab is drawn from that call, text and all.
    expect_null(figure()$error)
  })
})

test_that("several pairs are written as a loop over the pairs", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = TRUE))

    code <- output$code
    expect_match(code, "specs <- list(", fixed = TRUE)
    expect_match(code, "list(exposure = \"no2\", interaction = \"male\"",
                 fixed = TRUE)
    expect_match(code, "figures <- lapply(specs, function(spec) {", fixed = TRUE)
    # The pairs differ in what varies between the figures and in nothing else,
    # so the two calls are written once apiece rather than once per pair.
    expect_match(code, "do.call(foresty_main, c(list(list(my_fit)), spec",
                 fixed = TRUE)
    expect_match(code, "do.call(foresty_interaction, c(list(my_fit), spec",
                 fixed = TRUE)
    expect_match(code, "combined <- do.call(foresty_combine, ", fixed = TRUE)

    # Each figure says which exposure it is of, under its own title, since
    # there is more than one of them to tell apart.
    expect_match(code, "subtitle = \"Exposure: no2 (per 1)\"", fixed = TRUE)

    # What the tab shows has to reproduce what the tab is beside, which is what
    # stops the two of them drifting apart.
    env <- new.env(parent = environment())
    # The script says what the interaction test came to; the test log need not.
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    expect_named(env$figures, names(figures()))
    expect_s3_class(env$combined, "foresty")
    expect_equal(as.data.frame(env$combined)$estimate,
                 as.data.frame(combined()$value)$estimate, tolerance = 1e-10)

    # The overall estimate and the two subgroups, in one figure.
    expect_equal(nrow(as.data.frame(combined()$value)), 3L)
    expect_length(display(), 1L)
    expect_named(display(), "Combined")

    # Unless the combining is turned off, when both are drawn as they are.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = TRUE, combine = FALSE))
    expect_null(combined())
    expect_named(display(), c("no2, overall", "no2 by male"))
    expect_false(grepl("foresty_combine", output$code, fixed = TRUE))
  })
})

test_that("the loop carries the style, and the combination carries it too", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = c("male", "urbanicity"),
                            overall = FALSE, style = "jama", base_size = 14,
                            contrast_kind_no2 = "iqr"))

    code <- output$code
    expect_false(grepl("is.null(spec$interaction)", code, fixed = TRUE))
    expect_match(code, "contrast = \"iqr\"", fixed = TRUE)
    expect_match(code, "do.call(foresty_combine, c(", fixed = TRUE)
    expect_match(code, "unname(figures),", fixed = TRUE)
    expect_match(code, "layout = foresty_layout(\"jama\", base_size = 14)",
                 fixed = TRUE)

    env <- new.env(parent = environment())
    # The script says what the interaction test came to; the test log need not.
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    expect_equal(as.data.frame(env$combined)$estimate,
                 as.data.frame(combined()$value)$estimate, tolerance = 1e-10)
  })
})

test_that("figures of different exposures stay apart when combined", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(exposure = c("no2", "maternal_age"),
                            modifier = "male", overall = FALSE))
    # foresty_combine() returns a figure per exposure, and the app draws them
    # one under the other rather than trying to make one of them.
    expect_length(display(), 2L)
    expect_null(combined()$error)
  })
})

test_that("a pair the guards refuse is left out of the combination", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", overall = FALSE,
                            modifier = c("male", "maternal_age")))
    states <- figures()
    expect_null(states[["no2 by male"]]$error)
    expect_match(states[["no2 by maternal_age"]]$error, "cut\\(")

    # One figure was drawn, so there is nothing to combine it with, and it is
    # drawn as it is.
    expect_null(combined())
    expect_named(display(), "no2 by male")
  })
})

test_that("the Models tab writes out how each figure was arrived at", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = TRUE, test = "wald"))
    text <- output$models

    # The model as it was fitted, and the model each figure came out of, each
    # under a heading saying what the figure is of.
    expect_match(text, "asthma ~ no2 + male + urbanicity + maternal_age",
                 fixed = TRUE)
    expect_match(text, "Outcome:         asthma", fixed = TRUE)
    expect_match(text, "Effect modifier: male", fixed = TRUE)
    expect_match(text, "Effect modifier: none -- the overall effect",
                 fixed = TRUE)
    expect_match(text, "my_fit_int <- update(my_fit, . ~ . + no2:male)",
                 fixed = TRUE)
    # The estimates and the test as the code they amount to, named for this
    # model and these variables. It is the call foresty makes, not a shorter
    # one that would come to something else: the default method, the degrees
    # of freedom and the test it worked out.
    expect_match(text, "car::linearHypothesis.default(my_fit_int", fixed = TRUE)
    expect_match(text, "error.df = Inf, test = \"Chisq\"", fixed = TRUE)
    expect_match(text, "singular.ok = TRUE", fixed = TRUE)
    expect_match(text, "rows$male <- level", fixed = TRUE)
    # The levels are put back as the column holds them: a flag coded 0 and 1 is
    # a number, and quoting it would build a design matrix of a character
    # column.
    expect_match(text, "lapply(c(0, 1), within)", fixed = TRUE)
    expect_match(text, "Wald chi-square", fixed = TRUE)
    # The design matrix is built the way the fitting function builds its own,
    # levels and contrast coding included.
    expect_match(text, "xlev = my_fit_int$xlevels", fixed = TRUE)
    expect_match(text, "contrasts.arg = my_fit_int$contrasts", fixed = TRUE)
    # The overall figure is drawn from the model as it stands, and says so.
    expect_match(text, "THE MODEL YOU FITTED -- no interaction term",
                 fixed = TRUE)

    # Each section opens with what the model is, in the words a methods section
    # uses rather than the classes of the object it is held in, and carries the
    # formula of the model that section's figure came out of -- which is the
    # refitted one where the section is a subgroup analysis.
    expect_match(text, "Logistic regression model", fixed = TRUE)
    expect_false(grepl("Class:", text, fixed = TRUE))
    expect_match(text, "Formula:         asthma ~ no2 + male + urbanicity + maternal_age",
                 fixed = TRUE)
    expect_match(text, "Formula:         asthma ~ no2 + male + urbanicity + maternal_age + no2:male",
                 fixed = TRUE)

    # The likelihood ratio test is a different piece of code, and the tab
    # writes the one that was taken.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = FALSE, test = "lrt"))
    expect_match(output$models, "2 * (as.numeric(logLik(my_fit_int))",
                 fixed = TRUE)
    expect_match(output$models, "anova(reduced, my_fit_int, test = \"LRT\")",
                 fixed = TRUE)
  })
})

test_that("what the Models tab writes out is what foresty ran", {
  skip_on_cran()
  # Fitted here rather than through the fixture, because the code the tab
  # writes names the data frame the model's own call names -- which is what
  # makes it code a reader can paste beside the model -- and pasting it needs
  # that data frame to be there.
  d <- foresty_cohort
  d$male <- as.numeric(d$sex == "Male")
  my_fit <- glm(asthma ~ no2 + male + urbanicity + maternal_age,
                family = binomial, data = d)
  info <- fy_model_info(my_fit)
  compared <- list(from = "11.9", to = "21.9", said = "")

  # Pasting the code beside the model has to reproduce the number on the
  # figure, or the tab is a description of the analysis rather than the
  # analysis.
  code <- paste(fy_app_lincom_code(info, "my_fit", "no2", compared),
                collapse = "\n")
  drawn <- foresty_main(list(my_fit), exposure = "no2", at = c(11.9, 21.9))
  pasted <- eval(parse(text = code))
  expect_equal(exp(as.vector(attr(pasted, "value"))),
               as.data.frame(drawn)$estimate, tolerance = 1e-10)

  # The subgroups, from the model carrying the interaction.
  my_fit_int <- update(my_fit, . ~ . + no2:male)
  info_int <- fy_model_info(my_fit_int)
  subgroups <- eval(parse(text = paste(
    fy_app_lincom_code(info_int, "my_fit_int", "no2", compared,
                       modifier = "male", levels = c("0", "1")),
    collapse = "\n"
  )))
  by_male <- foresty_interaction(my_fit, exposure = "no2", interaction = "male",
                                 at = c(11.9, 21.9))
  expect_equal(
    unname(exp(vapply(subgroups,
                      function(x) as.vector(attr(x, "value")), numeric(1)))),
    as.data.frame(by_male)$estimate, tolerance = 1e-10
  )

  # And the joint test beside them.
  columns <- fy_modifier_interaction_columns(info_int, "no2", "male")
  joint <- eval(parse(text = paste(
    fy_app_joint_code(info_int, "my_fit_int", columns), collapse = "\n"
  )))
  wald <- foresty_interaction(my_fit, exposure = "no2", interaction = "male",
                              at = c(11.9, 21.9), test = "wald")
  expect_equal(joint[["Pr(>Chisq)"]][2L],
               fy_result(wald)$interaction_test$p.value, tolerance = 1e-10)

  # A gaussian model is tested with F on its residual degrees of freedom, and
  # the code says so rather than saying Chisq to everything.
  linear <- lm(no2 ~ maternal_age + sex, data = foresty_cohort)
  text <- paste(fy_app_lincom_code(fy_model_info(linear), "linear",
                                   "maternal_age",
                                   list(from = "29", to = "39", said = "")),
                collapse = "\n")
  expect_match(text, paste0("error.df = ", stats::df.residual(linear),
                            ", test = \"F\""), fixed = TRUE)
})

test_that("the summary tab shows the model fitted and the model with the term", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  my_fit <- fy_app_fit()
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = TRUE))
    text <- output$model

    expect_match(text, "THE MODEL YOU FITTED", fixed = TRUE)
    expect_match(text, "WITH THE no2 BY male INTERACTION TERM", fixed = TRUE)
    # A coefficient table says which terms are in the model and nothing about
    # what the figure beside it is of, so the heading says it.
    expect_match(text, "Outcome:         asthma", fixed = TRUE)
    expect_match(text, "Exposure:        no2", fixed = TRUE)
    expect_match(text, "Effect modifier: male", fixed = TRUE)
    # The model you fitted carries no interaction term, which is the whole of
    # what makes it the model the overall effect comes from.
    expect_match(text, "Effect modifier: none -- this model carries no",
                 fixed = TRUE)
    # Both summaries are there, so the coefficient of the interaction can be
    # read beside the model that does not carry it.
    expect_match(text, "no2:male", fixed = TRUE)
    expect_equal(length(gregexpr("Coefficients:", text, fixed = TRUE)[[1L]]), 2L)
  })
})

test_that("a model that already carries the interaction is not given it twice", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  d <- foresty_cohort
  d$male <- as.numeric(d$sex == "Male")
  my_fit <- glm(asthma ~ no2 * male + maternal_age, family = binomial, data = d)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE))
    expect_match(output$models, "already", fixed = TRUE)
  })
})

test_that("a download is named for what it is of", {
  skip_on_cran()
  one <- list(list(exposure = "no2", modifier = "male"))
  expect_equal(fy_app_slug(one), "no2_male")
  expect_equal(fy_app_slug(list(list(exposure = "no2", modifier = NULL))),
               "no2_overall")
  expect_equal(
    fy_app_slug(c(one, list(list(exposure = "maternal_age", modifier = NULL)))),
    "no2_maternal_age_figures"
  )

  # One figure is one file; several that were not combined are several files,
  # which come down zipped.
  expect_equal(fy_app_file_name(one, list(1), "png"), "no2_male.png")
  expect_equal(fy_app_file_name(one, list(1, 2), "png"), "no2_male_png.zip")
  expect_equal(fy_app_file_name(one, list(1, 2), "html"), "no2_male_html.zip")
})

test_that("the figures export as a named list to go on with", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  app <- foresty_app(fy_app_fit(), launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = TRUE))

    drawn <- Filter(function(s) !is.null(s$value), figures())
    objects <- stats::setNames(lapply(drawn, function(s) s$value), names(drawn))
    objects$Combined <- combined()$value

    expect_named(objects, c("no2, overall", "no2 by male", "Combined"))
    expect_s3_class(objects[["no2 by male"]], "foresty")
    # The estimates travel with the figures, which is what makes them worth
    # exporting rather than an image of them.
    expect_equal(nrow(as.data.frame(objects[["no2 by male"]])), 2L)

    file <- tempfile(fileext = ".rds")
    on.exit(unlink(file), add = TRUE)
    saveRDS(objects, file)
    expect_named(readRDS(file), names(objects))
  })
})

test_that("the figure is written as PNG and as SVG", {
  skip_on_cran()
  fit <- fy_app_fit()
  figures <- list("no2 by male" = foresty_interaction(fit, exposure = "no2",
                                                      interaction = "male"))
  input <- list(width = 6, height = 4, dpi = 96)

  png <- tempfile(fileext = ".png")
  on.exit(unlink(png), add = TRUE)
  fy_app_save(figures, png, "png", input)
  # A PNG begins with the bytes that say so.
  expect_equal(readBin(png, "raw", 4L), as.raw(c(0x89, 0x50, 0x4e, 0x47)))

  skip_if_not(requireNamespace("svglite", quietly = TRUE) ||
                isTRUE(capabilities("cairo")), "no SVG device")
  svg <- tempfile(fileext = ".svg")
  on.exit(unlink(svg), add = TRUE)
  fy_app_save(figures, svg, "svg", input)
  expect_match(paste(readLines(svg, n = 3L, warn = FALSE), collapse = " "),
               "<svg")
})

test_that("figures that were not combined come down as a file apiece", {
  skip_on_cran()
  skip_if_not_installed("zip")
  fit <- fy_app_fit()
  figures <- list(
    "no2, overall" = foresty_main(list(fit), exposure = "no2"),
    "no2 by male" = foresty_interaction(fit, exposure = "no2",
                                        interaction = "male")
  )
  input <- list(width = 6, height = 4, dpi = 96)

  file <- tempfile(fileext = ".zip")
  on.exit(unlink(file), add = TRUE)
  fy_app_save_all(figures, file, "png", input)

  # Named for the pair each of them is of, so that a directory of them can be
  # read without opening them.
  inside <- zip::zip_list(file)$filename
  expect_equal(sort(inside), c("no2_by_male.png", "no2_overall.png"))
})

test_that("a report is written for each model rather than for the first", {
  skip_on_cran()
  skip_if_not_installed("zip")
  skip_if_not_installed("gt")
  fit <- fy_app_fit()
  figures <- list(
    "no2, overall" = foresty_main(list(fit), exposure = "no2"),
    "no2 by male" = foresty_interaction(fit, exposure = "no2",
                                        interaction = "male")
  )

  file <- tempfile(fileext = ".zip")
  on.exit(unlink(file), add = TRUE)
  fy_app_write_reports(figures, file)
  expect_equal(sort(zip::zip_list(file)$filename),
               c("no2_by_male.html", "no2_overall.html"))

  # One figure is one page, under the name the browser was promised rather than
  # under the one foresty_report() would have chosen.
  single <- tempfile()
  on.exit(unlink(single), add = TRUE)
  fy_app_write_reports(figures[1], single)
  expect_true(file.exists(single))
  expect_match(paste(readLines(single, n = 5L, warn = FALSE), collapse = " "),
               "<html")
})

test_that("what foresty says while it works is shown, not swallowed", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  # The model already carries the interaction, which foresty reports with a
  # message; the app has to pass that on, since it changes how the figure is
  # read.
  d <- foresty_cohort
  d$male <- as.numeric(d$sex == "Male")
  my_fit <- glm(asthma ~ no2 * male + maternal_age, family = binomial, data = d)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE))
    state <- figure()
    expect_null(state$error)
    expect_true(any(grepl("already contains", state$notes)))
  })
})

# What things are called, and a model too slow to redraw ----------------------

test_that("the outcome, the axis and the person-time unit reach the call", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  d <- foresty_cohort
  set.seed(1)
  d$follow_up <- stats::runif(nrow(d), 1, 5)
  d$male <- as.numeric(d$sex == "Male")
  rate <- glm(asthma ~ no2 + male + offset(log(follow_up)), family = poisson,
              data = d)
  app <- foresty_app(rate, launch = FALSE)

  shiny::testServer(app, {
    # Nothing is written where the boxes were left alone.
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE))
    expect_false(grepl("outcome", output$code, fixed = TRUE))
    expect_false(grepl("xlab", output$code, fixed = TRUE))
    expect_false(grepl("person_time", output$code, fixed = TRUE))

    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            outcome = "incident asthma",
                            xlab = "Rate ratio (95% CI)",
                            person_time = 1000,
                            label_no2 = "NO2, ug/m3"))
    code <- output$code
    expect_match(code, "outcome = \"incident asthma\"", fixed = TRUE)
    expect_match(code, "xlab = \"Rate ratio (95% CI)\"", fixed = TRUE)
    expect_match(code, "person_time = 1000", fixed = TRUE)
    # The exposure is renamed where it is named, which is one thing to paste
    # into a script rather than two.
    expect_match(code, "`NO2, ug/m3` = \"no2\"", fixed = TRUE)

    # And the figure is drawn with them.
    expect_null(figure()$error)
    result <- fy_result(figure()$value)
    expect_equal(result$measure_label, "Incidence rate ratio for incident asthma")
    expect_equal(result$person_time$unit, 1000)
    expect_match(as.character(result$estimates$variable_label[1L]),
                 "NO2, ug/m3", fixed = TRUE)
  })
})

test_that("the app is opened in a browser unless it is told not to be", {
  skip_on_cran()
  # A point-and-click tool that starts by printing a URL has not started. The
  # argument is named rather than left to `...` so that a caller who wants the
  # other behaviour -- a remote session, a scripted screenshot -- can say so.
  expect_equal(formals(foresty_app)$launch.browser, TRUE)
})

# The model the plain script is written beside, with its data under a name the
# script can find. The generated code names the data frame the model's own call
# names, which is the point of it: it is meant to be pasted into the script
# that fitted the model rather than into an empty session.
fy_plain_session <- function(fit, data) {
  env <- new.env(parent = globalenv())
  assign("my_fit", fit, envir = env)
  assign("cohort", data, envir = env)
  env
}

# The script's prose as one line: its comments are wrapped to the width the tab
# is read at, so a sentence tested for here would otherwise have to be tested
# for in whichever two halves the wrapping happened to leave it in.
fy_plain_prose <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
  gsub("\\s+", " ", paste(sub("^\\s*#\\s?", "", lines), collapse = " "))
}

fy_plain_fit <- function(cohort) {
  glm(asthma ~ no2 + male + urbanicity + maternal_age, family = binomial,
      data = cohort)
}

test_that("the R code tab writes where the estimates came from", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  cohort <- foresty_cohort
  cohort$male <- as.numeric(cohort$sex == "Male")
  my_fit <- fy_plain_fit(cohort)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs, fy_app_defaults(modifier = "male",
                                               overall = FALSE))
    code <- output$code_plain

    # It is a script about this model and these variables, and nothing in it
    # comes from the package it is the alternative to.
    expect_match(code, "update(my_fit, . ~ . + no2:male)", fixed = TRUE)
    expect_match(code, "at <- cohort[", fixed = TRUE)
    # It calculates rather than draws: the estimates come out of car, and no
    # part of the figure is rebuilt here.
    expect_match(code, "car::linearHypothesis.default(", fixed = TRUE)
    expect_false(grepl("ggplot(", code, fixed = TRUE))
    # The test it took is named and written out; the other one is neither.
    expect_match(fy_plain_prose(code),
                 "the likelihood ratio test of the interaction", fixed = TRUE)
    expect_false(grepl("wald_p <- function", code, fixed = TRUE))
    # Nothing it runs comes from the package. The comments above the code do
    # name it, since saying what is not being reproduced means naming the
    # function that would have.
    runs <- grep("^\\s*#", strsplit(code, "\n")[[1L]], value = TRUE,
                 invert = TRUE)
    expect_false(any(grepl("foresty", runs, fixed = TRUE)))
    # Every line fits the width the tab is read at, since a line that does not
    # is read with a scrollbar or not at all.
    expect_true(all(nchar(strsplit(code, "\n", fixed = TRUE)[[1L]]) <= 78L))

    # And it runs, beside the model, and comes to the numbers on the figure.
    env <- fy_plain_session(my_fit, cohort)
    # The script says what the interaction test came to; the test log need not.
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))

    drawn <- as.data.frame(figure()$value)
    expect_equal(env$rows_1$estimate, drawn$estimate, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.low, drawn$conf.low, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.high, drawn$conf.high, tolerance = 1e-8)

    # The p-value beside them is the same test taken the same way.
    expect_equal(env$p_interaction_1,
                 fy_result(figure()$value)$interaction_test$p.value,
                 tolerance = 1e-8)

    # A categorical exposure is its levels, the first of them the reference the
    # others are read against, taken within every subgroup.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "urbanicity", modifier = "male",
                            overall = FALSE))
    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(eval(parse(text = output$code_plain), envir = env)))

    drawn <- as.data.frame(figure()$value)
    expect_equal(nrow(env$rows_1), nrow(drawn))
    expect_equal(env$rows_1$estimate, drawn$estimate, tolerance = 1e-8)
  })
})

test_that("the plain script writes out a multinomial fit's arithmetic", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("nnet")
  # A fit of several equations has a coefficient vector holding a block for
  # each of them, so its contrast is a difference of blocks rather than a
  # difference of two rows of the design. The script says so and does it.
  cohort <- foresty_cohort
  my_fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                           data = cohort, trace = FALSE)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "sex", overall = FALSE))
    code <- output$code_plain

    expect_match(code, "3 outcome levels")
    expect_match(code, "equation_row <- function", fixed = TRUE)
    expect_match(code, "car::linearHypothesis.default(", fixed = TRUE)
    # One row per comparison between outcome levels, within each subgroup.
    expect_match(code, "lapply(outcomes, function(oc) {", fixed = TRUE)
    expect_match(code, "outcome = oc$level, against = oc$against,", fixed = TRUE)
    expect_false(grepl("ggplot(", code, fixed = TRUE))
    # Nothing it runs comes from the package it is the alternative to.
    runs <- grep("^\\s*#", strsplit(code, "\n")[[1L]], value = TRUE,
                 invert = TRUE)
    expect_false(any(grepl("foresty", runs, fixed = TRUE)))
    # Every line still fits the width the tab is read at.
    expect_true(all(nchar(strsplit(code, "\n", fixed = TRUE)[[1L]]) <= 78L))

    # And it runs, beside the model, and comes to the numbers on the figure:
    # the same rows in the same order, and the same joint test.
    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))

    drawn <- as.data.frame(figure()$value)
    expect_equal(nrow(env$rows_1), nrow(drawn))
    expect_equal(env$rows_1$estimate, drawn$estimate, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.low, drawn$conf.low, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.high, drawn$conf.high, tolerance = 1e-8)
    expect_equal(env$p_interaction_1,
                 fy_result(figure()$value)$interaction_test$p.value,
                 tolerance = 1e-8)

    # A categorical exposure, on the scale the model was fitted on, with the
    # Wald test: the coefficients it is taken over are one per equation, and
    # they are written a line apiece rather than off the side of the tab.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "maternal_smoking", modifier = "sex",
                            overall = FALSE, test = "wald", scale = "log"))
    code <- output$code_plain
    expect_match(code, "\"Transient:sexMale:maternal_smokingYes\",", fixed = TRUE)
    expect_true(all(nchar(strsplit(code, "\n", fixed = TRUE)[[1L]]) <= 78L))

    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    drawn <- as.data.frame(figure()$value)
    expect_equal(env$rows_1$estimate, drawn$estimate, tolerance = 1e-8)
    expect_equal(env$p_interaction_1,
                 fy_result(figure()$value)$interaction_test$p.value,
                 tolerance = 1e-8)
  })
})

test_that("the multinomial script reads its rows against the level chosen", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("nnet")
  cohort <- foresty_cohort
  my_fit <- nnet::multinom(wheeze_phenotype ~ no2 + sex + maternal_smoking,
                           data = cohort, trace = FALSE)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs, fy_app_defaults(exposure = "no2"))
    # Left on the level the model was fitted against, there is no row that is
    # the reference and nothing is written about one.
    expect_match(output$code_plain, "against = \"None\", reference = FALSE)",
                 fixed = TRUE)
    expect_false(grepl("drawn <- drawn[1]", output$code_plain, fixed = TRUE))

    # Another level is not a refit: the comparisons change and the model does
    # not, which is what the difference of two blocks buys.
    session$setInputs(outcome_reference = "Transient",
                      outcome_reference_row = TRUE)
    code <- output$code_plain
    expect_match(code, "against = \"Transient\"", fixed = TRUE)
    expect_match(code, "if (isTRUE(oc$reference)) drawn <- drawn[1]",
                 fixed = TRUE)
    expect_false(grepl("update(my_fit", code, fixed = TRUE))

    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    drawn <- as.data.frame(figure()$value)
    expect_equal(nrow(env$rows_1), nrow(drawn))
    expect_equal(env$rows_1$estimate, drawn$estimate, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.low, drawn$conf.low, tolerance = 1e-8)
    # The level everything is read against carries no estimate, and is drawn
    # once for the whole of the exposure rather than once per value of it.
    expect_equal(sum(is.na(env$rows_1$conf.low)), 1L)
  })
})

test_that("the plain script writes the test the figure actually reported", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("geepack")
  # The sidebar asks for a likelihood ratio test and this model has no
  # likelihood to take one over, so the figure reports the joint Wald test
  # instead. The script has to write that one: writing the other would print a
  # p-value that is not the one on the figure, and for this model would not run
  # at all.
  cohort <- foresty_cohort
  cohort$id <- seq_len(nrow(cohort))
  my_fit <- geepack::geeglm(asthma ~ no2 + sex + maternal_smoking,
                            family = binomial, id = id, data = cohort,
                            corstr = "independence")
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "sex", overall = FALSE, test = "lrt"))
    reported <- fy_result(figure()$value)$interaction_test
    expect_match(reported$test, "Wald")

    code <- output$code_plain
    expect_match(code, "wald_p(", fixed = TRUE)
    expect_false(grepl("lrt_p", code, fixed = TRUE))
    expect_match(fy_plain_prose(code), "the joint Wald test of the interaction",
                 fixed = TRUE)

    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    expect_equal(env$p_interaction_1, reported$p.value, tolerance = 1e-8)
  })
})

test_that("the plain script puts back what the loop took away with it", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("geepack")
  # A model fitted in a loop records the symbol its call was written with --
  # `corstr = cs` -- and not the string that symbol stood for. Pasted into a
  # session where the loop is over, update() has nothing to find, so the
  # script writes the symbol back from what the fit itself recorded.
  cohort <- foresty_cohort
  cohort$id <- rep(seq_len(nrow(cohort) / 4L), each = 4L)
  fits <- lapply(c("independence", "exchangeable"), function(cs)
    geepack::geeglm(asthma ~ no2 + sex + maternal_smoking, id = id,
                    data = cohort, family = binomial, corstr = cs))
  my_fit <- fits[[2L]]
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "sex", overall = FALSE))
    drawn <- as.data.frame(figure()$value)

    code <- output$code_plain
    expect_match(code, "cs <- my_fit$corstr", fixed = TRUE)

    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    expect_equal(env$rows_1$estimate, drawn$estimate, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.low, drawn$conf.low, tolerance = 1e-8)
  })
})

test_that("the plain script runs for a fit that keeps no copies to subset", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("lme4")
  # An S4 fit answers `$xlevels` with an error rather than with a NULL, and
  # keeps coef() for the coefficients of each group rather than for the fixed
  # effects, so a script written with those two would not run at all against
  # the model it was written for. `factor(visit)` is in the formula because a
  # factor written into it is what needs the levels most: rebuilt from the two
  # rows of a contrast it has one level, which has no contrasts.
  cohort <- foresty_cohort
  cohort$site <- factor(rep(seq_len(20), length.out = nrow(cohort)))
  cohort$visit <- rep(1:4, length.out = nrow(cohort))
  my_fit <- lme4::glmer(asthma ~ no2 + sex + factor(visit) + (1 | site),
                        family = binomial, data = cohort)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "sex", overall = FALSE))
    drawn <- as.data.frame(figure()$value)
    reported <- fy_result(figure()$value)$interaction_test

    code <- output$code_plain
    expect_match(code, "b <- lme4::fixef(model)", fixed = TRUE)
    expect_match(code, "xlev = .getXlevels(tt, model.frame(model))",
                 fixed = TRUE)
    expect_match(code, "contrasts.arg = attr(model.matrix(model)",
                 fixed = TRUE)
    expect_false(grepl("model$xlevels", code, fixed = TRUE))

    # And what it comes to is what was drawn.
    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    expect_equal(env$rows_1$estimate, drawn$estimate, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.low, drawn$conf.low, tolerance = 1e-8)
    expect_equal(env$p_interaction_1, reported$p.value, tolerance = 1e-8)
  })
})

test_that("a splined exposure spends its interaction across the equations", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  skip_if_not_installed("nnet")
  skip_if_not_installed("splines")
  # The interaction of a spline with the modifier adds one coefficient per
  # basis column to each equation of the fit, so the term the script adds and
  # the term it takes away again have to be that whole set: a reduced model
  # missing some of it would be a test of the wrong thing, and would still
  # produce a p-value.
  cohort <- foresty_cohort
  my_fit <- nnet::multinom(
    wheeze_phenotype ~ splines::ns(no2, 4) + sex + maternal_smoking,
    data = cohort, trace = FALSE)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "sex", overall = FALSE, test = "wald"))
    code <- output$code_plain
    # Four basis columns in each of the two equations.
    expect_equal(lengths(regmatches(code, gregexpr("sexMale\"", code))), 8L)

    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))
    expect_equal(env$p_interaction_1,
                 fy_result(figure()$value)$interaction_test$p.value,
                 tolerance = 1e-8)
    expect_equal(fy_result(figure()$value)$interaction_test$df, 8)

    # And the likelihood ratio test spends the same degrees of freedom, which
    # is what says the model the script subtracts the term from is the model
    # without it rather than a model missing something else as well.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "sex", overall = FALSE, test = "lrt"))
    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(
      eval(parse(text = output$code_plain), envir = env)
    ))
    expect_equal(attr(logLik(env$fit_int_1), "df") -
                   attr(logLik(update(env$fit_int_1,
                                      . ~ . - splines::ns(no2, 4):sex)), "df"),
                 8L)
    expect_equal(env$p_interaction_1,
                 fy_result(figure()$value)$interaction_test$p.value,
                 tolerance = 1e-8)
    expect_equal(fy_result(figure()$value)$interaction_test$df, 8)
  })
})

test_that("one reference group is offered per modifier and drawn per pair", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  cohort <- foresty_cohort
  cohort$male <- as.numeric(cohort$sex == "Male")
  my_fit <- fy_plain_fit(cohort)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(exposure = "urbanicity", modifier = "male",
                            overall = FALSE))
    # Unticked, the figure is the one it always was: each subgroup read against
    # its own reference level, and nothing said about a reference group.
    expect_false(grepl("reference", output$code, fixed = TRUE))

    session$setInputs(single_ref_male = TRUE, ref_level_male = "1",
                      ref_level_male_urbanicity = "Suburban")
    expect_match(output$code, "reference = c(urbanicity = \"Suburban\", male = \"1\")",
                 fixed = TRUE)

    # One row of the six is the reference, and it is the one that was chosen.
    est <- as.data.frame(figure()$value)
    expect_equal(sum(est$reference), 1L)
    expect_equal(est$level[est$reference], "Suburban")
    expect_equal(as.character(est$modifier_level[est$reference]), "1")

    # The script beside it calculates the same six numbers the same way.
    env <- fy_plain_session(my_fit, cohort)
    invisible(utils::capture.output(
      eval(parse(text = output$code_plain), envir = env)
    ))
    expect_equal(env$rows_1$estimate, est$estimate, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.low, est$conf.low, tolerance = 1e-8)

    # A continuous exposure has no levels to combine, so the pair is drawn as
    # it always was however the box is set.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "male",
                            overall = FALSE))
    expect_false(grepl("reference", output$code, fixed = TRUE))
    expect_equal(sum(as.data.frame(figure()$value)$reference), 0L)
  })
})

test_that("a label with a quote in it still writes a script that parses", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  cohort <- foresty_cohort
  cohort$male <- as.numeric(cohort$sex == "Male")
  my_fit <- fy_plain_fit(cohort)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    # A subgroup label is typed by hand, so it can hold anything a person
    # types. Pasted into the script unquoted, a quotation mark in it ends the
    # string early and the script no longer parses.
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE))
    session$setInputs(level_label_male_1 = 'girls ("female")',
                      level_label_male_2 = "boys \\ males")

    for (code in list(output$code, output$code_plain)) {
      expect_silent(parsed <- parse(text = code))
      expect_true(length(parsed) > 0L)
    }
    expect_match(output$code, 'girls (\\"female\\")', fixed = TRUE)
  })
})

test_that("two quantiles the wrong way round are not a reversed figure", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  info <- fy_model_info(fy_app_fit())

  # The lower one first, as the boxes read. A pair the other way round would
  # draw the effect of a fall in the exposure under labels saying it was a
  # rise, which is what the "Reverse the direction" box is for.
  expect_length(fy_app_quantile_values(info, "no2", c(0.1, 0.9)), 2L)
  expect_null(fy_app_quantile_values(info, "no2", c(0.9, 0.1)))
  expect_null(fy_app_quantile_values(info, "no2", c(0.5, 0.5)))
  expect_null(fy_app_quantile_values(info, "no2", c(-0.1, 0.9)))
})

test_that("the plain script follows the scale and the test that were chosen", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("patchwork")
  cohort <- foresty_cohort
  cohort$male <- as.numeric(cohort$sex == "Male")
  my_fit <- fy_plain_fit(cohort)
  app <- foresty_app(my_fit, launch = FALSE)

  shiny::testServer(app, {
    do.call(session$setInputs,
            fy_app_defaults(modifier = "male", overall = FALSE,
                            scale = "log", test = "wald", ci_level = 0.9))
    code <- output$code_plain
    expect_match(code, "exponentiate <- FALSE", fixed = TRUE)
    expect_match(code, "ci_level     <- 0.9", fixed = TRUE)
    expect_match(code, "wald_p(", fixed = TRUE)
    expect_false(grepl("lrt_p(", code, fixed = TRUE))
    # The test it does not take is not written out either, and the one it does
    # take is named rather than left for the reader to work out.
    expect_false(grepl("lrt_p <- function", code, fixed = TRUE))
    expect_match(fy_plain_prose(code), "the joint Wald test of the interaction",
                 fixed = TRUE)

    env <- fy_plain_session(my_fit, cohort)
    # The script says what the interaction test came to; the test log need not.
    invisible(utils::capture.output(eval(parse(text = code), envir = env)))

    drawn <- as.data.frame(figure()$value)
    expect_equal(env$rows_1$estimate, drawn$estimate, tolerance = 1e-8)
    expect_equal(env$rows_1$conf.low, drawn$conf.low, tolerance = 1e-8)
    expect_equal(env$p_interaction_1,
                 fy_result(figure()$value)$interaction_test$p.value,
                 tolerance = 1e-8)
  })
})

test_that("the Plot tab says what the counts are counts of, when it needs to", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  d <- foresty_cohort
  fit <- glm(asthma ~ urbanicity + no2 + sex, family = binomial, data = d)
  app <- foresty_app(fit, launch = FALSE)

  drawn <- function(output) paste(as.character(output$plots), collapse = "")

  shiny::testServer(app, {
    # A categorical exposure compares one of its levels with another, so its
    # rows carry both groups and a line under the figure says which two.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "urbanicity", modifier = "sex",
                            overall = FALSE, combine = FALSE))
    expect_match(drawn(output), "fy-caption", fixed = TRUE)
    expect_match(drawn(output), "the group its estimate is compared with")

    # A continuous exposure and a binary outcome are read the way a reader
    # reads them, and nothing is said.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "no2", modifier = "sex",
                            overall = FALSE, combine = FALSE))
    expect_false(grepl("fy-caption", drawn(output), fixed = TRUE))

    # The line goes with the columns it is about: turning the table off takes
    # it away with them.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "urbanicity", modifier = "sex",
                            overall = FALSE, combine = FALSE, table = FALSE))
    expect_false(grepl("fy-caption", drawn(output), fixed = TRUE))

    # Asking for the row's own group alone writes the option that says so and
    # changes what the line under the figure says.
    do.call(session$setInputs,
            fy_app_defaults(exposure = "urbanicity", modifier = "sex",
                            overall = FALSE, combine = FALSE,
                            counts_row = TRUE))
    expect_match(output$code, "counts = \"row\"", fixed = TRUE)
    expect_match(drawn(output), "no row carries the total behind its own")
  })
})
