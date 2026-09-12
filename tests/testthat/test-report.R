test_that("the report holds every section, the numbers and the plot", {
  skip_if_not_installed("gt")
  x <- foresty_interaction(fy_test_logistic(), exposure = "no2",
                           interaction = "sex")
  file <- fy_temp_html()
  on.exit(unlink(file), add = TRUE)
  foresty_report(x, file = file)

  expect_true(file.exists(file))
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")

  for (section in c("Model", "Interaction", "Subgroup-specific estimates",
                    "Forest plot", "Full coefficient table")) {
    expect_match(html, paste0("<h2>", section), fixed = TRUE)
  }
  expect_match(html, "src=\"data:image/png;base64,", fixed = TRUE)
  expect_match(html, "Ratio of odds ratios", fixed = TRUE)
  # The default test of the interaction, named as what it is.
  expect_match(html, "Likelihood ratio chi-square", fixed = TRUE)
  expect_match(html, "<table", fixed = TRUE)

  # The heading over the test and the table of ratios under it say what the
  # test is a test of, so the test is written as the test and nothing restates
  # the hypothesis.
  expect_false(grepl("term is zero", html, fixed = TRUE))
})

test_that("a report of several models carries each of them", {
  skip_if_not_installed("gt")
  d <- foresty_cohort
  x <- foresty_main(
    list(glm(asthma ~ no2 + sex, family = binomial, data = d),
         glm(asthma ~ black_carbon + sex, family = binomial, data = d)),
    exposure = c("no2", "black_carbon")
  )
  file <- fy_temp_html()
  on.exit(unlink(file), add = TRUE)
  foresty_report(x, file = file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")

  # Headed with the exposure each was fitted for, since that is what tells them
  # apart.
  expect_match(html, "<h2>Model: no2", fixed = TRUE)
  expect_match(html, "<h2>Model: black_carbon", fixed = TRUE)
  expect_match(html, "<h2>Full coefficient table: no2", fixed = TRUE)
  expect_match(html, "<h2>Full coefficient table: black_carbon", fixed = TRUE)

  # `model` cuts it down to one, which is then headed as a single model is.
  second <- fy_temp_html()
  on.exit(unlink(second), add = TRUE)
  foresty_report(x, file = second, model = 2)
  only <- paste(readLines(second, warn = FALSE), collapse = "\n")
  expect_match(only, "<h2>Model</h2>", fixed = TRUE)
  expect_false(grepl("no2 +", only, fixed = TRUE))
})

test_that("the report names whichever test was taken", {
  skip_if_not_installed("gt")
  file <- fy_temp_html()
  on.exit(unlink(file), add = TRUE)
  foresty_report(
    foresty_interaction(fy_test_logistic(), exposure = "no2",
                        interaction = "sex", test = "wald"),
    file = file
  )
  expect_match(paste(readLines(file, warn = FALSE), collapse = "\n"),
               "Wald chi-square", fixed = TRUE)
})

test_that("the html argument writes the same report", {
  skip_if_not_installed("gt")
  file <- fy_temp_html()
  on.exit(unlink(file), add = TRUE)
  foresty_interaction(fy_test_logistic(), exposure = "no2",
                      interaction = "sex", html = file)
  expect_true(file.exists(file))
  expect_gt(file.size(file), 10000)
})

test_that("html = TRUE writes the report under the variables' names", {
  skip_if_not_installed("gt")
  dir <- file.path(tempdir(), "foresty-html")
  dir.create(dir, showWarnings = FALSE)
  old <- setwd(dir)
  on.exit({
    setwd(old)
    unlink(dir, recursive = TRUE)
  }, add = TRUE)

  # The exposure and the modifier, joined by an underscore.
  foresty_interaction(fy_test_logistic(), exposure = "no2",
                      interaction = "sex", html = TRUE)
  expect_true(file.exists("no2_sex.html"))

  # A figure with no modifier behind it is named for its exposures alone.
  foresty_main(list(fy_test_logistic()), exposure = "no2", html = TRUE)
  expect_true(file.exists("no2.html"))

  d <- foresty_cohort
  foresty_main(
    list(glm(asthma ~ no2 + sex, family = binomial, data = d),
         glm(asthma ~ black_carbon + sex, family = binomial, data = d)),
    exposure = c("no2", "black_carbon"), html = TRUE
  )
  expect_true(file.exists("no2_black_carbon.html"))

  # FALSE, the default, writes nothing: nothing leaves the session unless it is
  # asked for.
  written <- sort(list.files())
  foresty_interaction(fy_test_logistic(), "no2", "maternal_smoking")
  expect_equal(sort(list.files()), written)

  expect_error(foresty_main(list(fy_test_logistic()), "no2", html = 1),
               "must be TRUE or FALSE")
})

test_that("a name written in another script still names its own file", {
  # A letter is a letter in any script. Reduced to the ASCII in them, these
  # two would both come back empty and both be written to foresty.html, so the
  # second figure would overwrite the first.
  # Built from their code points rather than written out, so that this file
  # stays the ASCII the rest of the package is.
  smoking <- intToUtf8(c(0x55ABL, 0x7159L))
  age <- intToUtf8(c(0x5E74L, 0x9F62L))
  expect_equal(fy_file_slug(smoking), smoking)
  expect_false(identical(fy_file_slug(smoking), fy_file_slug(age)))

  # What a file name cannot carry is still turned into an underscore, and a
  # name with nothing left in it still has the fallback behind it.
  expect_equal(fy_file_slug("ns(no2, 3)"), "ns_no2_3")
  expect_equal(fy_file_slug("%%%"), "")
})

test_that("a list of figures is not a report", {
  d <- foresty_cohort
  figures <- foresty_combine(
    NO2 = foresty_main(list(glm(asthma ~ no2 + sex, family = binomial,
                                data = d)), "no2"),
    `Black carbon` = foresty_main(
      list(glm(asthma ~ black_carbon + sex, family = binomial, data = d)),
      "black_carbon")
  )
  expect_error(foresty_report(figures, file = tempfile()), "one figure at a time")
})

test_that("an extension is added when one is missing", {
  skip_if_not_installed("gt")
  x <- foresty_main(list(fy_test_logistic()), exposure = "no2")
  stem <- tempfile()
  written <- foresty_report(x, file = stem)
  expect_equal(written, paste0(stem, ".html"))
  expect_true(file.exists(written))
  unlink(written)
})

test_that("a report of a linear model reports differences, not ratios", {
  skip_if_not_installed("gt")
  x <- foresty_interaction(fy_test_linear(), exposure = "black_carbon",
                           interaction = "sex")
  file <- fy_temp_html()
  on.exit(unlink(file), add = TRUE)
  foresty_report(x, file = file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")

  expect_match(html, "Mean difference", fixed = TRUE)
  expect_match(html, "Difference in effect", fixed = TRUE)
  expect_false(grepl("Odds ratio", html, fixed = TRUE))
})

test_that("the page names the model rather than the class of the object", {
  skip_if_not_installed("gt")
  file <- fy_temp_html()
  on.exit(unlink(file), add = TRUE)
  foresty_report(foresty_main(list(fy_test_logistic()), exposure = "no2"),
                 file = file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")

  # A reader checking the page against a methods section is looking for the
  # model, not for the function that fitted it.
  expect_match(html, "Logistic regression model", fixed = TRUE)
  expect_false(grepl(">glm, lm<", html, fixed = TRUE))

  # Every column of numbers is headed with the confidence level, so a row of
  # its own saying it again is not there.
  expect_false(grepl("Confidence level", html, fixed = TRUE))
  expect_match(html, "95% CI", fixed = TRUE)
})

test_that("each kind of model is named as that kind of model", {
  d <- foresty_cohort
  expect_equal(fy_model_name(fy_test_logistic()), "Logistic regression model")
  expect_equal(fy_model_name(fy_test_linear()), "Linear regression model")
  expect_equal(
    fy_model_name(glm(asthma ~ no2, family = poisson, data = d)),
    "Poisson regression model"
  )
  expect_equal(
    fy_model_name(glm(asthma ~ no2 + offset(log(followup_years)),
                      family = poisson, data = d)),
    "Poisson rate model"
  )
  expect_equal(
    fy_model_name(glm(asthma ~ no2, family = binomial(link = "log"), data = d)),
    "Log-binomial regression model"
  )
  # A model this package has never heard of is still named something, and what
  # fitted it is the only honest thing left to say.
  expect_match(fy_model_name(structure(list(), class = "brms_fit")),
               "brms_fit model")
})

test_that("the figure on the page is drawn tall enough to be read", {
  # Ten inches wide and two and a half tall is a strip rather than a forest
  # plot: the rows are what a reader is there for, and the height is what sets
  # them apart.
  one <- foresty_main(list(fy_test_logistic()), exposure = "no2")
  expect_gte(fy_plot_height(one), 3)

  # A figure of many rows is taller still, half an inch apiece.
  many <- foresty_main(
    list(glm(asthma ~ urbanicity + sex, family = binomial,
             data = foresty_cohort)),
    exposure = "urbanicity"
  )
  expect_gte(fy_plot_height(many), fy_plot_height(one))

  # A height asked for is the height drawn.
  expect_equal(fy_plot_height(one, height = 3), 3)
})

test_that("only a foresty object can be reported", {
  expect_error(foresty_report(list(a = 1), file = tempfile()),
               "foresty_interaction")
})

test_that("the page starts with its date and claims nothing at the foot", {
  skip_if_not_installed("gt")
  fit <- fy_test_logistic()
  x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
  file <- fy_temp_html()
  foresty_report(x, file = file)
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")

  # The date is what a directory of pages of the same analysis is told apart
  # by, so it is read before the numbers rather than after them.
  expect_match(html, "<p class=\"date\">", fixed = TRUE)
  expect_lt(regexpr("<p class=\"date\">", html, fixed = TRUE),
            regexpr("<h2>", html, fixed = TRUE))
  expect_false(grepl("<h1>", html, fixed = TRUE))
  expect_match(html, format(Sys.Date(), "%d %B %Y"), fixed = TRUE)

  expect_false(grepl("<footer", html, fixed = TRUE))
  expect_false(grepl("foresty package", html, fixed = TRUE))
})
