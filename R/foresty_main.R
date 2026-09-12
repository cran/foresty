#' Forest plot of exposure effects across models
#'
#' Draws the effect of one exposure from each of several fitted models, one
#' row apiece, so that exposures that were each fitted in their own model come
#' onto a single figure. None of the models may interact its exposure with
#' anything; use [foresty_interaction()] for those.
#'
#' The effect is the difference between two rows of the model's own design
#' matrix, one at the baseline value of the exposure and one at the value it is
#' compared with, so it is read off correctly whether the exposure is
#' continuous, binary or a factor with several levels, and whatever contrast
#' coding the fitting function used. The estimate and its confidence interval
#' are computed by [car::linearHypothesis()].
#'
#' A categorical exposure gets one row per level, the reference level included
#' and marked as such, with the levels named on the rows and the variable named
#' once at the left of the figure.
#'
#' @section A splined exposure:
#'
#' An exposure entered as a spline has no single effect to report: the
#' difference it makes depends on where along the curve it is taken. So the two
#' values are named, and the row says which they were:
#'
#' ```r
#' fit <- glm(asthma ~ splines::ns(no2, 3) + sex, family = binomial, data = d)
#' foresty_main(list(fit), exposure = "no2", at = c(10, 20))
#' ```
#'
#' The basis has to be built inside the formula, by `splines::ns()`,
#' `splines::bs()`, `stats::poly()` or another function of the variable, so the
#' two design-matrix rows can be evaluated at the two values.
#'
#' A basis computed before the fit and entered as columns of its own --
#' `Hmisc::rcspline.eval()` written into `spline1`, `spline2`, `spline3` and
#' then fitted as `y ~ spline1 + spline2 + spline3` -- cannot be handled that
#' way, because nothing in the fit records that those three columns are one
#' variable or how to recompute them at another value. Name the exposure and
#' the model refuses, saying that it is not in the model. Put the basis in the
#' formula instead, which fits exactly the same model:
#'
#' ```r
#' knots <- quantile(d$age, probs = c(0.05, 0.35, 0.65, 0.95))
#' fit <- glm(y ~ splines::ns(age, 4) + sex, family = binomial, data = d)
#' foresty_main(list(fit), exposure = "age", at = c(30, 60))
#' ```
#'
#' @section What the counts beside the rows count:
#'
#' The table beside the plot reports the number of people behind each row and,
#' where the outcome is an event, how many of them had it. Both are counted
#' over the rows the model was fitted to and not over the data frame it was
#' fitted from, so anyone missing the outcome, the exposure or any covariate in
#' the formula is left out of them -- which is the same complete-case set the
#' estimates come from, and is what makes the two agree. A column that is not
#' in the model does not affect them however much of it is missing.
#'
#' A row of a forest plot is usually half of a comparison, so by default the
#' other half is beside it: a row of suburban children reads `822 vs 468`,
#' those being the 822 the row is of and the 468 rural ones its odds ratio was
#' estimated against. `counts = "row"` in [foresty_layout()] holds the row's
#' own group alone instead. What the two groups are depends on what the row
#' compares:
#'
#' * A **continuous exposure** compares no two groups of people -- its row is a
#'   step along a slope -- so the count is the one group the row is of: every
#'   row of the model's data in a `foresty_main()` figure, everyone in the
#'   subgroup in a `foresty_interaction()` one.
#' * A **categorical exposure** compares one of its levels with the level the
#'   contrast is from, so a row carries the two: `822 vs 468` people and
#'   `125 vs 59` events. The reference row is the group the others are compared
#'   with rather than a comparison of its own, so it carries one number.
#' * A **multinomial fit** ([nnet::multinom()]) of an exposure with no levels
#'   compares two levels of the outcome, and the two numbers are how many
#'   people were at each: a "Transient vs None" row of women reads
#'   `637 vs 1,050`. That is the only pair of counts such a row has -- the
#'   events column would repeat it -- so it goes under the sizes and the events
#'   column is left off. Of a categorical exposure, the row compares the
#'   exposure's levels within one comparison of the outcome, so the sizes are
#'   the two exposure groups whatever their outcome and the events are the
#'   people at the row's outcome level within each.
#' * An **ordinal fit** ([MASS::polr()]) has one set of coefficients for the
#'   whole outcome, so its rows carry sizes and no count of events.
#'
#' A multinomial estimate is worth one caution the counts cannot give: it did
#' not come out of the two groups beside it alone. All the equations are fitted
#' over the whole outcome at once, so the people at the levels a row is not
#' about bear on it too, and a row reading `637 vs 1,050` is not the logistic
#' regression of those 1,687.
#'
#' The figures whose rows compare two groups say what the counts are under the
#' plot in [foresty_app()] and under the table of estimates in
#' [foresty_report()]; the ones that do not say nothing.
#'
#' @section Adjusting the figure:
#'
#' The result is a `ggplot2` object, so layers, scales and themes are added to
#' it as usual, and `+` always reaches the forest:
#'
#' ```r
#' foresty_main(list(fit), "no2") + ggplot2::coord_cartesian(xlim = c(0.8, 2))
#' foresty_main(list(fit), "no2") + ggplot2::theme_minimal(base_size = 14)
#' ```
#'
#' `&` reaches every panel of a figure that has more than one, which is worth
#' knowing about but rarely what you want: a scale or a coordinate system
#' applied to the table of numbers beside the plot will spoil it. Use `+`.
#'
#' @param fits A list of fitted models. Models fitted by [stats::glm()],
#'   [stats::lm()] and the `survival` package are supported,
#'   as is any fit supplying `coef()`, `vcov()` and a model frame.
#' @param exposure Name of the exposure variable in each model, as a character
#'   vector: either one name for all of them, or one name per model. Naming an
#'   element, as `exposure = c(NO2 = "no2")`, gives that variable its label on
#'   the figure, which saves repeating it in `labels`.
#' @param measure Effect measure, one of `"OR"`, `"RR"`, `"HR"`, `"IRR"`,
#'   `"MD"` or `"Coefficient"`. The default reads it from the model: a logistic
#'   regression gives an odds ratio, as do the ordinal and multinomial forms of
#'   one, a Cox model a hazard ratio, a Poisson
#'   model with an offset an incidence rate ratio, and a linear model a mean
#'   difference. Ratios are exponentiated; differences are not. What an offset
#'   holds is not recorded anywhere, so a Poisson model offset by something
#'   other than person-time -- the size of a population, say -- is named here
#'   instead, as `measure = "RR"`.
#' @param exponentiate Whether a ratio measure is drawn as a ratio. `TRUE`, the
#'   default, draws an odds ratio as an odds ratio. `FALSE` leaves it on the
#'   scale the model was fitted on: the figure reports a log odds ratio, read
#'   against zero rather than against one, and the estimates and their
#'   intervals come back on that scale from [tidy()] as well. It says nothing
#'   about a mean difference or a coefficient, which are on that scale already
#'   and are never exponentiated.
#' @param labels Named character vector giving the label to draw for a
#'   variable, as `c(no2 = "Nitrogen dioxide")`. Names not matched are left as
#'   they are. This is where the exposure is renamed, and naming it where it is
#'   chosen -- `exposure = c(NO2 = "no2")` -- comes to the same thing.
#' @param outcome What to call the outcome, as `outcome = "incident asthma"`.
#'   The default takes it from the left of the model's formula, which is the
#'   name of a column -- `asthma_ever_dx`, `evt5` -- and is rarely what a figure
#'   should say the effect is an effect on. It is written wherever the outcome
#'   is named: the axis under the plot, the title the package writes for itself,
#'   and the HTML report. `NA` names none, leaving "Adjusted odds ratio" on its
#'   own for a figure whose caption says what of.
#' @param outcome_reference For a multinomial logistic regression, the level of
#'   the outcome every estimate is read against, as
#'   `outcome_reference = "None"`. `NULL`, the default, is the level the model
#'   itself was referred to, read off the fit as the level it holds no equation
#'   for rather than assumed to be any particular one.
#'   Naming another does not refit anything: the odds ratio of one level
#'   against another is the difference between their two equations, and the
#'   covariance of the pair is already in the model. Every other level is then
#'   drawn against it, one row apiece, each row saying which two levels it
#'   compares. It says nothing about a model of one equation, whose reference
#'   is fixed by how the outcome is coded, and is refused there rather than
#'   ignored.
#' @param outcome_reference_row Whether that level is drawn as a row of its
#'   own. `FALSE`, the default, draws only the levels compared with it, each
#'   row saying which two levels it compares. `TRUE` adds a row for the
#'   reference level itself, the way the reference level of a categorical
#'   exposure is drawn: it carries no estimate, being the definition the other
#'   rows are differences from, so it is `1` on the ratio scale and `0` on the
#'   scale the model was fitted on, with no interval, no test and no p-value.
#'   It is one row whatever the exposure is, since it is the same definition at
#'   every value of it, and the counts beside it are of everybody in that level
#'   of the outcome for the same reason: the row is not about a value of the
#'   exposure, so it is not counted at one. On a figure of subgroups it is drawn
#'   once inside each and counted within it. It says nothing about a model of
#'   one equation.
#' @param xlab The label under the plot, which by default names the measure and
#'   the outcome, as `"Adjusted odds ratio for asthma"`. A string is drawn as it
#'   was given -- `xlab = "Odds ratio (95% CI), NO2 per 10 ug/m3"` -- and `NA`
#'   draws none. Renaming only the outcome is what `outcome` is for; this
#'   replaces the whole line.
#' @param person_time The unit person-time is reported in, for a model that
#'   carries any. `NULL`, the default, reports the total the model was fitted
#'   over. A number divides by it, so `person_time = 1000` draws a column of
#'   thousands of person-years and heads it `"Person-time (per 1,000)"`, since a
#'   count of person-time that does not say what it counts cannot be read
#'   against another study's. Naming the number heads the column outright, as
#'   `person_time = c("Person-years (per 1,000)" = 1000)`. The unit reaches the
#'   figure, [summary()] and the HTML report alike. It does not refit the model
#'   or change its estimates, confidence intervals, or p-values: `person_time`
#'   controls only how the person-time column is written.
#' @param ci_level Confidence level of the intervals. Defaults to `0.95`.
#' @param contrast For a continuous exposure, the increment the effect is
#'   reported per. `NULL`, the default, is one unit and is not written on the
#'   figure. An increment you name is: `contrast = 10` draws the row as
#'   `"NO2 (per 10)"` and `contrast = 1` draws it as `"NO2 (per 1)"`, the same
#'   estimate as the default said out loud. No unit is invented, since the
#'   package cannot know what a column's numbers
#'   mean; name the unit in `labels`, as `c(no2 = "NO2, ug/m3")`, and the row
#'   reads `"NO2, ug/m3 (per 10)"`. `contrast = "iqr"` takes the increment from
#'   the data instead: the interquartile range of the exposure as the model saw
#'   it, which is what an exposure with no natural unit is usually reported per.
#'   The range it came to is written beside the variable, as
#'   `"NO2 (per IQR, 8.44)"`, because an effect per interquartile range cannot
#'   be compared with anything unless the figure says which range that was.
#'   `contrast` says nothing about a categorical exposure, whose comparisons are
#'   its levels.
#' @param at The two values of the exposure to contrast, as `c(from, to)`.
#'   Which two they were is written beside the exposure, as `"NO2 (10 -> 20)"`,
#'   wherever the exposure is named: every row of a figure is that same
#'   comparison taken within another subgroup, so it is said once rather than
#'   on each of them. An exposure entered as a spline, or in any other way that
#'   spreads it over more than one coefficient, has no single effect to report
#'   and is drawn only when `at` names the two values; any other exposure may
#'   be given them too. `at` and `contrast` both say which two values are
#'   compared, so only one of them is accepted at a time, but they do not say it
#'   the same way: `contrast` is an increment taken from the middle of the
#'   exposure's own distribution, and `at` is the two values themselves. Where
#'   the exposure enters the model as it stands the two come to the same number,
#'   an increment being the same difference wherever it is taken; where it
#'   enters transformed -- `log(no2)`, a spline, a polynomial -- they do not,
#'   and `at` is the one that says where on the curve the difference was taken.
#'   For a categorical exposure the two values are two of its levels, and the
#'   figure is then that one comparison rather than a row for every level.
#' @param vcov Robust standard errors. `NULL`, the default, uses the model's
#'   own. `"robust"` gives the heteroskedasticity-consistent sandwich estimator
#'   (`HC1`), and `"HC0"` to `"HC4"` name one exactly; both come from the
#'   `sandwich` package. A function is called on the fit, and a matrix is used
#'   as it stands. For a Cox model refit with `robust = TRUE`; a fit that is
#'   already robust is used as it is.
#' @param cluster Cluster-robust standard errors, passed to
#'   [sandwich::vcovCL()]. It says which observations belong together, so it
#'   takes a column name, a vector of one identifier per observation, or a
#'   one-sided formula: `cluster = "practice_id"`, `cluster = data$practice_id`
#'   or `cluster = ~practice_id`.
#' @param table Whether to draw the table of numbers beside the plot. Defaults
#'   to `TRUE`, a forest plot being read from the numbers as much as from the
#'   marks; `table = FALSE` leaves a plain figure, and `summary()` and [tidy()]
#'   report the same numbers at the console either way.
#' @param columns Which columns the table carries, from `"estimate"`, `"p"`,
#'   `"n"`, `"events"`, `"person_time"`, `"interaction_p"` and, where both
#'   tests of an interaction were asked for, `"interaction_p_lrt"`. The default
#'   shows the estimate and the p-value together with whichever of the others
#'   the models can supply. On a figure reporting a test of an interaction the
#'   p-value of each row is left off, being easily read as the test beside it;
#'   name it in `columns` to have it back.
#' @param layout How the figure is drawn: the name of a style, as
#'   `layout = "jama"`, or a layout built by [foresty_layout()] when something
#'   about it has to be changed. The styles are `"classic"`, `"jama"`,
#'   `"nejm"`, `"lancet"`, `"bmj"` and `"revman"`.
#' @param title Plot title. The default names the measure, the outcome it is a
#'   measure of, the exposure it is reported for and the fact that the estimate
#'   comes from a model with no interaction term in it, as `"Adjusted odds
#'   ratio for asthma associated with NO2, from one model without an
#'   interaction term"`. `NA` draws none, and the journal styles draw none, a
#'   caption being where a journal puts that.
#' @param subtitle Plot subtitle. `NULL`, the default, draws none.
#' @param html Whether to write the HTML report -- the model it was drawn from,
#'   the estimates, the figure and the whole coefficient table, on a page that
#'   is a single file and can be sent on. `FALSE`, the default, writes nothing,
#'   so nothing leaves the session unless it is asked for. `TRUE` writes it to
#'   a file named for the exposures it is about, joined by underscores where
#'   there is more than one, so that a figure of NO2 is written to
#'   `no2.html` in the working directory. A path writes it there instead, as
#'   `html = "reports/no2.html"`. The report can also be written at any time
#'   afterwards with [foresty_report()].
#'
#' @return A `ggplot2` object, of class `foresty`, carrying the estimates it
#'   was drawn from. Print it to draw it. [summary()] reports the coefficients
#'   and tests behind it, [tidy()] returns the estimates as a data frame, and
#'   [predict()] passes through to the underlying model.
#'
#' @seealso [foresty_interaction()], [foresty_layout()], [foresty_report()].
#'
#' @examples
#' fit_no2 <- glm(asthma ~ no2 + sex + maternal_smoking + maternal_age,
#'                family = binomial, data = foresty_cohort)
#' fit_bc <- glm(asthma ~ black_carbon + sex + maternal_smoking + maternal_age,
#'               family = binomial, data = foresty_cohort)
#'
#' foresty_main(
#'   list(fit_no2, fit_bc),
#'   exposure = c("no2", "black_carbon"),
#'   labels = c(no2 = "NO2", black_carbon = "Black carbon")
#' )
#'
#' # A categorical exposure: the levels are named, the variable once at the left.
#' fit_urban <- glm(asthma ~ urbanicity + sex + maternal_age,
#'                  family = binomial, data = foresty_cohort)
#' foresty_main(list(fit_urban), exposure = "urbanicity")
#'
#' # Every variation the figure has, drawn one after another. They are
#' # skipped by the timed run of the examples only because there are many
#' # of them, not because any one is slow.
#' \donttest{
#' # In the layout of a journal, and without the numbers beside the plot.
#' foresty_main(list(fit_urban), exposure = "urbanicity", layout = "jama")
#' foresty_main(list(fit_urban), exposure = "urbanicity", table = FALSE)
#'
#' # Per 10 units of the exposure rather than per 1, which the row says.
#' foresty_main(list(fit_no2), exposure = "no2", contrast = 10)
#'
#' # A rate model. The offset is the time each child was followed for, so the
#' # measure is an incidence rate ratio and the person-time behind each row is
#' # drawn beside the counts.
#' fit_rate <- glm(asthma ~ no2 + sex + maternal_age +
#'                   offset(log(followup_years)),
#'                 family = poisson, data = foresty_cohort)
#' foresty_main(list(fit_rate), exposure = "no2",
#'              labels = c(no2 = "NO2"), contrast = 10)
#'
#' # A splined exposure: the two values being compared are named.
#' fit_spline <- glm(asthma ~ splines::ns(no2, 3) + sex + maternal_age,
#'                   family = binomial, data = foresty_cohort)
#' foresty_main(list(fit_spline), exposure = "no2", at = c(10, 20))
#'
#' # On the scale the model was fitted on, as a log odds ratio about zero.
#' foresty_main(list(fit_no2), exposure = "no2", exponentiate = FALSE)
#'
#' # The exposure, the outcome and the axis all named by hand.
#' foresty_main(list(fit_no2), exposure = c(`NO2, ug/m3` = "no2"),
#'              outcome = "incident asthma by age 8",
#'              xlab = "Adjusted odds ratio (95% CI)")
#'
#' # Person-time reported per 1,000 rather than as the total.
#' foresty_main(list(fit_rate), exposure = "no2", person_time = 1000)
#'
#' # A multinomial logistic regression has one equation per non-reference level
#' # of the outcome, so the exposure has one effect per level and the figure
#' # has one row per level. `outcome_reference` says which level they are all
#' # read against.
#' if (requireNamespace("nnet", quietly = TRUE)) {
#'   fit_phenotype <- nnet::multinom(
#'     wheeze_phenotype ~ no2 + sex + maternal_smoking,
#'     data = foresty_cohort, trace = FALSE
#'   )
#'   # Against "None", which is the level the model itself was fitted against,
#'   # and drawn as a row of its own so that the figure says so.
#'   print(foresty_main(list(fit_phenotype), exposure = "no2", contrast = 10,
#'                      outcome_reference_row = TRUE))
#'
#'   # The same estimates read against another level instead.
#'   against_transient <- foresty_main(list(fit_phenotype), exposure = "no2",
#'                                     contrast = 10,
#'                                     outcome_reference = "Transient")
#'   summary(against_transient)
#' }
#' }
#'
#' @export
foresty_main <- function(fits,
                         exposure,
                         measure = NULL,
                         exponentiate = TRUE,
                         labels = NULL,
                         outcome = NULL,
                         outcome_reference = NULL,
                         outcome_reference_row = FALSE,
                         ci_level = 0.95,
                         contrast = NULL,
                         at = NULL,
                         vcov = NULL,
                         cluster = NULL,
                         table = TRUE,
                         columns = NULL,
                         person_time = NULL,
                         layout = NULL,
                         title = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         html = FALSE) {
  fits <- fy_check_fit_list(fits)
  # A label written where the exposure is named -- `exposure = c(NO2 = "no2")`
  # -- says what to call it as well as which one is meant.
  labels <- fy_absorb_labels(exposure, labels)
  exposure <- unname(exposure)
  checkmate::assert_character(exposure, min.len = 1L, any.missing = FALSE)
  checkmate::assert_number(ci_level, lower = 0.5, upper = 0.9999)
  checkmate::assert_flag(table)
  layout <- fy_as_layout(layout)
  person_time <- fy_person_time_spec(person_time)

  if (length(exposure) == 1L) {
    exposure <- rep(exposure, length(fits))
  }
  if (length(exposure) != length(fits)) {
    stop(
      "`exposure` must name one exposure, or one exposure for each of the ",
      length(fits), " models supplied, not ", length(exposure),
      call. = FALSE
    )
  }

  infos <- lapply(fits, fy_model_info, measure = measure,
                  exponentiate = exponentiate, vcov = vcov, cluster = cluster,
                  outcome = outcome)
  pieces <- Map(function(info, e) {
    fy_guard_no_interaction(info, e)
    values <- fy_exposure_values(info, e, contrast = contrast, at = at)
    fy_exposure_estimates(
      info, e, values, ci_level = ci_level,
      comparisons = fy_outcome_comparisons(info, outcome_reference,
                                           outcome_reference_row)
    )
  }, infos, exposure)

  estimates <- do.call(rbind, pieces)
  measures <- vapply(infos, `[[`, character(1), "measure")
  if (length(unique(measures)) > 1L) {
    stop(
      "the models supplied report different effect measures (",
      paste(unique(measures), collapse = ", "),
      "), so they cannot be drawn on one axis; pass `measure` to fix a common one",
      call. = FALSE
    )
  }

  info <- infos[[1L]]
  adjusted <- all(mapply(fy_is_adjusted, infos, exposure))

  # A multinomial fit reports one estimate per comparison between outcome
  # levels. Where the exposure has no levels of its own, those comparisons are
  # what the rows are and they take the column of labels. Where it has -- a
  # categorical exposure, whose rows are already its levels -- the comparisons
  # become the blocks instead, so that each is read down as the levels of the
  # exposure within that one comparison.
  by_outcome <- any(!is.na(estimates$outcome_label))
  by_level <- any(!is.na(estimates$level))
  if (by_outcome && !by_level) {
    estimates <- fy_fold_outcome_into_level(estimates)
  }
  estimates <- fy_finish_estimates(estimates, labels)
  if (by_outcome && by_level) {
    estimates$outcome_label <- fy_outcome_label_factor(estimates)
  }

  # What the rows are of. The column carries the exposures, so it is headed
  # with the word, and the figure says which effect it is showing and that it
  # comes from a model with no interaction in it -- which is what separates it
  # from a foresty_interaction() figure of the same exposure.
  # Where the rows are comparisons between outcome levels, the column beside
  # the plot carries outcomes rather than exposures and is headed for what is
  # in it.
  label_header <- layout$headings$label %||%
    if (by_outcome && !by_level) "Outcome" else "Exposure"
  title <- fy_resolve_title(
    title, layout,
    fy_main_title(info, adjusted, ci_level, estimates, length(fits))
  )

  plot <- fy_forest_plot(
    estimates,
    exponentiate = info$exponentiate,
    measure_label = fy_resolve_xlab(xlab, fy_axis_label(info, adjusted)),
    estimate_header = fy_estimate_header(info, adjusted, ci_level,
                                         with_outcome = FALSE),
    table = table,
    columns = columns,
    # A categorical exposure names its levels on the rows, so the variable it
    # belongs to is written once at the left instead of on every row.
    group = if (by_outcome && by_level) {
      "outcome_label"
    } else if (any(!is.na(estimates$level))) {
      "variable_label"
    },
    title = title,
    subtitle = subtitle,
    layout = layout,
    label_header = label_header,
    person_time = person_time
  )

  out <- fy_new_result(
    plot,
    estimates = estimates,
    infos = infos,
    exposure = exposure,
    counts_note = fy_counts_note(estimates, by_level = by_level,
                                 by_outcome = by_outcome, table = table,
                                 columns = columns, counts = layout$counts),
    counts_reading = c(by_level = by_level, by_outcome = by_outcome),
    counts_mode = layout$counts,
    measure = info$measure,
    measure_label = info$measure_label,
    exponentiate = info$exponentiate,
    ci_level = ci_level,
    adjusted = adjusted,
    robust = info$robust,
    person_time = person_time
  )

  file <- fy_report_file(html, exposure)
  if (!is.null(file)) {
    foresty_report(out, file = file)
  }
  out
}

# Which title the figure ends up with. `NULL` asks for the one the function
# writes for itself, `NA` for none at all, and anything else is drawn as it was
# given. A journal's style writes none of its own, the caption being where a
# journal puts that; a title passed by hand is always drawn.
#
# `default` is only forced when it is going to be used.
fy_resolve_title <- function(title, layout, default) {
  if (length(title) == 1L && is.na(title)) {
    return(NULL)
  }
  if (!is.null(title)) {
    return(title)
  }
  if (!isTRUE(layout$auto_labels)) {
    return(NULL)
  }
  # A title the package wrote is long enough to run off a narrow device, and
  # the width the figure will be drawn at is not known here, so it is broken
  # over two lines rather than over the edge. One passed by hand is drawn as it
  # was given.
  fy_wrap(default, 72)
}

# "Adjusted odds ratio for asthma associated with NO2, from one model without
# an interaction term".
#
# What comes after "odds ratio for" is the outcome, which is what the ratio is
# a ratio of; the exposure it is reported for is named after it. The title also
# says where the estimate came from: one model apiece, none of them carrying an
# interaction, which is the thing a reader has to know to read the figure
# beside an interaction one.
fy_main_title <- function(info, adjusted, ci_level, estimates, n_models) {
  exposures <- fy_and(unique(as.character(estimates$variable_label)))
  source <- if (n_models == 1L) {
    "from one model without an interaction term"
  } else {
    paste0("from ", n_models,
           " models, none of them with an interaction term")
  }
  paste0(fy_effect_phrase(info, adjusted, exposures), ", ", source)
}

# "a", "a and b", "a, b and c".
fy_and <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1L) {
    return(paste(x, collapse = ""))
  }
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

fy_check_fit_list <- function(fits) {
  # A fitted model is itself a list, so a list of models is told from a single
  # model by whether the container has a class of its own.
  if (!is.list(fits) || !is.null(attr(fits, "class"))) {
    stop(
      "`fits` must be a list of fitted models, as `list(fit)` or ",
      "`list(fit_no2, fit_bc)`, even when there is only one of them.",
      call. = FALSE
    )
  }
  if (!length(fits)) {
    stop("`fits` is an empty list; supply at least one fitted model",
         call. = FALSE)
  }
  fits
}

fy_guard_no_interaction <- function(info, exposure) {
  with_vars <- fy_interacting_vars(info, exposure)
  if (length(with_vars)) {
    stop(
      "\"", exposure, "\" is interacted with ",
      paste0("\"", with_vars, "\"", collapse = " and "),
      " in this model, so it has no single effect to draw. Use ",
      "foresty_interaction() to see its effect within each level of ",
      "\"", with_vars[1L], "\", or refit without the interaction.",
      call. = FALSE
    )
  }
  main <- fy_exposure_terms(info, exposure)$main
  if (!length(main)) {
    stop(
      "\"", exposure, "\" does not appear in this model. Its terms are ",
      paste0("\"", names(info$term_map), "\"", collapse = ", "), ".",
      fy_transformed_hint(exposure),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# What to say to someone whose exposure was transformed before the fit rather
# than in the formula: a basis written into columns of its own is three
# variables as far as the fit is concerned, and nothing in it records that they
# came from one variable or how to work them out at another value of it, which
# is what a contrast between two values needs.
fy_transformed_hint <- function(exposure) {
  paste0(
    " If \"", exposure, "\" was expanded into columns of its own before the ",
    "model was fitted -- a spline basis written into spline1, spline2, ... ",
    "-- the fit knows nothing of \"", exposure, "\" itself. Put the basis in ",
    "the formula instead, as splines::ns(", exposure, ", 3) or splines::bs(",
    exposure, ", 3), and name the two values to compare with ",
    "`at = c(from, to)`."
  )
}

# Adds the drawing labels and fixes the order rows appear in, which is the
# order they were given in, read down the figure.
fy_finish_estimates <- function(estimates, labels) {
  drawn <- fy_row_labels(estimates, labels)
  estimates$variable_label <- factor(drawn$variable_label,
                                     levels = unique(drawn$variable_label))
  estimates$label <- factor(drawn$row_label, levels = rev(unique(drawn$row_label)))
  rownames(estimates) <- NULL
  estimates
}
