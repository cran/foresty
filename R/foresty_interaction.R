#' Exposure effect within each level of an effect modifier
#'
#' Takes a model that does not yet interact the exposure with the modifier,
#' updates it with the interaction term, and estimates the exposure effect
#' separately within every level of the modifier. The estimates are drawn one
#' level above the other, a row to a level, and can be written to a
#' self-contained HTML page carrying the joint test of the interaction.
#'
#' The point is to make an interaction readable. The p-value of an interaction
#' term reports only that the subgroups depart from a common effect; it does
#' not say which subgroup carries the effect, in which direction, or how far
#' apart they are, and a small p-value from a large study can accompany
#' subgroup estimates that are practically identical. Seeing the
#' subgroup-specific estimates and the test together is what settles whether an
#' interaction is worth reporting.
#'
#' Each subgroup effect is a linear combination of the coefficients of the
#' single interaction model, not a separate model fitted in each subgroup: in
#' the reference level of the modifier it is the exposure term alone, and in
#' the other levels it is the exposure term plus the relevant interaction
#' terms. Every subgroup therefore shares one estimate of the covariate
#' effects, and the interaction can be tested. The combinations and their
#' tests are computed by [car::linearHypothesis()].
#'
#' @section One reference group for the whole figure:
#'
#' By default every row is the effect of the exposure inside one subgroup, so
#' each subgroup is read against its own reference level and the rows of one
#' subgroup are not comparisons with the rows of another. Where the exposure and
#' the modifier are both categorical there is a second question a figure can
#' answer: what every combination of the two comes to against one of them.
#' `reference` asks it. `reference = c(ecog = "0-1", egfr = "Negative")` names
#' the one combination the rest are compared with, and each row is then that
#' whole cell -- this level of the exposure and this level of the modifier --
#' against that cell, all of it out of the same interaction model. The cell
#' named carries no estimate of its own and is drawn as the reference it is.
#'
#' The two figures answer different questions and the difference is worth being
#' clear about. Without `reference`, a row says how much the exposure matters in
#' that subgroup, and the interaction test beside the rows says whether those
#' effects differ. With it, a row says how far that combination of the two
#' variables is from one chosen combination, which is what a table of six groups
#' against one baseline group reports, and the interaction test beside them is
#' the same test of the same coefficients: it still asks whether the effect of
#' the exposure depends on the modifier, and it is not a test of the rows.
#'
#' `reference = TRUE` takes the first level of each, which is the cell a model
#' fitted under R's default treatment contrasts takes as its own baseline. Which
#' cell that is makes no difference to the estimates -- each row is a difference
#' between two rows of the design matrix, so it comes out the same however the
#' factors were coded -- but it is what the rest of the figure is read against,
#' so name the cell with `reference = c(...)` where the first level is not the
#' one a reader should start from.
#'
#' @section Testing the interaction:
#'
#' The p-value beside the subgroups is the joint test that every exposure by
#' modifier coefficient is zero. It is the likelihood ratio test by default,
#' comparing the likelihood of the model carrying the interaction with that of
#' the same model without it.
#'
#' `test = "wald"` takes the joint Wald test over the coefficients and their
#' covariance instead, which is what a model summary reports and what a robust
#' or cluster-robust variance changes. The two tests answer the same question
#' and usually agree; they part company where the Wald approximation is poor --
#' a small study, a rare outcome, a sparse subgroup, an estimate far from the
#' null -- and the likelihood ratio test is the more trustworthy of the two
#' there, which is why it is the default. `test = "both"` reports each in a
#' column of its own, which is a way of showing that the reported p-value does
#' not turn on which test was chosen.
#'
#' The likelihood ratio test is computed from the likelihood, so it has no
#' robust form: it takes no account of a variance passed through `vcov` or
#' `cluster`. Where it would not match the intervals drawn beside it for that
#' reason, the Wald test is reported in its place and `foresty` says so; ask for
#' the likelihood ratio test by name there and it is given, with a warning that
#' it does not match them.
#'
#' A model with no likelihood at all is a different case, and no likelihood
#' ratio test is reported for one however it is asked for. A GEE is the one that
#' comes up: it estimates its coefficients from estimating equations rather than
#' from a likelihood, and its standard errors are the sandwich ones from the
#' start, so the joint Wald test is the test of it. A quasi-likelihood family is
#' the same case. `test = "lrt"` on either is an error naming the reason rather
#' than a number that would look like a likelihood ratio test and not be one;
#' the default and `test = "both"` report the Wald test and say that they did.
#'
#' For a linear model the likelihood ratio test is the chi-square form rather
#' than the exact F test, which is what the Wald test gives there.
#'
#' A [survey::svyglm()] fit maximizes a pseudo-likelihood weighted by the
#' design, so the ordinary likelihood ratio test does not apply to it. In its
#' place `test = "lrt"` takes the Rao-Scott working likelihood ratio test that
#' `survey` reports for two nested fits, through [survey::anova.svyglm()], and
#' `test = "wald"` the F test on the degrees of freedom of the design, which is
#' what [survey::regTermTest()] reports. Both are design-based from the start,
#' so neither is affected by the caveat above.
#'
#' @inheritSection foresty_main What the counts beside the rows count
#' @inheritSection foresty_main Adjusting the figure
#'
#' @inheritParams foresty_main
#' @param fit A fitted model. If it already contains the exposure by modifier
#'   interaction it is used as it stands; otherwise it is updated to add it.
#' @param exposure Name of the exposure variable, as a character string.
#'   Naming it, as `exposure = c(NO2 = "no2")`, gives it that label on the
#'   figure, which saves repeating it in `labels`.
#' @param interaction Name of the effect modifier, as a character string, and
#'   named as `interaction = c(Sex = "sex")` where the figure is to call it
#'   something other than what the column is called. It must have at least two
#'   levels. A numeric column taking exactly two values counts as having them,
#'   so a flag coded 0 and 1 is read as those two subgroups without being
#'   wrapped in [factor()] and fitted again: it entered the model through the
#'   one coefficient a two-level factor would have given, and the estimates and
#'   the test are the same either way. Its rows are labelled with the values
#'   themselves, `0` and `1`, which is rarely what a figure should say, so name
#'   them through `level_labels`. A modifier taking three or more numeric values
#'   is rejected: categorize it with [cut()] and refit, so that the subgroups
#'   being compared are the ones you intend and the test has the degrees of
#'   freedom the figure implies.
#' @param level_labels Named character vector giving the label to draw for a
#'   level of the modifier, as `c(F = "Female", M = "Male")`, or
#'   `c("0" = "No", "1" = "Yes")` for a modifier coded as a flag.
#' @param reference The one combination of the exposure and the modifier every
#'   row is compared with, as a named character vector naming a level of each:
#'   `reference = c(ecog = "0-1", egfr = "Negative")`. `TRUE` takes the first
#'   level of each. `NULL`, the default, compares nothing across subgroups: each
#'   row is the effect of the exposure inside its own subgroup. Both variables
#'   must be categorical for a combination of them to be a group at all. See
#'   *One reference group for the whole figure*.
#' @param test How the interaction is tested: `"lrt"`, the default, is the
#'   likelihood ratio test against the same model without the interaction
#'   terms; `"wald"` is the joint Wald test of those terms; `"both"` reports
#'   the two of them in columns of their own. See *Testing the interaction*.
#' @param title Plot title. The default names the measure, the outcome it is a
#'   measure of, the exposure, the modifier and the fact that every subgroup
#'   estimate comes out of one model carrying their interaction term, as
#'   `"Adjusted odds ratio for asthma associated with NO2 within each level of
#'   Sex, from one model containing their interaction term"`. `NA` draws none,
#'   and the journal styles draw none, a caption being where a journal puts
#'   that.
#' @param subtitle Plot subtitle. `NULL`, the default, draws none.
#' @param html Whether to write the HTML report. `FALSE`, the default, writes
#'   nothing, so nothing leaves the session unless it is asked for. `TRUE`
#'   writes it to a file named for the variables it is about, the exposure and
#'   the modifier joined by an underscore, so that NO2 by sex is written to
#'   `no2_sex.html` in the working directory. A path writes it there instead,
#'   as `html = "reports/no2_sex.html"`. The report can also be written at any
#'   time afterwards with [foresty_report()].
#'
#' @return A `ggplot2` object, of class `foresty`, carrying the estimates and
#'   the interaction test. Print it to draw it. [summary()] reports the
#'   coefficients and the test, and [tidy()] returns the estimates as a data
#'   frame.
#'
#' @seealso [foresty_main()], [foresty_layout()], [foresty_report()].
#'
#' @examples
#' fit <- glm(asthma ~ no2 + sex + maternal_smoking + maternal_age,
#'            family = binomial, data = foresty_cohort)
#'
#' # Sex, where the interaction is real.
#' by_sex <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
#' by_sex
#' summary(by_sex)
#'
#' # The rest of what the function can be asked for, drawn one after
#' # another. Each is quick; there are simply several of them.
#' \donttest{
#' # Maternal smoking, where it is not.
#' foresty_interaction(fit, exposure = "no2", interaction = "maternal_smoking")
#'
#' # Naming the modifier labels it, and both tests of the interaction can be
#' # reported side by side.
#' foresty_interaction(fit, exposure = c(NO2 = "no2"),
#'                     interaction = c(Sex = "sex"), test = "both")
#'
#' # Every combination of two categorical variables against one of them, which
#' # is the other question a two-way table of groups asks.
#' fit2 <- glm(asthma ~ urbanicity + sex + maternal_age, family = binomial,
#'             data = foresty_cohort)
#' foresty_interaction(fit2, exposure = "urbanicity", interaction = "sex",
#'                     reference = c(urbanicity = "Rural", sex = "Female"))
#'
#' # The subgroup estimates, the numbers behind them and the test of the
#' # interaction, in the layout of a journal.
#' foresty_interaction(fit, exposure = "no2", interaction = "sex",
#'                     layout = "jama")
#'
#' # A multinomial outcome. Each subgroup carries one row per comparison
#' # between outcome levels, and the interaction is tested jointly across the
#' # equations, so it has one degree of freedom for each of them.
#' if (requireNamespace("nnet", quietly = TRUE)) {
#'   fit_phenotype <- nnet::multinom(
#'     wheeze_phenotype ~ no2 + sex + maternal_smoking,
#'     data = foresty_cohort, trace = FALSE
#'   )
#'   print(foresty_interaction(fit_phenotype, exposure = "no2",
#'                             interaction = "sex", contrast = 10))
#' }
#' }
#'
#' @export
foresty_interaction <- function(fit,
                                exposure,
                                interaction,
                                measure = NULL,
                                exponentiate = TRUE,
                                labels = NULL,
                                level_labels = NULL,
                                reference = NULL,
                                outcome = NULL,
                                outcome_reference = NULL,
                                outcome_reference_row = FALSE,
                                ci_level = 0.95,
                                contrast = NULL,
                                at = NULL,
                                vcov = NULL,
                                cluster = NULL,
                                test = c("lrt", "wald", "both"),
                                table = TRUE,
                                columns = NULL,
                                person_time = NULL,
                                layout = NULL,
                                title = NULL,
                                subtitle = NULL,
                                xlab = NULL,
                                html = FALSE) {
  # A label written where the variable is named -- `interaction = c(Sex =
  # "sex")` -- says what to call it as well as which one is meant.
  labels <- fy_absorb_labels(interaction, fy_absorb_labels(exposure, labels))
  exposure <- unname(exposure)
  interaction <- unname(interaction)

  checkmate::assert_string(exposure)
  checkmate::assert_string(interaction)
  checkmate::assert_number(ci_level, lower = 0.5, upper = 0.9999)
  checkmate::assert_flag(table)
  # A test chosen by hand is the one that is reported, or an error saying why it
  # could not be; the default one falls back to the Wald test where the
  # likelihood ratio test cannot be taken or would not match the intervals.
  chosen <- !missing(test)
  test <- match.arg(test)
  layout <- fy_as_layout(layout)
  person_time <- fy_person_time_spec(person_time)
  if (identical(exposure, interaction)) {
    stop("`exposure` and `interaction` name the same variable, \"",
         exposure, "\"", call. = FALSE)
  }

  base_info <- fy_model_info(fit, measure = measure)
  fy_check_modifier(base_info, interaction)

  already <- fy_has_interaction(base_info, exposure, interaction)
  fit_int <- fy_add_interaction(fit, base_info, exposure, interaction,
                                already = already)
  info <- fy_model_info(fit_int, measure = measure,
                        exponentiate = exponentiate, vcov = vcov,
                        cluster = cluster, outcome = outcome)

  interaction_columns <- fy_modifier_interaction_columns(info, exposure, interaction)
  if (!length(interaction_columns)) {
    stop(
      "the updated model has no coefficient for the \"", exposure, "\" by \"",
      interaction, "\" interaction, so there is nothing to explore; check that ",
      "both variables vary in the data the model was fitted to",
      call. = FALSE
    )
  }

  modifier_levels <- levels(droplevels(as.factor(info$mf[[interaction]])))
  reference_cell <- fy_reference_cell(reference, info, exposure, interaction,
                                      modifier_levels, contrast = contrast,
                                      at = at)
  values <- fy_exposure_values(info, exposure, contrast = contrast, at = at)
  # A multinomial fit has one effect per comparison between outcome levels, so
  # every subgroup carries that whole set of comparisons rather than one row.
  comparisons <- fy_outcome_comparisons(info, outcome_reference,
                                        outcome_reference_row)

  estimates <- do.call(rbind, lapply(modifier_levels, function(lv) {
    out <- fy_exposure_estimates(
      info, exposure, fy_mark_reference(values, reference_cell, lv),
      ci_level = ci_level, modifier = interaction, modifier_level = lv,
      reference_cell = reference_cell, comparisons = comparisons
    )
    out$modifier_level <- lv
    out
  }))

  tests <- fy_interaction_tests(
    info, interaction_columns, test = test, fit = fit, base_info = base_info,
    exposure = exposure, interaction = interaction, already = already,
    chosen = chosen
  )
  # The first test is the one the figure reports; a figure asked for both
  # carries the Wald test in the column it always had and the likelihood ratio
  # test in one of its own beside it.
  estimates$interaction_p <- tests[[1L]]$p.value
  if (length(tests) > 1L) {
    estimates$interaction_p_lrt <- tests$lrt$p.value
  }
  estimates$modifier <- interaction
  # The blocks of this figure are the subgroups of the modifier, so a
  # multinomial fit names its comparisons between outcome levels on the rows
  # inside each block rather than taking blocks of its own. What the rows were
  # before they were folded together is what the note under the figure is
  # about, so it is taken while the two are still apart.
  by_outcome <- any(!is.na(estimates$outcome_label))
  by_level <- any(!is.na(estimates$level))
  estimates <- fy_fold_outcome_into_level(estimates)
  estimates <- fy_finish_estimates(estimates, labels)
  estimates$modifier_label <- factor(
    fy_apply_labels(estimates$modifier_level, level_labels),
    levels = fy_apply_labels(modifier_levels, level_labels)
  )

  adjusted <- fy_is_adjusted(info, exposure, interaction)
  modifier_label <- fy_apply_labels(interaction, labels)

  # The modifier names the column of its own levels, and the title says which
  # effect is being shown and that it came out of one model carrying the
  # interaction rather than out of a model fitted in each subgroup. A journal
  # prints that in the caption instead, so its styles write no title of their
  # own.
  label_header <- layout$headings$label %||% modifier_label
  # The exposure is named as the rows name it, which is with the comparison
  # behind it -- "no2 (per 10)", "no2 (10 -> 20)" -- since every row of the
  # figure is that comparison taken within another subgroup.
  exposure_label <- fy_and(unique(as.character(estimates$variable_label)))
  title <- fy_resolve_title(title, layout, if (is.null(reference_cell)) {
    paste0(
      fy_effect_phrase(info, adjusted, exposure_label),
      " within each level of ", modifier_label,
      ", from one model containing their interaction term"
    )
  } else {
    # A row of this figure is a combination of the two variables rather than an
    # effect of the exposure, so the title names the combination it is read
    # against: a reader who cannot see which cell is the reference cannot read
    # any of the others.
    paste0(
      fy_axis_label(info, adjusted), " for every combination of ",
      exposure_label, " and ", modifier_label, ", each compared with ",
      exposure_label, " ", reference_cell$exposure_level, " and ",
      modifier_label, " ",
      fy_apply_labels(reference_cell$modifier_level, level_labels),
      ", from one model containing their interaction term"
    )
  })

  plot <- fy_forest_plot(
    estimates,
    exponentiate = info$exponentiate,
    measure_label = fy_resolve_xlab(xlab, fy_axis_label(info, adjusted)),
    estimate_header = fy_estimate_header(info, adjusted, ci_level,
                                         with_outcome = FALSE),
    table = table,
    columns = columns,
    group = "modifier_label",
    title = title,
    subtitle = subtitle,
    layout = layout,
    label_header = label_header,
    person_time = person_time
  )

  out <- fy_new_result(
    plot,
    estimates = estimates,
    infos = list(info),
    exposure = exposure,
    counts_note = fy_counts_note(estimates, by_level = by_level,
                                 by_outcome = by_outcome, table = table,
                                 columns = columns, counts = layout$counts),
    counts_reading = c(by_level = by_level, by_outcome = by_outcome),
    counts_mode = layout$counts,
    modifier = interaction,
    modifier_display = modifier_label,
    reference_cell = reference_cell,
    interaction_test = tests[[1L]],
    interaction_tests = tests,
    measure = info$measure,
    measure_label = info$measure_label,
    exponentiate = info$exponentiate,
    ci_level = ci_level,
    adjusted = adjusted,
    robust = info$robust,
    variance = info$variance,
    person_time = person_time
  )

  file <- fy_report_file(html, exposure, interaction)
  if (!is.null(file)) {
    foresty_report(out, file = file)
  }
  out
}

# The modifier has to have levels. A continuous one would silently be
# contrasted at a single arbitrary value, which is worse than refusing.
#
# Having levels is not the same as being stored as a factor. A numeric modifier
# taking exactly two values in the fitted data has two levels: it entered the
# model through the one coefficient a two-level factor would have given, so the
# contrast taken at each of its values is exact and the joint test of the
# interaction has the same single degree of freedom. A binary coded 0 and 1 --
# which is how a flag usually reaches a regression -- is therefore read as the
# two subgroups it is, rather than sent back to be wrapped in factor() and
# fitted again to no effect.
#
# Three or more values are refused even when they look like codes, because the
# model fitted them as a straight line: there is one interaction coefficient,
# the subgroup estimates would be constrained to equal steps while appearing to
# have been estimated freely, and the test would have one degree of freedom
# where the figure implies more. That has to be a refit the caller asks for.
fy_check_modifier <- function(info, interaction) {
  if (!interaction %in% names(info$mf)) {
    stop(
      "\"", interaction, "\" is not a variable in this model. Its variables are ",
      paste0("\"", names(info$mf), "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  x <- info$mf[[interaction]]
  if (is.matrix(x) && ncol(x) > 1L) {
    stop(
      "\"", interaction, "\" enters this model through a transformation ",
      "rather than as a plain factor, so its levels are not available. Pass ",
      "the variable itself as the modifier and let it enter the formula ",
      "unmodified.",
      call. = FALSE
    )
  }
  values <- if (is.numeric(x)) {
    unique(x[!is.na(x)])
  } else {
    levels(droplevels(as.factor(x)))
  }
  if (length(values) < 2L) {
    stop(
      "\"", interaction, "\" takes only one value in the data this model was ",
      "fitted to, so there are no subgroups to compare",
      call. = FALSE
    )
  }
  if (is.numeric(x) && length(values) > 2L) {
    stop(
      "\"", interaction, "\" is continuous, and foresty does not split a ",
      "continuous modifier for you. Categorize it first, for example ",
      "`data$", interaction, "_grp <- cut(data$", interaction,
      ", breaks = quantile(data$", interaction,
      ", 0:3/3), include.lowest = TRUE)`, refit, and pass the new variable. ",
      "It takes ", length(values), " values in this model; a numeric modifier ",
      "taking two, 0 and 1 for instance, is read as those two subgroups and ",
      "needs no such change.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# The one combination of the exposure and the modifier the whole figure is read
# against, as the two levels it is, or NULL where the figure is the ordinary one
# of an effect within each subgroup.
#
# It is a group only where both variables have levels to make a group out of. A
# continuous exposure has none: its row is a slope rather than a cell of a
# table, and there is no combination of it with anything to compare with another
# combination. That is refused rather than approximated at some value of it,
# since a figure whose rows are the effect of moving along a continuous exposure
# and whose title says they are compared with one group would be read wrongly.
fy_reference_cell <- function(reference, info, exposure, interaction,
                              modifier_levels, contrast = NULL, at = NULL) {
  if (is.null(reference) || isFALSE(reference)) {
    return(NULL)
  }
  x <- fy_variable(info, exposure)
  if (!fy_is_categorical(x)) {
    stop(
      "`reference` names one combination of \"", exposure, "\" and \"",
      interaction, "\", and \"", exposure,
      "\" is continuous, so it has no levels to combine: its rows are the ",
      "effect of a difference in it rather than groups of people. Drop ",
      "`reference` for the effect of \"", exposure,
      "\" within each subgroup, or categorize it with cut() and refit.",
      call. = FALSE
    )
  }
  if (!is.null(at) || !is.null(contrast)) {
    stop(
      "`reference` and ", if (is.null(at)) "`contrast`" else "`at`",
      " both say which comparisons are made, so only one of them can be ",
      "given: `reference` compares every level of \"", exposure,
      "\" in every subgroup with one combination of the two variables.",
      call. = FALSE
    )
  }

  exposure_levels <- levels(droplevels(as.factor(x)))
  wanted <- if (isTRUE(reference)) {
    # The first level of each, which is the cell the model itself takes as its
    # baseline under the treatment contrasts R uses by default, and is where a
    # reader who has not decided yet should start. Under another coding it is
    # the first level rather than the model's baseline; the estimates are the
    # same either way, since each is a difference between two rows of the
    # design, so what this settles is which cell the figure is read against.
    stats::setNames(c(exposure_levels[[1L]], modifier_levels[[1L]]),
                    c(exposure, interaction))
  } else {
    fy_reference_levels(reference, exposure, interaction)
  }

  chosen <- function(variable, given, levels) {
    if (given %in% levels) {
      return(given)
    }
    stop(
      "\"", given, "\" is not a level of \"", variable,
      "\" in the data this model was fitted to. Its levels are ",
      paste0("\"", levels, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  list(
    exposure_level = chosen(exposure, wanted[[exposure]], exposure_levels),
    modifier_level = chosen(interaction, wanted[[interaction]], modifier_levels)
  )
}

# `reference` as the two levels it names. Named for the variables it is about,
# since a bare pair of levels does not say which of them belongs to which
# variable and a figure drawn from the wrong way round would look right.
fy_reference_levels <- function(reference, exposure, interaction) {
  said <- paste0(
    "`reference` names a level of \"", exposure, "\" and a level of \"",
    interaction, "\", as `reference = c(", exposure, " = \"...\", ",
    interaction, " = \"...\")`, or TRUE for the first level of each"
  )
  if (!is.character(reference) && !is.factor(reference) &&
      !is.numeric(reference)) {
    stop(said, "; it is ", paste(class(reference), collapse = "/"), ".",
         call. = FALSE)
  }
  out <- stats::setNames(as.character(reference), names(reference))
  if (length(out) != 2L) {
    stop(said, "; it names ", length(out),
         if (length(out) == 1L) " level." else " levels.", call. = FALSE)
  }
  if (is.null(names(out)) || !setequal(names(out), c(exposure, interaction))) {
    stop(said, ".", call. = FALSE)
  }
  out
}

# Which row of a subgroup is the reference one. Without a reference cell that is
# the exposure's own first level, taken again inside every subgroup, and the
# values carry it already; with one it is a single row of the whole figure, so
# every other subgroup has none.
fy_mark_reference <- function(values, reference_cell, modifier_level) {
  if (is.null(reference_cell)) {
    return(values)
  }
  here <- identical(as.character(modifier_level),
                    as.character(reference_cell$modifier_level))
  lapply(values, function(v) {
    v$reference <- here && identical(as.character(v$level),
                                     as.character(reference_cell$exposure_level))
    v
  })
}

# The tests of the interaction the caller asked for, in the order they are
# reported. The Wald test comes out of the fitted model on its own; the
# likelihood ratio test needs the model without the interaction as well, which
# is the model that was passed in unless it already carried the interaction, in
# which case it has to be taken out again.
#
# `chosen` says whether the caller named the test. The likelihood ratio test is
# the default, and it cannot always be taken -- a GEE and a quasi-likelihood
# family have no likelihood, and a robust variance is no part of one -- so a
# default falls back to the Wald test and says that it did, while a test asked
# for by name is either given or refused.
fy_interaction_tests <- function(info, columns, test, fit, base_info, exposure,
                                 interaction, already, chosen = TRUE) {
  out <- list()
  if (test %in% c("wald", "both")) {
    out$wald <- fy_joint_test(info, columns)
  }

  # The likelihood-based test, taken by `taker` over the model without the
  # interaction, which is the model that was passed in unless it already
  # carried the interaction, in which case it has to be taken out again. A
  # taker that refits that model for itself is not handed one.
  take <- function(taker, with_reduced = TRUE) {
    reduced <- if (!with_reduced) {
      NULL
    } else if (already) {
      fy_drop_interaction(fit, base_info, exposure, interaction)
    } else {
      fit
    }
    lrt <- if (chosen) {
      taker(info, reduced, columns)
    } else {
      try(taker(info, reduced, columns), silent = TRUE)
    }
    if (inherits(lrt, "try-error")) {
      message(
        "The likelihood ratio test could not be taken over this model, so the ",
        "joint Wald test is reported instead: ",
        # The reason ends by suggesting the Wald test, which is what has just
        # been done, so that sentence is left off.
        sub("\\s*[Uu]se `test = \"wald\"`\\.\\s*$", "",
            conditionMessage(attr(lrt, "condition")))
      )
      return(list(wald = out$wald %||% fy_joint_test(info, columns)))
    }
    out$lrt <- lrt
    out
  }

  if (test %in% c("lrt", "both")) {
    # A survey-weighted fit has no likelihood of the data to take the test
    # over, but it has the design's own version of it: the Rao-Scott working
    # likelihood ratio test, which stands where the likelihood ratio test does
    # for every other model. Its variance is design-based from the start, so
    # the robust-variance caveat below does not arise either.
    if (inherits(info$fit, "svyglm")) {
      terms <- names(info$term_vars)[fy_interaction_terms(info, exposure,
                                                          interaction)]
      return(take(
        function(info, reduced, columns) fy_svy_lrt_test(info, terms, columns),
        with_reduced = FALSE
      ))
    }
    # A fit that has no likelihood has no likelihood ratio test, whoever asked
    # for one. A GEE is the case that matters: it is estimating equations rather
    # than a likelihood, so there is nothing to take twice the difference of,
    # and refitting it without the interaction to compare the two would not
    # produce a test either. The Wald test is the test of a GEE, and it is what
    # is reported -- silently in the sense that nothing is dressed up as a
    # likelihood ratio test, and out loud in that foresty says which it gave.
    if (fy_has_no_likelihood(info$fit)) {
      if (identical(test, "lrt") && chosen) {
        stop(
          "no likelihood ratio test can be taken over this model: ",
          fy_no_likelihood_reason(info$fit),
          ". The joint Wald test is the test of it; ask for that one with ",
          "`test = \"wald\"`.",
          call. = FALSE
        )
      }
      message(
        "No likelihood ratio test can be taken over this model, since ",
        fy_no_likelihood_reason(info$fit),
        ", so the joint Wald test is reported instead."
      )
      return(list(wald = out$wald %||% fy_joint_test(info, columns)))
    }
    if (isTRUE(info$robust)) {
      if (!chosen) {
        message(
          "The standard errors are robust, and the likelihood ratio test takes ",
          "no account of them, so the joint Wald test is reported instead. Ask ",
          "for it with `test = \"lrt\"` to have it anyway."
        )
        return(list(wald = out$wald %||% fy_joint_test(info, columns)))
      }
      warning(
        "the likelihood ratio test is computed from the model's likelihood ",
        "and takes no account of the robust standard errors, so it does not ",
        "match the intervals drawn beside it. The Wald test, `test = ",
        "\"wald\"`, is the one that does.",
        call. = FALSE
      )
    }
    return(take(fy_lrt_test))
  }
  out
}

# Whether the fit already interacts the two variables.
fy_has_interaction <- function(info, exposure, interaction) {
  any(fy_interaction_terms(info, exposure, interaction))
}

fy_interaction_terms <- function(info, exposure, interaction) {
  vapply(info$term_vars, function(v) {
    all(c(exposure, interaction) %in% v)
  }, logical(1))
}

# The fit without its exposure by modifier terms, which is what the likelihood
# ratio test compares the interaction model against when the model handed to
# foresty already carried the interaction.
fy_drop_interaction <- function(fit, info, exposure, interaction) {
  terms <- names(info$term_vars)[fy_interaction_terms(info, exposure,
                                                      interaction)]
  dropped <- try(
    fy_refit(fit, info, stats::as.formula(
      paste("~ . -", paste(terms, collapse = " - "))
    )),
    silent = TRUE
  )
  if (inherits(dropped, "try-error")) {
    stop(
      "the model could not be refitted without the term",
      if (length(terms) > 1L) "s " else " ", paste(terms, collapse = " and "),
      ", so a likelihood ratio test of the interaction cannot be taken: ",
      conditionMessage(attr(dropped, "condition")),
      "\nUse `test = \"wald\"`, or fit the model without the interaction and ",
      "let foresty add it.",
      call. = FALSE
    )
  }
  dropped
}

# Adds the interaction unless the fit already carries it.
fy_add_interaction <- function(fit, info, exposure, interaction,
                               already = fy_has_interaction(info, exposure,
                                                            interaction)) {
  if (already) {
    message(
      "The model already contains the ", exposure, " by ", interaction,
      " interaction; using it as it stands rather than updating it."
    )
    return(fit)
  }

  other <- setdiff(fy_interacting_vars(info, exposure), interaction)
  if (length(other)) {
    warning(
      "\"", exposure, "\" is also interacted with ",
      paste0("\"", other, "\"", collapse = " and "),
      " in this model. The subgroup estimates are reported at the first ",
      "complete observation's values of those variables.",
      call. = FALSE
    )
  }

  # The interaction is written with the model's own term labels rather than the
  # bare variable names, so that a variable entered as `rcs(no2, 4)` is
  # interacted as `rcs(no2, 4):sex`. Writing `no2:sex` would add a second,
  # linear copy of the exposure and the fit would fail or mean something else.
  new_term <- paste0(
    fy_main_term_label(info, exposure), ":",
    fy_main_term_label(info, interaction)
  )
  updated <- try(
    fy_refit(fit, info, stats::as.formula(paste("~ . +", new_term))),
    silent = TRUE
  )
  if (inherits(updated, "try-error")) {
    stop(
      "the model could not be updated to add the term ", new_term, ": ",
      conditionMessage(attr(updated, "condition")),
      "\nRefit the model with the interaction included and pass that fit instead.",
      call. = FALSE
    )
  }
  updated
}

# Refits the model with an extra term.
#
# stats::update() cannot be used directly, because it evaluates the call in its
# own caller, which here is inside this package. A model fitted with
# `data = d`, where `d` is a variable local to the user's function or to a
# loop, would then fail to find its own data. The call is instead evaluated
# where the model's formula came from, with the data bound under the name the
# call used, so a model fitted inside a function or a loop can be updated too.
# What it cannot do is update a fit that records no usable call of its own,
# which is why every caller of this takes the failure as a failure and says so.
fy_refit <- function(fit, info, added) {
  call <- stats::update(fit, added, evaluate = FALSE)
  # svyglm() records its call under the bare name of the generic however it
  # was called, so a fit made with survey::svyglm() in a session that never
  # attached survey would fail to find the function here. The namespace is
  # put back.
  if (inherits(fit, "svyglm") && identical(call[[1L]], as.name("svyglm"))) {
    call[[1L]] <- quote(survey::svyglm)
  }
  env <- environment(stats::formula(fit))
  if (is.null(env)) {
    env <- parent.frame()
  }
  if (!is.null(info$data) && !is.null(call$data) && is.name(call$data)) {
    env <- list2env(
      stats::setNames(list(info$data), as.character(call$data)),
      parent = env
    )
  }
  eval(call, envir = fy_bind_lost_args(call, fit, env))
}

# Arguments the call names but nothing can supply any more.
#
# A model fitted inside a loop or an lapply() records the symbol it was written
# with rather than the value it stood for: `geeglm(..., corstr = cs)` fitted
# over the four working correlations keeps `cs`, and the frame `cs` lived in is
# gone by the time the fit reaches foresty, so the refit fails to find it.
# Where the fit itself recorded what that argument came to -- geeglm() keeps
# its `corstr` -- the lost symbol is bound to the fit's own copy for the refit.
#
# Only arguments written as a bare symbol that cannot be evaluated are looked
# up, and only under the name the call gave them, so an argument that still
# resolves keeps whatever it resolves to. The elements a fit stores under the
# name of an argument without holding what was passed are left out: `weights`
# is a glm's working weights rather than its prior ones, `na.action` the rows
# it dropped rather than the function that dropped them, and `model`, `x` and
# `y` the things those flags asked to be kept rather than the flags.
fy_lost_arg_blocklist <- c("formula", "data", "weights", "na.action",
                           "subset", "model", "x", "y")

# Arguments a fit keeps under another name: survey::svyglm() is given a
# `design` and stores it as `survey.design`, the rows it dropped taken out.
fy_lost_arg_aliases <- c(design = "survey.design")

fy_bind_lost_args <- function(call, fit, env) {
  lost <- fy_lost_args(fit, call, env)
  if (!length(lost)) {
    return(env)
  }
  list2env(
    stats::setNames(lapply(lost, function(argument) fit[[argument]]),
                    names(lost)),
    parent = env
  )
}

# Which arguments those are: the symbol each was written with, named, and the
# element of the fit that holds what it came to. The app writes the same pair
# out as a line of R, so that a script carrying an update() of such a fit runs
# where the fit was made.
fy_lost_args <- function(fit, call = stats::getCall(fit), env = NULL) {
  # An S4 fit cannot be subsetted, so it has nothing to be asked for.
  if (isS4(fit) || is.null(call)) {
    return(character(0))
  }
  if (is.null(env)) {
    env <- environment(stats::formula(fit)) %||% parent.frame()
  }
  arguments <- names(call)
  if (is.null(arguments)) {
    return(character(0))
  }
  found <- character(0)
  for (i in seq_along(call)[-1L]) {
    argument <- arguments[[i]]
    written <- call[[i]]
    if (!nzchar(argument) || argument %in% fy_lost_arg_blocklist ||
        !is.name(written)) {
      next
    }
    if (!inherits(try(eval(written, envir = env), silent = TRUE),
                  "try-error")) {
      next
    }
    element <- if (argument %in% names(fy_lost_arg_aliases)) {
      fy_lost_arg_aliases[[argument]]
    } else {
      argument
    }
    if (!is.null(fit[[element]])) {
      found[[as.character(written)]] <- element
    }
  }
  found
}

# How a variable is written in this model's formula: "rcs(no2, 4)" rather than
# "no2" when it was entered as a spline. Falls back to the variable name when
# it has no main effect of its own.
fy_main_term_label <- function(info, variable) {
  main <- fy_exposure_terms(info, variable)$main
  if (length(main) == 1L) main[[1L]] else variable
}

# Coefficients belonging to terms that involve both the exposure and the
# modifier; these are the ones the joint test is taken over.
fy_modifier_interaction_columns <- function(info, exposure, interaction) {
  wanted <- vapply(info$term_vars, function(v) {
    all(c(exposure, interaction) %in% v)
  }, logical(1))
  fy_term_columns(info, names(info$term_vars)[wanted])
}
