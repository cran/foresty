# Assembling the rows that a forest plot is drawn from.
#
# Shared by foresty_main() and foresty_interaction() so that a row means the
# same thing in both, and so that the reference level of a categorical exposure
# is handled once.

fy_exposure_estimates <- function(info, exposure, values, ci_level = 0.95,
                                  modifier = NULL, modifier_level = NULL,
                                  reference_cell = NULL, comparisons = NULL) {
  L <- fy_contrast_matrix(info, exposure, values, modifier = modifier,
                          modifier_level = modifier_level,
                          reference_cell = reference_cell,
                          comparisons = comparisons)
  estimated <- if (is.null(L)) NULL else fy_lincom(info, L, ci_level = ci_level)

  null_value <- if (info$exponentiate) 1 else 0
  position <- 0L

  # A fit of one equation has one comparison to make, which is the exposure
  # against itself elsewhere; a multinomial fit repeats that comparison within
  # each comparison of the outcome, and the rows are taken in the same order
  # the contrast matrix put them in.
  one <- function(v, cmp) {
    counts <- fy_counts(
      info,
      fy_estimate_rows(info, exposure, v, modifier = modifier,
                       modifier_level = modifier_level,
                       # The level of the outcome the others are read against
                       # is one row for every value of the exposure at once, so
                       # the people counted beside it are all of them rather
                       # than the ones at whichever level happens to be drawn
                       # first. A subgroup of a modifier is still a subgroup:
                       # that row belongs to it and is counted within it.
                       whole_exposure = isTRUE(cmp$is_reference)),
      outcome_level = cmp$level
    )

    # The reference level of a categorical exposure, and the level of the
    # outcome a multinomial figure reads its rows against, are both drawn so
    # that the rows around them can be read. Neither is an estimate: each is
    # the definition the others are differences from, so it is the null value
    # of the scale -- 1 for a ratio, 0 for the scale the model was fitted on --
    # and carries no interval, no statistic and no p-value.
    is_definition <- isTRUE(v$reference) || isTRUE(cmp$is_reference)

    # And the people the row's estimate is compared with, so that the figure
    # can put the two side by side rather than leaving the reader to find the
    # other half of the comparison on another row.
    compared <- fy_compared_counts(
      info, exposure, v, cmp, modifier = modifier,
      modifier_level = modifier_level, reference_cell = reference_cell,
      is_definition = is_definition
    )
    if (is_definition) {
      out <- data.frame(
        estimate = null_value, se = NA_real_,
        conf.low = NA_real_, conf.high = NA_real_,
        statistic = NA_real_, p.value = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      position <<- position + 1L
      out <- estimated[position, c("estimate", "se", "conf.low", "conf.high",
                                   "statistic", "p.value"), drop = FALSE]
    }

    data.frame(
      variable = exposure,
      # The reference level of the outcome is one row whatever the exposure
      # is: it is the same definition at every value of it, and three rows all
      # reading 1.00 say nothing three times.
      level = if (isTRUE(cmp$is_reference)) NA_character_ else v$level,
      # What the comparison was, where it is not the plain one unit: "per 10",
      # or "10 -> 20" from `at`. Carried on the estimates so that the figure
      # can say it and so that it survives into tidy().
      contrast_label = v$contrast_label %||% NA_character_,
      reference = is_definition,
      out,
      n = counts$n,
      events = counts$events,
      person_time = counts$person_time,
      # The same two counts for the group the row is compared with, and what
      # that group is: `"exposure"` where the row is one level of a categorical
      # exposure against another, `"outcome"` where it is one level of a
      # multinomial outcome against another, and NA where the row is compared
      # with nothing that is a group of people -- a step along a continuous
      # exposure, and the reference rows, which are the comparison rather than
      # a side of it.
      n_compared = compared$n,
      events_compared = compared$events,
      counts_pair = compared$pair,
      # Which two levels of the outcome the row compares, and how a figure
      # says it. All three are NA for a fit of one equation, whose rows are of
      # the outcome as a whole.
      outcome_level = cmp$level %||% NA_character_,
      outcome_reference = cmp$reference %||% NA_character_,
      outcome_label = cmp$label %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }

  rows <- if (is.null(comparisons)) {
    lapply(values, one, cmp = NULL)
  } else {
    unlist(
      lapply(comparisons, function(cmp) {
        drawn <- if (isTRUE(cmp$is_reference)) values[1L] else values
        lapply(drawn, one, cmp = cmp)
      }),
      recursive = FALSE
    )
  }

  out <- do.call(rbind, rows)

  # Every row of the contrast matrix belongs to exactly one row of the figure,
  # taken in the order the two were built in: the rows skipped here -- the
  # reference level of a categorical exposure, and the level of the outcome the
  # others are read against -- are the rows fy_contrast_matrix() left out. The
  # two orders are settled in different functions, so that they still agree is
  # checked rather than assumed: a mismatch would put an estimate against the
  # wrong label, which is not something a reader of the figure could catch.
  if (position != NROW(estimated)) {
    stop(
      "internal error: the figure has ", position, " estimated row",
      if (position == 1L) "" else "s", " but ", NROW(estimated),
      " were computed for it, so the two could not be matched up. Please ",
      "report this with the model that produced it.",
      call. = FALSE
    )
  }

  rownames(out) <- NULL
  out
}

# The people a row's estimate is compared with, and which of the two axes of
# the figure the comparison runs along.
#
# A row of a forest plot is half of a comparison, and the counts beside it have
# always been of that half alone: a row of 822 suburban children carries an
# odds ratio estimated from those 822 and the 468 rural ones on the reference
# row, and the 468 appear nowhere near the estimate they are half of. This is
# the other half.
#
# Which group that is follows what the row compares:
#
# * A categorical exposure compares one of its levels with the level the
#   contrast is from, within the same subgroup: Urban against Rural.
# * `reference` on foresty_interaction() compares every cell with one named
#   cell of the two variables, which is a group in another subgroup.
# * A multinomial fit of an exposure with no levels compares one level of the
#   outcome with the level the equations were fitted against, over the same
#   people: Transient against None.
#
# Nothing else has a second group to report. A step along a continuous exposure
# is not two groups of people, and neither is a contrast between two values of
# one; the reference rows are the group everything else is compared with rather
# than a row with a comparison of its own.
fy_compared_counts <- function(info, exposure, v, cmp, modifier = NULL,
                               modifier_level = NULL, reference_cell = NULL,
                               is_definition = FALSE) {
  none <- list(n = NA_integer_, events = NA_integer_, pair = NA_character_)
  if (isTRUE(is_definition)) {
    return(none)
  }

  along_exposure <- function(level, subgroup) {
    counts <- fy_counts(
      info,
      fy_rows_at(info, exposure, level, modifier, subgroup),
      outcome_level = cmp$level
    )
    list(n = counts$n, events = counts$events, pair = "exposure")
  }

  if (!is.null(reference_cell)) {
    return(along_exposure(reference_cell$exposure_level,
                          reference_cell$modifier_level))
  }

  x <- info$mf[[exposure]]
  if (!is.null(x) && fy_is_categorical(x) && !is.na(v$level)) {
    return(along_exposure(v$from, modifier_level))
  }

  if (!is.null(cmp) && !is.null(cmp$reference)) {
    counts <- fy_counts(
      info,
      fy_estimate_rows(info, exposure, v, modifier = modifier,
                       modifier_level = modifier_level),
      outcome_level = cmp$reference
    )
    return(list(n = counts$n, events = counts$events, pair = "outcome"))
  }

  none
}

# The comparison between outcome levels written into the column of levels, for
# a figure whose blocks are already spoken for -- one per subgroup of the
# modifier -- and which therefore has to name the outcome comparison on the
# rows. Where the exposure has levels of its own, the row carries both.
fy_fold_outcome_into_level <- function(estimates) {
  outcome <- as.character(estimates$outcome_label)
  if (!any(!is.na(outcome))) {
    return(estimates)
  }
  level <- as.character(estimates$level)
  estimates$level <- ifelse(is.na(level), outcome, paste0(level, ", ", outcome))
  estimates
}

# The outcome comparisons as a factor in the order they are drawn, so that the
# blocks of a figure follow the order of the outcome's own levels.
fy_outcome_label_factor <- function(estimates) {
  outcome <- as.character(estimates$outcome_label)
  factor(outcome, levels = unique(outcome[!is.na(outcome)]))
}

# The label each row is drawn against.
#
# A categorical exposure is labelled by its level alone, with the variable it
# belongs to shown once at the left of the figure rather than repeated on every
# row. A continuous exposure has no level, so it takes the variable's label.
#
# A comparison that is not the plain one unit says so beside the variable --
# "NO2 (per 10)", "NO2 (20 -> 10)" -- and says it wherever the variable is
# named, the title included, since the estimate means nothing without it. It
# belongs to the exposure rather than to the row because every row of the
# figure is that same comparison, taken within another subgroup. It is written
# whatever the variable is called, a label being where the unit goes rather
# than where the comparison does.
fy_row_labels <- function(estimates, labels = NULL) {
  variable_label <- fy_apply_labels(estimates$variable, labels)
  said <- as.character(estimates$contrast_label %||%
                         rep(NA_character_, nrow(estimates)))
  says <- !is.na(said)
  variable_label[says] <- paste0(variable_label[says], " (", said[says], ")")
  row_label <- ifelse(is.na(estimates$level), variable_label, estimates$level)
  list(variable_label = variable_label, row_label = row_label)
}

# The exposure as a figure of it names it: the variable, and the comparison
# where that is not the plain one unit -- "no2 (per 10)", "no2 (per IQR, 8.44)",
# "no2 (10 -> 20)".
#
# It is the label the rows are drawn against, taken before anything is drawn,
# so that a caller wanting to say which exposure a figure is of says it the
# same way the figure does rather than writing its own version.
fy_exposure_display <- function(info, exposure, contrast = NULL, at = NULL,
                                labels = NULL) {
  values <- fy_exposure_values(info, exposure, contrast = contrast, at = at)
  estimates <- data.frame(
    variable = exposure,
    level = NA_character_,
    contrast_label = values[[1L]]$contrast_label %||% NA_character_,
    stringsAsFactors = FALSE
  )
  fy_row_labels(estimates, labels)$variable_label[[1L]]
}

# The one combination every row of the figure is compared with, said in the two
# variables it is a combination of, so that it can be read without the figure
# beside it: "urbanicity = Rural and sex = Female".
fy_reference_phrase <- function(result) {
  cell <- result$reference_cell
  if (is.null(cell)) {
    return(NULL)
  }
  paste0(result$exposure, " = ", cell$exposure_level, " and ",
         result$modifier, " = ", cell$modifier_level)
}

# 10 rather than 10.0, 0.5 rather than 0.500, and 1000000 rather than 1e+06:
# these numbers are written on a figure and into the sentences that explain it,
# where an exponent is read as a mistake rather than as a number.
fy_trim_number <- function(x) {
  format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
}

# A label written where the variable is named: `interaction = c(Sex = "sex")`
# says both which variable is meant and what to call it, which saves naming it
# twice. The name written beside the variable is the more specific of the two,
# so it wins over one given in `labels`.
fy_absorb_labels <- function(x, labels) {
  named <- names(x)
  if (is.null(named) || !any(nzchar(named))) {
    return(labels)
  }
  given <- nzchar(named)
  here <- stats::setNames(named[given], as.character(x)[given])
  if (is.null(labels)) {
    return(here)
  }
  checkmate::assert_character(labels, names = "named")
  c(labels[setdiff(names(labels), names(here))], here)
}

fy_apply_labels <- function(x, labels) {
  out <- as.character(x)
  if (is.null(labels)) {
    return(out)
  }
  checkmate::assert_character(labels, names = "named")
  hit <- match(out, names(labels))
  out[!is.na(hit)] <- unname(labels[hit[!is.na(hit)]])
  out
}

# Whether anything beyond the exposure and the modifier is in the model, which
# is what makes an estimate an adjusted one.
fy_is_adjusted <- function(info, exposure, modifier = NULL) {
  variables <- unique(unlist(info$term_vars, use.names = FALSE))
  length(setdiff(variables, c(exposure, modifier))) > 0L
}

# How the estimate column is headed: "Adjusted odds ratio (95% CI)" and the
# like, naming the measure the model actually produced.
#
# `with_outcome = FALSE` leaves the outcome out, which is what the column
# beside the plot does: the axis under the plot names it, and a column of a
# table is as narrow as it can be made.
fy_estimate_header <- function(info, adjusted, ci_level, with_ci = TRUE,
                               with_outcome = TRUE) {
  label <- if (with_outcome) info$measure_label else info$measure_name
  if (adjusted) {
    label <- paste0("Adjusted ", tolower(substring(label, 1, 1)),
                    substring(label, 2))
  }
  if (!with_ci) {
    return(label)
  }
  paste0(label, " (", round(ci_level * 100), "% CI)")
}

# What is written under the plot: "Adjusted odds ratio for asthma". The axis is
# the one place every figure has, so it is where the outcome is named, and a
# reader who sees only the plot still knows what the ratio is a ratio of.
fy_axis_label <- function(info, adjusted) {
  fy_estimate_header(info, adjusted, ci_level = NULL, with_ci = FALSE,
                     with_outcome = TRUE)
}

# "Adjusted odds ratio for asthma associated with NO2", the phrase every title
# is built out of. The outcome follows the measure, since that is what the
# measure is a measure of; the exposure follows the outcome.
fy_effect_phrase <- function(info, adjusted, exposures) {
  paste0(fy_axis_label(info, adjusted), " associated with ", exposures)
}

# The label under the plot, where the caller has named one.
#
# `NULL` is the one the model wrote for itself, which names the measure and the
# outcome. A string is drawn as it was given, and `NA` draws none, for a figure
# whose caption carries that instead.
fy_resolve_xlab <- function(xlab, default) {
  if (is.null(xlab)) {
    return(default)
  }
  if (length(xlab) == 1L && is.na(xlab)) {
    return(NULL)
  }
  checkmate::assert_string(xlab)
  xlab
}

# The outcome renamed on a model description that has already been built, which
# is what foresty_combine() needs: its figures were drawn one at a time and it
# is relabelling what they came back with rather than fitting anything.
fy_relabel_outcome <- function(info, outcome) {
  if (is.null(outcome)) {
    return(info)
  }
  spec <- fy_apply_outcome(
    list(name = info$measure_name, outcome = info$outcome), outcome
  )
  info$outcome <- spec$outcome
  info$measure_label <- spec$label
  info
}

# Person-time -----------------------------------------------------------------

# The unit person-time is reported in.
#
# `NULL` is the total the model carries, which is what a figure of a few hundred
# person-years wants. A number is a unit to divide by -- `person_time = 1000`
# for a column counting thousands of person-years -- and the column says which,
# since a count of person-time that does not say what it counts cannot be read
# against another study's. Naming the number names the column outright:
# `person_time = c("Person-years (per 1,000)" = 1000)`.
fy_person_time_spec <- function(person_time) {
  if (is.null(person_time)) {
    return(NULL)
  }
  # Already resolved: foresty_combine() takes the unit its figures were drawn
  # with rather than being told it again.
  if (is.list(person_time) && !is.null(person_time$unit)) {
    return(person_time)
  }
  if (!is.numeric(person_time) || length(person_time) != 1L ||
      is.na(person_time) || !is.finite(person_time) || person_time <= 0) {
    stop(
      "`person_time` is the unit person-time is reported in, as ",
      "`person_time = 1000` for a column counting thousands of person-years. ",
      "It must be one positive number, optionally named to head the column: ",
      "`person_time = c(\"Person-years (per 1,000)\" = 1000)`.",
      call. = FALSE
    )
  }
  heading <- names(person_time)
  list(
    unit = as.numeric(person_time),
    heading = if (!is.null(heading) && nzchar(heading[[1L]])) heading[[1L]]
  )
}

# "Person-time" becomes "Person-time (per 1,000)", on a line of its own so that
# the column stays as narrow as the numbers in it.
fy_person_time_heading <- function(base, spec, sep = "\n") {
  if (is.null(spec)) {
    return(base)
  }
  if (!is.null(spec$heading)) {
    return(spec$heading)
  }
  if (isTRUE(all.equal(spec$unit, 1))) {
    return(base)
  }
  paste0(base, sep, "(per ", fy_plain_number(spec$unit), ")")
}

# 1000 written as 1,000, and never as 1e+03.
fy_plain_number <- function(x) {
  format(x, big.mark = ",", trim = TRUE, scientific = FALSE,
         drop0trailing = TRUE)
}
