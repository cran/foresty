#' Combine several foresty figures into one forest plot
#'
#' Draws the rows of figures already made -- an overall effect from
#' [foresty_main()], the same effect within the levels of sex from
#' [foresty_interaction()], within the levels of ethnicity from another -- as
#' one figure, each of them a block of rows under a heading of its own. This is
#' the subgroup panel a paper prints: the overall estimate at the top, the
#' subgroup analyses under it, and the interaction p-value beside each block.
#'
#' Nothing is refitted and nothing is re-estimated. Each figure keeps the
#' estimates it was drawn from, so a block means exactly what it meant on its
#' own figure, and the estimates behind the combined one are still reached with
#' [summary()], [tidy()] and [as.data.frame()].
#'
#' The figures have to agree on what they are measuring: the same effect
#' measure and the same confidence level, since the rows are read against one
#' axis. They do not have to come from the same model, and usually will not,
#' each subgroup analysis being its own interaction model.
#'
#' @section More than one exposure:
#'
#' Every row of a combined figure is read against the rows above it, and rows
#' reporting the effect of different exposures are not comparable that way,
#' however alike their axes happen to be. So the figures handed in are sorted
#' by the exposure they report and one figure is drawn for each: NO2 overall
#' and within every subgroup on the first, black carbon overall and within
#' every subgroup on the second, each titled with its own exposure.
#'
#' A call covering one exposure -- which is the usual one -- returns that
#' figure. A call covering several returns the figures in a list, named for the
#' exposures, which draws them one after another when it is printed and holds
#' `foresty` figures that are used singly as any other is:
#'
#' ```r
#' figures <- foresty_combine(Overall = overall, Sex = by_sex)
#' figures                      # draws each in turn
#' figures[["NO2"]]             # one of them
#' ggplot2::ggsave("no2.png", figures[["NO2"]])
#' ```
#'
#' @section Naming the blocks:
#'
#' A named argument names its block. An unnamed one is named for itself: a
#' [foresty_interaction()] figure by its modifier, a [foresty_main()] figure
#' `"Overall"`.
#'
#' A block holding a single row -- the overall estimate, most often -- is drawn
#' as one row carrying the block's name, rather than as a heading with a single
#' row indented under it.
#'
#' @section Singling out the overall estimate:
#'
#' The overall estimate is not one of the subgroups; it is what the subgroups
#' are read against, and a figure that draws it as another row of the same kind
#' invites a reader to compare it with them as though it were one. It is
#' therefore drawn apart from them: a filled diamond on its interval, half again
#' the size of the squares under it, its label in bold, and a rule between it
#' and the subgroups, drawn whether or not the style rules between subgroups.
#' The interval is a plain line, as every other interval on the figure is, so
#' the whole figure is read the same way and the diamond says which row is the
#' summary.
#'
#' `emphasize` says which blocks are drawn that way. `"auto"`, the default,
#' takes the blocks that came from [foresty_main()], which are the ones with no
#' modifier behind them; a figure of nothing but those has none singled out,
#' since every row would be. Name blocks to choose them yourself, as
#' `emphasize = c("Overall", "Pooled")`, and pass `NULL` to draw every block
#' alike. How they are drawn is set in [foresty_layout()], through
#' `emphasis_shape`, `emphasis_height`, `emphasis_face` and `emphasis_gap`.
#'
#' @inheritSection foresty_main What the counts beside the rows count
#' @inheritSection foresty_main Adjusting the figure
#'
#' @param ... Figures returned by [foresty_main()] or [foresty_interaction()],
#'   in the order they are to be drawn, optionally named.
#' @param emphasize Which blocks are drawn as the figure's summary rather than
#'   as another subgroup: `"auto"`, the default, takes the overall estimates
#'   that are a single row; `TRUE` takes every overall estimate, a block of
#'   several rows included; a character vector names blocks by the names they
#'   are drawn under; and `NULL` or `FALSE` draws every block alike. See
#'   *Singling out the overall estimate*.
#' @inheritParams foresty_main
#' @param title Plot title. The default names the measure, the outcome it is a
#'   measure of and the exposure the figure reports, as `"Adjusted odds ratio
#'   for asthma associated with NO2, overall and within each subgroup"`. The
#'   exposure is named once however many blocks it was drawn under, and with
#'   the comparison behind it where that is not the plain one unit --
#'   `contrast = 10`, or two values named by `at` -- since the estimates cannot
#'   be read without it. `NA` draws none, and the journal styles draw none. A
#'   title given by hand is drawn on every figure of a call covering more than
#'   one exposure, which is a reason to leave it to the default there.
#' @param subtitle Plot subtitle. `NULL`, the default, draws none.
#' @param person_time The unit person-time is reported in. `NULL`, the default,
#'   takes whatever the figures being combined were drawn with, since that is a
#'   decision already made; a number, or a named number, overrides them all. See
#'   [foresty_main()].
#'
#' @return A `ggplot2` object, of class `foresty`, carrying every estimate
#'   drawn on it -- or, where the figures combined report more than one
#'   exposure, a list of one such object per exposure, named for them and of
#'   class `foresty_figures`. See *More than one exposure*.
#'
#' @seealso [foresty_main()], [foresty_interaction()], [foresty_layout()].
#'
#' @examples
#' fit <- glm(asthma ~ no2 + sex + maternal_smoking + maternal_age,
#'            family = binomial, data = foresty_cohort)
#'
#' overall <- foresty_main(list(fit), exposure = "no2",
#'                         labels = c(no2 = "NO2"))
#' by_sex <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
#' by_smoking <- foresty_interaction(fit, exposure = "no2",
#'                                   interaction = "maternal_smoking")
#'
#' foresty_combine(Overall = overall, Sex = by_sex,
#'                 `Maternal smoking` = by_smoking)
#'
#' # The rest of the ways the blocks can be combined, drawn one after
#' # another. Each is quick; there are simply several of them.
#' \donttest{
#' # In the layout of a journal, the interaction p-value written once against
#' # each block, and without the numbers beside the plot.
#' foresty_combine(overall, by_sex, by_smoking, layout = "jama")
#' foresty_combine(overall, by_sex, by_smoking, table = FALSE)
#'
#' # Every block drawn alike, the overall estimate included.
#' foresty_combine(Overall = overall, Sex = by_sex, emphasize = NULL)
#'
#' # Two exposures are two figures, one apiece, named for them.
#' fit_bc <- glm(asthma ~ black_carbon + sex + maternal_smoking + maternal_age,
#'               family = binomial, data = foresty_cohort)
#' figures <- foresty_combine(
#'   Overall = foresty_main(list(fit, fit_bc),
#'                          exposure = c(NO2 = "no2",
#'                                       `Black carbon` = "black_carbon")),
#'   Sex = by_sex,
#'   `Black carbon by sex` = foresty_interaction(fit_bc, "black_carbon", "sex")
#' )
#' names(figures)
#' figures[["NO2"]]
#' }
#'
#' @export
foresty_combine <- function(...,
                            emphasize = "auto",
                            outcome = NULL,
                            table = TRUE,
                            columns = NULL,
                            person_time = NULL,
                            layout = NULL,
                            title = NULL,
                            subtitle = NULL,
                            xlab = NULL) {
  parts <- list(...)
  checkmate::assert_flag(table)
  layout <- fy_as_layout(layout)

  if (!length(parts)) {
    stop(
      "foresty_combine() needs at least one figure to combine, as ",
      "`foresty_combine(Overall = overall, Sex = by_sex)`",
      call. = FALSE
    )
  }
  ok <- vapply(parts, inherits, logical(1), what = "foresty")
  if (!all(ok)) {
    first <- which(!ok)[1L]
    stop(
      "every argument must be a figure returned by foresty_main() or ",
      "foresty_interaction(); argument ", first, " is of class ",
      paste(class(parts[[first]]), collapse = "/"), ".",
      call. = FALSE
    )
  }

  results <- lapply(parts, fy_result)
  fy_check_combinable(results)
  blocks <- fy_block_names(parts, results)

  # The unit person-time is reported in belongs to the figures that were drawn,
  # not to the act of putting them side by side, so it is carried over from them
  # unless this call says otherwise. Nothing here has to agree with anything:
  # the column is a count, and a combined figure writes every one of its rows in
  # one unit whichever figure the row came off.
  person_time <- fy_person_time_spec(
    person_time %||% fy_first_person_time(results)
  )

  emphasized <- fy_emphasized_blocks(emphasize, results, blocks)
  estimates <- fy_combined_estimates(results, blocks)
  estimates$emphasis <- estimates$block %in% emphasized

  infos <- unlist(lapply(parts, fy_infos), recursive = FALSE)
  # Figures of different outcomes are a figure of no one outcome, and the axis
  # says so rather than naming whichever came first: "Odds ratio" over a set of
  # blocks that are the outcomes, rather than "Odds ratio for asthma" over rows
  # half of which are about something else. Naming one through `outcome` names
  # it whatever the figures were of.
  outcomes <- unique(vapply(infos, function(i) i$outcome %||% NA_character_,
                            character(1)))
  if (is.null(outcome) && length(outcomes) > 1L) {
    outcome <- NA
  }
  # The outcome is renamed on the descriptions the figures came back with; there
  # is nothing to refit, and the axis and the title of the combined figure are
  # written from them.
  infos <- lapply(infos, fy_relabel_outcome, outcome = outcome)
  # Which exposure each of those models was drawn for, so that a figure of one
  # exposure carries the models behind it and not the models behind the others.
  info_exposure <- unlist(lapply(results, function(r) {
    rep_len(as.character(r$exposure), length(r$infos))
  }))
  # The blocks that are an overall estimate rather than a set of subgroups.
  # What is drawn from them -- whether the column of row labels is headed
  # "Subgroup", how the title ends -- is settled figure by figure, since one
  # exposure may carry an overall estimate where another does not.
  overall_blocks <- blocks[vapply(results, function(r) is.null(r$modifier),
                                  logical(1))]

  common <- list(
    blocks = blocks,
    overall_blocks = overall_blocks,
    measure = results[[1L]]$measure,
    measure_label = if (is.null(outcome)) {
      results[[1L]]$measure_label
    } else {
      infos[[1L]]$measure_label
    },
    exponentiate = isTRUE(results[[1L]]$exponentiate),
    ci_level = results[[1L]]$ci_level,
    adjusted = all(vapply(results, function(r) isTRUE(r$adjusted), logical(1))),
    robust = any(vapply(results, function(r) isTRUE(r$robust), logical(1))),
    # Where the standard errors came from, where every figure says the same;
    # figures that disagree leave it to the robust flag above.
    variance = fy_shared_value(lapply(results, function(r) r$variance)),
    # How the rows of the figures being combined read, so that the combined
    # figure can say the same about its counts as they did. The sentences
    # themselves are written again from this figure's own rows and layout
    # rather than carried over, since it may draw different columns or hold
    # the counts differently from any of them.
    counts_reading = fy_combined_counts_reading(results),
    table = table,
    columns = columns,
    person_time = person_time,
    layout = layout,
    title = title,
    subtitle = subtitle,
    xlab = xlab
  )

  # Two exposures are two figures. They are not two blocks of one: every row of
  # a combined figure is read against the rows above it, and rows reporting the
  # effect of different exposures are not comparable that way, however alike
  # their axes happen to be.
  variables <- unique(as.character(estimates$variable))
  figures <- lapply(variables, function(v) {
    rows <- as.character(estimates$variable) == v
    do.call(fy_combine_figure, c(
      list(
        estimates = estimates[rows, , drop = FALSE],
        exposure = v,
        infos = if (any(info_exposure == v)) infos[info_exposure == v] else infos
      ),
      common
    ))
  })

  if (length(figures) == 1L) {
    return(figures[[1L]])
  }
  # Named for what each of them is of, so that the list reads as the figures it
  # holds.
  names(figures) <- vapply(variables, function(v) {
    as.character(estimates$variable_label[
      as.character(estimates$variable) == v][1L])
  }, character(1))
  structure(figures, class = c("foresty_figures", "list"))
}

# One figure, of one exposure. The rows handed here are the rows of the
# combined estimates that belong to it, so the factors that order the figure --
# the blocks it still carries, the labels down its side -- are taken again over
# those rows rather than over all of them.
fy_combine_figure <- function(estimates, exposure, infos, blocks,
                              overall_blocks, measure, measure_label,
                              exponentiate, ci_level, adjusted, robust,
                              variance, table, columns, person_time, layout,
                              title,
                              subtitle, xlab,
                              counts_reading = c(by_level = FALSE,
                                                 by_outcome = FALSE)) {
  estimates$block_label <- factor(estimates$block,
                                  levels = blocks[blocks %in% estimates$block])
  estimates$variable_label <- factor(
    as.character(estimates$variable_label),
    levels = unique(as.character(estimates$variable_label))
  )
  estimates$label <- factor(as.character(estimates$label),
                            levels = rev(unique(as.character(estimates$label))))
  # A column no row of this figure supplies says nothing, and an empty column in
  # the table beside the plot takes width from the plot. The modifier and its
  # levels go together: one without the other says nothing either.
  for (column in c("interaction_p", "interaction_p_lrt")) {
    if (!is.null(estimates[[column]]) && all(is.na(estimates[[column]]))) {
      estimates[[column]] <- NULL
    }
  }
  if (!is.null(estimates$modifier_level) && all(is.na(estimates$modifier_level))) {
    estimates$modifier_level <- NULL
    estimates$modifier <- NULL
  }
  rownames(estimates) <- NULL

  info <- infos[[1L]]
  overall <- any(estimates$block %in% overall_blocks)
  # "Subgroup" is right when every block is one, and wrong the moment the
  # figure carries an overall estimate: the first row under that heading would
  # then be "Overall", which is not a subgroup but the row the subgroups are
  # read against. A figure carrying one is left unheaded there, as a paper
  # leaves it.
  label_header <- layout$headings$label %||% (if (overall) NULL else "Subgroup")
  title <- fy_resolve_title(
    title, layout,
    fy_combined_title(info, adjusted, estimates, overall)
  )

  plot <- fy_forest_plot(
    estimates,
    exponentiate = exponentiate,
    measure_label = fy_resolve_xlab(xlab, fy_axis_label(info, adjusted)),
    estimate_header = fy_estimate_header(info, adjusted, ci_level,
                                         with_outcome = FALSE),
    table = table,
    columns = columns,
    group = "block_label",
    title = title,
    subtitle = subtitle,
    layout = layout,
    label_header = label_header,
    fold_singletons = TRUE,
    person_time = person_time
  )

  fy_new_result(
    plot,
    estimates = estimates,
    infos = infos,
    exposure = exposure,
    blocks = levels(estimates$block_label),
    counts_note = fy_counts_note(
      estimates,
      by_level = isTRUE(counts_reading[["by_level"]]),
      by_outcome = isTRUE(counts_reading[["by_outcome"]]),
      table = table, columns = columns, counts = layout$counts
    ),
    counts_reading = counts_reading,
    counts_mode = layout$counts,
    measure = measure,
    measure_label = measure_label,
    exponentiate = exponentiate,
    ci_level = ci_level,
    adjusted = adjusted,
    robust = robust,
    variance = variance,
    person_time = person_time
  )
}

# One value the figures agree on, or NULL where they do not or none has it.
fy_shared_value <- function(values) {
  values <- unique(unlist(values, use.names = FALSE))
  if (length(values) == 1L) values else NULL
}

# Whether the rows of the figures being combined read the way a categorical
# exposure's rows do, or the way a multinomial fit's do, so that a report of
# the combined figure can say the same about its counts as a report of any of
# them would have.
fy_combined_counts_reading <- function(results) {
  flags <- c(by_level = FALSE, by_outcome = FALSE)
  for (result in results) {
    reading <- result$counts_reading
    if (is.null(reading)) {
      next
    }
    flags <- flags | reading[names(flags)]
  }
  # `|` over a vector carrying an NA -- a figure drawn by foresty_data(), which
  # has no reading of its own to report -- would spread it.
  flags[is.na(flags)] <- FALSE
  flags
}

# The unit the figures being combined were drawn in, or NULL where none of them
# named one. The first that did settles it: the rows of one figure are read
# against one another and cannot each carry a unit of their own.
fy_first_person_time <- function(results) {
  for (result in results) {
    if (!is.null(result$person_time)) {
      return(result$person_time)
    }
  }
  NULL
}

#' @export
print.foresty_figures <- function(x, ...) {
  for (figure in x) {
    print(figure)
  }
  invisible(x)
}

# Which blocks are drawn as the figure's own summary: a diamond rather than a
# square, the label in bold, and a rule between them and the subgroups.
#
# "auto" is the overall estimate, which is the block that came from
# foresty_main() and has no modifier behind it. A figure that is nothing but
# overall estimates has none to single out, every row being one.
#
# It is also only the block holding a single row. Emphasis says "this one row
# is what the rows below it are being read against", which is what an overall
# estimate is; a block of several rows -- the levels of a categorical exposure,
# or the comparisons between the levels of a multinomial outcome -- is not one
# summary but several estimates that happen to be unstratified, and drawing
# every one of them in bold with a diamond says the wrong thing about all of
# them. Such a block is still named and still sits above the subgroups; it is
# only not dressed as their summary. `emphasize = TRUE` or naming the block
# asks for it anyway.
fy_emphasized_blocks <- function(emphasize, results, blocks) {
  if (is.null(emphasize) || isFALSE(emphasize)) {
    return(character(0))
  }
  overall <- vapply(results, function(r) is.null(r$modifier), logical(1))
  if (identical(emphasize, "auto")) {
    if (all(overall)) {
      return(character(0))
    }
    single <- vapply(results, function(r) nrow(r$estimates) == 1L, logical(1))
    return(blocks[overall & single])
  }
  if (isTRUE(emphasize)) {
    return(blocks[overall])
  }
  checkmate::assert_character(emphasize, any.missing = FALSE)
  unknown <- setdiff(emphasize, blocks)
  if (length(unknown)) {
    stop(
      "`emphasize` names a block this figure does not carry: ",
      paste0("\"", unknown, "\"", collapse = ", "),
      ". Its blocks are ", paste0("\"", blocks, "\"", collapse = ", "),
      ". Use `emphasize = NULL` to single out none of them.",
      call. = FALSE
    )
  }
  emphasize
}

# The rows are read against one axis, so they have to be the same kind of
# number measured to the same width.
fy_check_combinable <- function(results) {
  measures <- vapply(results, function(r) r$measure, character(1))
  if (length(unique(measures)) > 1L) {
    stop(
      "the figures report different effect measures (",
      paste(unique(measures), collapse = ", "),
      "), so they cannot be drawn on one axis",
      call. = FALSE
    )
  }
  scales <- vapply(results, function(r) isTRUE(r$exponentiate), logical(1))
  if (length(unique(scales)) > 1L) {
    stop(
      "some of the figures report a ratio and others its logarithm, so they ",
      "cannot be drawn on one axis; redraw them with the same `exponentiate`",
      call. = FALSE
    )
  }
  levels <- vapply(results, function(r) r$ci_level, numeric(1))
  if (length(unique(levels)) > 1L) {
    stop(
      "the figures were drawn at different confidence levels (",
      paste(paste0(round(unique(levels) * 100), "%"), collapse = ", "),
      "), so their intervals are not comparable; redraw them at one level",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# What each block is called: the name it was passed under, or the thing it is
# of. Two blocks with the same name would be drawn as one, so a repeat is
# numbered.
fy_block_names <- function(parts, results) {
  given <- names(parts)
  if (is.null(given)) {
    given <- rep("", length(parts))
  }
  out <- vapply(seq_along(results), function(i) {
    if (nzchar(given[i])) {
      return(given[i])
    }
    result <- results[[i]]
    if (is.null(result$modifier)) {
      "Overall"
    } else {
      result$modifier_display %||% result$modifier
    }
  }, character(1))
  make.unique(out, sep = " ")
}

# One row per estimate, from every figure, with the block it belongs to and the
# label it is drawn against.
#
# A subgroup row is labelled by the level of the modifier it belongs to, and a
# row from foresty_main() by its own level or, for a continuous exposure that
# has none, by the exposure. A categorical exposure crossed with a modifier is
# two things at once, so it is labelled with both.
fy_combined_estimates <- function(results, blocks) {
  pieces <- Map(function(result, block) {
    est <- result$estimates
    subgroup <- if (is.null(est$modifier_label)) {
      NULL
    } else {
      as.character(est$modifier_label)
    }
    row_label <- if (is.null(subgroup)) {
      ifelse(is.na(est$level), as.character(est$variable_label), est$level)
    } else {
      ifelse(is.na(est$level), subgroup, paste0(subgroup, ": ", est$level))
    }

    data.frame(
      block = block,
      variable = est$variable,
      variable_label = as.character(est$variable_label),
      # Held to the one type each column is, whatever the figure it came off
      # stored it as, so that rbind() binds them rather than coercing a column
      # to whichever type the first figure happened to use.
      level = as.character(est$level),
      contrast_label = as.character(est$contrast_label %||% NA_character_),
      modifier = result$modifier %||% NA_character_,
      modifier_level = as.character(est$modifier_level %||% NA_character_),
      row_label = row_label,
      reference = as.logical(est$reference),
      estimate = est$estimate,
      se = est$se,
      conf.low = est$conf.low,
      conf.high = est$conf.high,
      statistic = est$statistic,
      p.value = est$p.value,
      n = est$n,
      events = as.numeric(est$events),
      person_time = as.numeric(est$person_time),
      # The group each row is compared with travels with the row, so that a
      # combined figure puts the two counts side by side where the figures it
      # was made from did.
      n_compared = as.numeric(est$n_compared %||% NA_real_),
      events_compared = as.numeric(est$events_compared %||% NA_real_),
      counts_pair = as.character(est$counts_pair %||% NA_character_),
      interaction_p = est$interaction_p %||% NA_real_,
      interaction_p_lrt = est$interaction_p_lrt %||% NA_real_,
      # A color a row was given travels with the row, so that a figure drawn
      # from a table of colored rows is still colored once it is combined with
      # another.
      color_group = as.character(est$color_group %||% NA_character_),
      stringsAsFactors = FALSE
    )
  }, results, blocks)

  out <- do.call(rbind, pieces)
  out$block_label <- factor(out$block, levels = blocks)
  out$variable_label <- factor(out$variable_label,
                               levels = unique(out$variable_label))
  out$label <- factor(out$row_label, levels = rev(unique(out$row_label)))

  # A column that no figure supplied says nothing, and an empty column in the
  # table beside the plot takes width from the plot.
  if (all(is.na(out$interaction_p))) {
    out$interaction_p <- NULL
  }
  if (all(is.na(out$interaction_p_lrt))) {
    out$interaction_p_lrt <- NULL
  }
  # A column of colors no figure supplied would tip the combined figure into
  # drawing every row from a column holding nothing.
  if (all(is.na(out$color_group))) {
    out$color_group <- NULL
  }
  if (all(is.na(out$modifier_level))) {
    out$modifier_level <- NULL
    out$modifier <- NULL
  }
  rownames(out) <- NULL
  out
}

# "Adjusted odds ratio for asthma associated with NO2, overall and within each
# subgroup".
#
# The exposure is named, as it is on every other figure the package draws: the
# rows of a combined figure are the names of the subgroups, so the exposure is
# named nowhere else on it, and an odds ratio per 10 ug/m3 of NO2 read as
# though it were per 1 unit of something else is wrong rather than vague.
fy_combined_title <- function(info, adjusted, estimates, overall) {
  paste0(
    fy_effect_phrase(info, adjusted, fy_combined_exposures(estimates)), ", ",
    if (overall) "overall and within each subgroup" else "within each subgroup"
  )
}

# The exposures the figure reports, one label apiece.
#
# The blocks come from figures that were labelled one at a time, so a figure
# whose overall block called the exposure "NO2" and whose subgroup blocks
# called it "no2" would otherwise be titled "associated with NO2 and no2". The
# label a variable is named by is the first one it was drawn under, and a label
# two variables share is written once.
fy_combined_exposures <- function(estimates) {
  variables <- as.character(estimates$variable)
  labels <- as.character(estimates$variable_label)
  fy_and(unique(labels[!duplicated(variables)]))
}
