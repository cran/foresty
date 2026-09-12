# A forest plot from numbers you already have.
#
# Every other entry point in this package starts from a fitted model and works
# out the estimates itself. This one starts where those finish: a table of
# estimates and their intervals, from wherever they came -- a meta-analysis, a
# model this package does not support, a published table being redrawn, a
# colleague's spreadsheet -- and draws it as the same figure, through the same
# fy_forest_plot() the model-driven functions use. The layout, the styles, the
# column choices and the alignment of the table with the rows of the plot are
# therefore not a second implementation but the same one.
#
# What it cannot do is anything that needs the model: there is none. summary(),
# foresty_report() and the coefficient table refuse, and say so.

#' Draw a forest plot and its table from a data frame of estimates
#'
#' Takes the estimates you already have -- one row per row of the figure, with
#' the effect and the two ends of its interval -- and draws the figure that
#' [foresty_app()] draws: the forest plot and, beside it, the table of numbers,
#' aligned row for row, in any of the journal styles of [foresty_layout()].
#'
#' This is the entry point for numbers that did not come out of a model this
#' package can read. A meta-analysis, a table being redrawn from a paper, a
#' model fitted by something `foresty` does not support, a set of results typed
#' out by hand: pass the data frame, name the columns holding the estimate and
#' its interval, and the figure is the same figure.
#'
#' @section The data:
#'
#' A `data.frame`, a `tibble`, a `data.table` or a matrix, all of which are
#' read as the plain data frame the columns are taken from. One row is one row
#' of the figure, drawn in the order the rows are in, so sort the data the way
#' the figure should read.
#'
#' The estimate and the two ends of its interval are the only columns the
#' figure cannot be drawn without. They are looked for by name when they are
#' not named here -- `estimate`, `conf.low` and `conf.high`, and the usual
#' alternatives (`lower`/`upper`, `lcl`/`ucl`, `ci_low`/`ci_high`) -- so a
#' frame that came out of `broom::tidy(conf.int = TRUE)` needs nothing said
#' about it at all.
#'
#' @section Subgroups:
#'
#' `group` names a column that blocks the rows: its values become the headings
#' the rows sit under, in the order they first appear. A block holding a single
#' row takes no heading of its own -- the row is labelled with the block
#' instead -- which is what makes an "Overall" row a row rather than a heading
#' with one line under it.
#'
#' `emphasis` names a logical column marking the rows drawn for emphasis: a
#' bolder label, a diamond rather than a square, and a rule setting the block
#' off from the ones around it. It is how the overall estimate is set apart
#' from the subgroups read against it. See [foresty_layout()].
#'
#' `interaction_p` names a column of p-values for the interaction. One test
#' covers the block it was taken across, so it is written once, against the
#' first row of the block, rather than repeated down the column.
#'
#' @section What is not here:
#'
#' The figure carries no model, because it was not given one. `summary()`,
#' [foresty_report()], `coef()`, `vcov()` and the coefficient table are
#' properties of a model and refuse on a figure drawn this way, naming
#' [foresty_main()] and [foresty_interaction()] as the way to have them.
#' `as.data.frame()`, `broom::tidy()` and `print()` work as they do on any
#' other `foresty` figure.
#'
#' @param data The estimates, one row per row of the figure. A data frame, a
#'   `tibble`, a `data.table`, or anything [as.data.frame()] accepts.
#' @param estimate,conf.low,conf.high The columns holding the effect and the
#'   two ends of its interval. `NULL`, the default, looks for them by name.
#' @param label The column holding what each row is called. `NULL` looks for
#'   `label`, `term`, `subgroup`, `name` or `variable`, and falls back on the
#'   row numbers.
#' @param group The column that blocks the rows into subgroups, or `NULL` for a
#'   figure of one block.
#' @param n,events,person_time The columns of counts to write in the table
#'   beside the plot, where the data carries them.
#' @param p The column of p-values for the rows themselves. A figure carrying a
#'   test of the interaction leaves this column off unless `columns` asks for
#'   it by name: two columns of p-values side by side are read for each other.
#' @param interaction_p The column of p-values for the interaction, written
#'   once per block.
#' @param reference A logical column marking rows that are a definition rather
#'   than an estimate -- the reference level of a categorical exposure -- which
#'   are drawn without an interval and written "1.00 (reference)". `NULL`
#'   treats a row whose interval is missing and whose estimate is the null
#'   value as one.
#' @param emphasis A logical column marking the rows drawn for emphasis.
#' @param measure What the estimates are, as one of `"OR"`, `"RR"`, `"HR"`,
#'   `"IRR"`, `"MD"` and `"Coefficient"`, or as a name of your own --
#'   `"Standardised mean difference"`, `"Prevalence ratio"` -- in which case
#'   say with `ratio` which side of the null it is drawn about.
#' @param outcome What the measure is a measure of, named so that the axis
#'   reads "Odds ratio for incident asthma" rather than "Odds ratio".
#' @param ratio Whether the estimates are ratios, drawn about 1 on a scale
#'   where the null is 1, rather than differences drawn about 0. `NULL`, the
#'   default, takes it from `measure`, which can only be done for a measure
#'   named by one of the codes above; a measure described in words of your own
#'   has to say, and it is an error not to.
#' @param adjusted Whether the estimates are adjusted, which is a word the axis
#'   and the heading of the table say if so. `foresty` cannot tell from a
#'   column of numbers, so it asks.
#' @param ci_level The confidence level the intervals were formed at, which is
#'   what the heading of the table reports. It is not used to compute anything:
#'   the interval is the one in the data.
#' @param table Whether to draw the table of numbers beside the plot.
#' @param columns Which columns of that table to draw, and in what order. See
#'   [foresty_main()].
#' @param person_time_unit The unit person-time is reported in, as
#'   `person_time_unit = 1000`. Optionally named, to head the column.
#' @param layout The style of the figure, as a name or a [foresty_layout()].
#' @param label_header What to write over the column of row labels. `NULL`
#'   writes "Subgroup" over a blocked figure with nothing emphasised, and
#'   nothing over the rest.
#' @param title,subtitle,xlab The title over the figure, the line under it, and
#'   the label under the plot. `NULL` takes the one the measure implies and
#'   `NA` draws none.
#'
#' @return A `foresty` figure: a `ggplot`/`patchwork` object carrying its
#'   estimates, which prints as the figure and can be added to as any `ggplot`
#'   can.
#'
#' @seealso [foresty_main()] and [foresty_interaction()], which start from a
#'   fitted model; [foresty_layout()] for the styles; [foresty_app()], whose
#'   figures this one reproduces.
#'
#' @examples
#' subgroups <- data.frame(
#'   subgroup  = c("Overall", "Female", "Male", "Under 35", "35 and over"),
#'   block     = c("Overall", "Sex", "Sex", "Maternal age", "Maternal age"),
#'   overall   = c(TRUE, FALSE, FALSE, FALSE, FALSE),
#'   estimate  = c(1.24, 1.05, 1.48, 1.11, 1.39),
#'   conf.low  = c(1.08, 0.86, 1.21, 0.90, 1.14),
#'   conf.high = c(1.42, 1.28, 1.81, 1.37, 1.69),
#'   n         = c(4000, 2009, 1991, 1832, 2168),
#'   events    = c(802, 327, 475, 341, 461),
#'   p_int     = c(NA, 0.012, NA, 0.106, NA)
#' )
#'
#' foresty_data(
#'   subgroups,
#'   label = "subgroup", group = "block", emphasis = "overall",
#'   interaction_p = "p_int",
#'   measure = "OR", outcome = "asthma", adjusted = TRUE
#' )
#'
#' # Every combination of two categorical variables against one of them, which
#' # is what foresty_interaction(reference = ) draws from a model: the blocks
#' # are the levels of the modifier, the rows the levels of the exposure, and
#' # one row of the figure is the group the rest are read against.
#' cells <- data.frame(
#'   ecog      = c("0-1", ">=2", "0-1", ">=2"),
#'   mutation  = c("Negative", "Negative", "Positive", "Positive"),
#'   estimate  = c(1.00, 1.94, 0.62, 1.21),
#'   conf.low  = c(NA, 1.42, 0.44, 0.83),
#'   conf.high = c(NA, 2.65, 0.87, 1.76),
#'   n         = c(212, 168, 190, 145),
#'   events    = c(126, 131, 92, 96),
#'   reference = c(TRUE, FALSE, FALSE, FALSE),
#'   p_int     = c(0.031, NA, NA, NA)
#' )
#'
#' foresty_data(
#'   cells,
#'   label = "ecog", group = "mutation", reference = "reference",
#'   interaction_p = "p_int", measure = "HR", outcome = "death",
#'   adjusted = TRUE, layout = "jama"
#' )
#'
#' @export
foresty_data <- function(data,
                         estimate = NULL,
                         conf.low = NULL,
                         conf.high = NULL,
                         label = NULL,
                         group = NULL,
                         n = NULL,
                         events = NULL,
                         person_time = NULL,
                         p = NULL,
                         interaction_p = NULL,
                         reference = NULL,
                         emphasis = NULL,
                         measure = "OR",
                         outcome = NULL,
                         ratio = NULL,
                         adjusted = FALSE,
                         ci_level = 0.95,
                         table = TRUE,
                         columns = NULL,
                         person_time_unit = NULL,
                         layout = NULL,
                         label_header = NULL,
                         title = NULL,
                         subtitle = NULL,
                         xlab = NULL) {
  data <- fy_data_frame(data)
  checkmate::assert_flag(adjusted)
  checkmate::assert_flag(table)
  checkmate::assert_number(ci_level, lower = 0.5, upper = 0.9999)
  layout <- fy_as_layout(layout)
  person_time_unit <- fy_person_time_spec(person_time_unit)

  info <- fy_data_info(measure, outcome, ratio)
  estimates <- fy_data_estimates(
    data, info,
    columns = list(estimate = estimate, conf.low = conf.low,
                   conf.high = conf.high, label = label, group = group,
                   n = n, events = events, person_time = person_time,
                   p = p, interaction_p = interaction_p,
                   reference = reference, emphasis = emphasis)
  )

  grouped <- !is.null(estimates$block_label)
  emphasised <- any(fy_emphasis_flags(estimates))
  # "Subgroup" is right when every block is one, and wrong the moment the
  # figure carries an overall estimate: the first row under that heading would
  # then be "Overall", which is not a subgroup but the row the subgroups are
  # read against. This is the rule foresty_combine() follows, for the same
  # reason and so that the two figures are headed alike.
  if (is.null(label_header)) {
    label_header <- layout$headings$label %||%
      (if (grouped && !emphasised) "Subgroup" else NULL)
  } else if (length(label_header) == 1L && is.na(label_header)) {
    label_header <- NULL
  }

  plot <- fy_forest_plot(
    estimates,
    exponentiate = info$exponentiate,
    measure_label = fy_resolve_xlab(xlab, fy_axis_label(info, adjusted)),
    estimate_header = fy_estimate_header(info, adjusted, ci_level,
                                         with_outcome = FALSE),
    table = table,
    columns = columns,
    group = if (grouped) "block_label" else NULL,
    title = fy_resolve_title(title, layout, fy_axis_label(info, adjusted)),
    subtitle = subtitle,
    layout = layout,
    label_header = label_header,
    # A block of one row is that row rather than a heading with one line under
    # it, which is what turns "Overall" into the row it is read as.
    fold_singletons = TRUE,
    person_time = person_time_unit
  )

  fy_new_result(
    plot,
    estimates = estimates,
    # There is no model behind this figure, and the methods that need one say
    # so rather than failing on an empty list somewhere further in.
    infos = list(),
    from_data = TRUE,
    exposure = NA_character_,
    blocks = if (grouped) levels(estimates$block_label) else NULL,
    measure = info$measure,
    measure_label = info$measure_label,
    exponentiate = info$exponentiate,
    ci_level = ci_level,
    adjusted = adjusted,
    robust = FALSE,
    person_time = person_time_unit
  )
}

# Whatever was passed, as a plain data frame.
#
# A tibble and a data.table are both data frames already, but a tibble subsets
# to a tibble and a data.table subsets to a data.table, and `frame[[column]]`
# then means something slightly different in each; a matrix is not a data frame
# at all. So everything becomes the one kind of thing before a column of it is
# read.
fy_data_frame <- function(data) {
  if (is.matrix(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(data)) {
    stop(
      "`data` must be the estimates as a data frame -- a `data.frame`, a ",
      "`tibble` or a `data.table` -- with one row per row of the figure, not ",
      paste(class(data), collapse = "/"), ". Draw a figure from a fitted ",
      "model with foresty_main() or foresty_interaction() instead.",
      call. = FALSE
    )
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!nrow(data)) {
    stop("`data` has no rows, so there is no figure to draw", call. = FALSE)
  }
  data
}

# What the estimates are, said the way the rest of the package says it, so that
# the axis and the heading of the table are written by the same functions that
# write them for a model.
#
# `measure` is one of the codes the package knows or a name of your own. A name
# of its own says nothing about which side of the null it is drawn about, so
# `ratio` has to, and defaults to the ratio that most named measures are.
fy_data_info <- function(measure, outcome, ratio) {
  checkmate::assert_string(measure, min.chars = 1L)
  checkmate::assert_string(outcome, null.ok = TRUE)
  checkmate::assert_flag(ratio, null.ok = TRUE)
  named <- outcome %||% NA_character_

  spec <- if (measure %in% fy_measure_codes()) {
    fy_measure_spec(measure, named)
  } else {
    # A measure named by the caller says nothing the package can read: whether
    # a "Prevalence ratio" is drawn about 1 and a "Standardised mean
    # difference" about 0 is the difference between a figure and a wrong
    # figure, and guessing it from the words in the name would be wrong
    # silently. So it is asked for rather than assumed.
    if (is.null(ratio)) {
      stop(
        "`measure = \"", measure, "\" is a measure of your own, and `ratio` ",
        "has to say which side of the null it is drawn about: `ratio = TRUE` ",
        "for a ratio, drawn about 1, and `ratio = FALSE` for a difference, ",
        "drawn about 0. One of \"",
        paste(fy_measure_codes(), collapse = "\", \""),
        "\" is named rather than described and needs no `ratio`.",
        call. = FALSE
      )
    }
    list(measure = measure, outcome = named, name = measure,
         label = fy_measure_label(measure, named), exponentiate = ratio)
  }
  # The names a model's description carries, which are the ones every function
  # that writes an axis or a heading reads.
  list(
    measure = spec$measure,
    measure_name = spec$name,
    measure_label = spec$label,
    outcome = spec$outcome,
    exponentiate = if (is.null(ratio)) spec$exponentiate else ratio
  )
}

# The rows of the figure, as the columns fy_forest_plot() reads them from.
#
# Everything it looks for is here whether the data carried it or not: a column
# of NA says "this figure has no events to report" and is left out of the table
# rather than drawn empty, which is the same thing the model-driven functions
# hand it.
fy_data_estimates <- function(data, info, columns) {
  take <- function(what, aliases = character(0), type = "numeric") {
    fy_data_column(data, columns[[what]], what, aliases, type)
  }

  estimate <- take("estimate", c("est", "effect", "yi", "beta", "coef",
                                 "or", "rr", "hr", "irr", "smd"))
  conf.low <- take("conf.low", c("conf_low", "ci.low", "ci_low", "ci_lower",
                                 "lower", "low", "lcl", "lci", "ci.lb"))
  conf.high <- take("conf.high", c("conf_high", "ci.high", "ci_high",
                                   "ci_upper", "upper", "high", "ucl", "uci",
                                   "ci.ub"))
  if (is.null(estimate) || is.null(conf.low) || is.null(conf.high)) {
    stop(
      "the estimate and the two ends of its interval are what a forest plot ",
      "is of, and at least one of them could not be found in `data`. Name ",
      "the columns holding them: `foresty_data(d, estimate = \"est\", ",
      "conf.low = \"lcl\", conf.high = \"ucl\")`. The columns of `data` are ",
      paste0("\"", names(data), "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (any(!is.na(conf.low) & !is.na(conf.high) & conf.low > conf.high)) {
    stop(
      "some rows of `data` have a lower end of the interval above the upper ",
      "end, so the two columns are the wrong way round or the wrong pair of ",
      "columns has been named",
      call. = FALSE
    )
  }
  # An estimate outside its own interval is the other way the columns come to
  # be paired wrongly, and one a figure would draw without complaint: the mark
  # sits off the end of the bar it belongs to, which reads as a finding rather
  # than as a mistake.
  outside <- !is.na(estimate) &
    ((!is.na(conf.low) & estimate < conf.low) |
       (!is.na(conf.high) & estimate > conf.high))
  if (any(outside)) {
    first <- which(outside)[1L]
    stop(
      "some rows of `data` have an estimate outside their own interval -- row ",
      first, " has ", fy_trim_number(estimate[first]), " against an interval ",
      "of ", fy_trim_number(conf.low[first]), " to ",
      fy_trim_number(conf.high[first]), " -- so at least one of the three ",
      "columns is not the one it was taken for.",
      call. = FALSE
    )
  }

  label <- take("label", c("term", "subgroup", "name", "row", "variable",
                           "level", "study"), type = "character")
  if (is.null(label)) {
    label <- as.character(seq_len(nrow(data)))
  }
  group <- take("group", type = "character")
  null_value <- if (isTRUE(info$exponentiate)) 1 else 0

  reference <- take("reference", type = "logical")
  if (is.null(reference)) {
    # A row with no interval whose estimate sits exactly on the null is a
    # definition rather than an estimate -- the reference level of a
    # categorical exposure -- and the table says so instead of leaving a blank.
    reference <- is.na(conf.low) & is.na(conf.high) &
      !is.na(estimate) & estimate == null_value
  }

  out <- data.frame(
    variable = label,
    variable_label = label,
    level = NA_character_,
    contrast_label = NA_character_,
    reference = reference,
    estimate = estimate,
    se = NA_real_,
    conf.low = ifelse(reference, NA_real_, conf.low),
    conf.high = ifelse(reference, NA_real_, conf.high),
    statistic = NA_real_,
    p.value = take("p", c("p.value", "p_value", "pval", "pvalue")) %||%
      rep(NA_real_, nrow(data)),
    n = take("n", c("total", "n_total", "size")) %||% rep(NA_real_, nrow(data)),
    events = take("events", c("event", "cases", "n_events")) %||%
      rep(NA_real_, nrow(data)),
    person_time = take("person_time", c("person_years", "pyears", "py")) %||%
      rep(NA_real_, nrow(data)),
    row_label = label,
    stringsAsFactors = FALSE
  )

  emphasis <- take("emphasis", type = "logical")
  if (!is.null(emphasis)) {
    out$emphasis <- emphasis
  }
  p_int <- take("interaction_p", c("p_interaction", "p.interaction",
                                   "interaction.p"))
  if (!is.null(p_int)) {
    out$interaction_p <- p_int
  }

  if (!is.null(group)) {
    # A missing block is not a block: the row would be left out of the levels
    # and drawn under a heading reading NA, at whichever end of the figure the
    # sort happened to put it.
    if (anyNA(group)) {
      stop(
        "the column `group` names has missing values in row",
        if (sum(is.na(group)) > 1L) "s " else " ",
        paste(which(is.na(group)), collapse = ", "),
        ", so those rows belong to no block. Name the block every row is in.",
        call. = FALSE
      )
    }
    out$block <- group
    out$block_label <- factor(group, levels = unique(group))
  }
  out$variable_label <- factor(out$variable_label,
                               levels = unique(out$variable_label))
  # The rows are drawn top to bottom in the order they were given, and the
  # factor a figure orders its rows by runs the other way.
  out$label <- factor(out$row_label, levels = rev(unique(out$row_label)))
  rownames(out) <- NULL
  out
}

# One column of the data: the one that was named, or the first of the names it
# is usually called by, or nothing.
#
# A name that was given and is not there is a mistake and is reported as one; a
# name that was guessed at and is not there is simply a column this figure does
# not carry.
fy_data_column <- function(data, given, what, aliases = character(0),
                           type = "numeric") {
  if (!is.null(given)) {
    checkmate::assert_string(given, min.chars = 1L, .var.name = what)
    if (!given %in% names(data)) {
      stop(
        "`", what, " = \"", given, "\"` names a column that is not in ",
        "`data`, whose columns are ",
        paste0("\"", names(data), "\"", collapse = ", "), ".",
        call. = FALSE
      )
    }
    return(fy_data_cast(data[[given]], given, type))
  }
  wanted <- c(what, aliases)
  found <- names(data)[tolower(names(data)) %in% tolower(wanted)]
  if (!length(found)) {
    return(NULL)
  }
  # In the order they are usually called by rather than the order the data
  # happens to hold them in, so that a frame carrying both `estimate` and `or`
  # is read as the first of them.
  found <- found[order(match(tolower(found), tolower(wanted)))]
  fy_data_cast(data[[found[1L]]], found[1L], type)
}

fy_data_cast <- function(x, name, type) {
  if (identical(type, "character")) {
    return(as.character(x))
  }
  if (identical(type, "logical")) {
    # A flag kept as 0 and 1, or as "yes" and "no", is a flag.
    if (is.numeric(x)) {
      return(!is.na(x) & x != 0)
    }
    if (is.character(x) || is.factor(x)) {
      return(tolower(as.character(x)) %in% c("true", "yes", "y", "1"))
    }
    return(!is.na(x) & as.logical(x))
  }
  out <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.na(out)) && !all(is.na(x))) {
    stop(
      "the column \"", name, "\" holds no numbers, so it cannot be the one ",
      "it was taken for. Name the column that does.",
      call. = FALSE
    )
  }
  out
}
