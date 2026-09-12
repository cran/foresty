# The object foresty returns is the figure itself, with the analysis carried
# along in an attribute.
#
# That is what lets ggplot2's own `+` and `&` reach it, so a figure can be
# retuned the way any other ggplot is, while summary(), tidy() and predict()
# still reach the numbers behind it. Defining `+.foresty` instead would collide
# with ggplot2's `+.gg` and silently return the wrong thing.

fy_as_foresty_plot <- function(plot) {
  class(plot) <- c("foresty", class(plot))
  plot
}

fy_new_result <- function(plot, ...) {
  attr(plot, "foresty") <- list(...)
  plot
}

# Drawing a figure --------------------------------------------------------

#' Draw a forest plot
#'
#' Draws the figure, having first made sure the plot in it has room for its
#' axis. A column of text is as wide as what it holds, and the plot is given
#' what those columns leave, which is a decision that cannot be made while the
#' figure is being built: how much they leave depends on the width the figure is
#' drawn at. So a table wide enough -- a survival model reporting N, events and
#' person-time beside the estimates -- can leave the plot a centimetre and the
#' numbers under its axis printed one on top of another. The floor is applied
#' here, where the width being drawn at is known: see the `min_plot_width`
#' argument of [foresty_layout()]. Where it has to act, the columns of text
#' stop being as wide as what they hold and share what the plot leaves in
#' proportion to it, which is where the room for the axis comes from; a figure
#' with room for its columns already is drawn exactly as it was built.
#'
#' @param x A figure returned by [foresty_main()], [foresty_interaction()] or
#'   [foresty_combine()].
#' @param ... Passed on to patchwork's and ggplot2's own print methods.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' fit <- glm(asthma ~ no2 + sex, family = binomial, data = foresty_cohort)
#' print(foresty_interaction(fit, "no2", "sex", table = TRUE))
#'
#' @export
print.foresty <- function(x, ...) {
  drawn <- fy_floor_plot_width(x)
  # Handed on to patchwork's method, or to ggplot2's on a figure that is
  # nothing but the plot. NextMethod() cannot be used: it would dispatch on the
  # object as it arrived rather than on the one whose widths have been settled.
  # The class is dropped rather than kept so that the print below is the next
  # method rather than this one again.
  class(drawn) <- setdiff(class(drawn), "foresty")
  print(drawn, ...)
  invisible(x)
}

#' @rdname print.foresty
#' @export
plot.foresty <- function(x, ...) {
  print(x, ...)
}

#' @rdname print.foresty
#' @param recording Whether to record the drawing on the display list, as in
#'   [grid::grid.draw()].
#' @export
grid.draw.foresty <- function(x, recording = TRUE) {
  # ggplot2::ggsave() draws rather than prints, so the width has to be settled
  # here as well: a figure saved to a file and a figure on the screen are the
  # same figure and cannot be laid out differently.
  drawn <- fy_floor_plot_width(x)
  class(drawn) <- setdiff(class(drawn), "foresty")
  grid::grid.draw(drawn, recording = recording)
}

# Gives the plot the least of the figure it is allowed to have.
#
# The widths patchwork was handed are centimetres for each column of text and
# one null unit for the plot, the null being "whatever is left". Where what is
# left is less than the floor, every width becomes a null unit instead: the
# plot takes its share of the figure and the columns of text divide the rest in
# proportion to what they hold, which is the same division `plot_width` as a
# fraction makes. Nothing is changed on a figure that fits, so the usual
# behaviour -- a column of text exactly as wide as its widest entry, and
# widening the figure widening the plot alone -- is what a figure with room for
# it is still drawn with.
fy_floor_plot_width <- function(x) {
  share <- attr(x, "fy_min_plot_width")
  if (!inherits(x, "patchwork") || is.null(share) || !isTRUE(share > 0)) {
    return(x)
  }
  widths <- x$patches$layout$widths
  if (!grid::is.unit(widths) || !length(widths)) {
    return(x)
  }

  # Exactly one null width is the layout this can act on: the plot taking what
  # the columns of text leave. A caller who fixed the widths themselves, in
  # centimetres or as fractions of the figure, has said what they want.
  null <- grid::unitType(widths) == "null"
  if (sum(null) != 1L) {
    return(x)
  }

  device <- fy_device_width_cm()
  cm <- vapply(seq_along(widths), function(i) {
    if (null[i]) 0 else grid::convertWidth(widths[i], "cm", valueOnly = TRUE)
  }, numeric(1))
  text <- sum(cm)
  if (is.na(device) || device <= 0 || device - text >= device * share) {
    return(x)
  }

  cm[null] <- text * share / (1 - share)
  x$patches$layout$widths <- grid::unit(cm, "null")
  x
}

# How wide the figure is being drawn, in centimetres. The device is the best
# answer available: a figure is a page of its own, and where it is not -- one
# panel of a larger composition -- the width is an over-estimate, which errs
# towards leaving the layout alone rather than towards rearranging a figure
# that was fine.
fy_device_width_cm <- function() {
  out <- tryCatch(grDevices::dev.size("cm")[1L], error = function(e) NA_real_)
  if (length(out) != 1L || !is.finite(out)) NA_real_ else out
}

# The analysis behind a figure. Errors rather than returning NULL, since a
# foresty object that has lost its attribute is a bug worth hearing about.
fy_result <- function(x) {
  out <- attr(x, "foresty")
  if (is.null(out)) {
    stop(
      "this object no longer carries its estimates. That happens when a ",
      "foresty figure is rebuilt by a function that drops attributes; keep ",
      "the original and add layers to a copy.",
      call. = FALSE
    )
  }
  out
}

# The models a figure was drawn from.
#
# A figure drawn by foresty_data() was drawn from a table of numbers and has
# none, so everything that is a property of a model rather than of the figure
# -- the coefficient table, the report, summary() -- refuses here, once, rather
# than failing further in on an empty list.
fy_infos <- function(x) {
  result <- fy_result(x)
  if (isTRUE(result$from_data)) {
    stop(
      "this figure was drawn from a data frame by foresty_data(), so there is ",
      "no fitted model behind it and nothing that is a property of one -- the ",
      "coefficient table, the model summary, the HTML report -- can be given. ",
      "The estimates themselves are there: as.data.frame() and broom::tidy() ",
      "return them. Draw the figure from the model with foresty_main() or ",
      "foresty_interaction() to have the rest.",
      call. = FALSE
    )
  }
  result$infos
}

fy_single_info <- function(x, model = NULL, what = "this") {
  infos <- fy_infos(x)
  if (length(infos) == 1L) {
    return(infos[[1L]])
  }
  if (is.null(model)) {
    stop(
      what, " covers ", length(infos), " models, so `model` has to say which ",
      "one is meant, as `model = 1`.",
      call. = FALSE
    )
  }
  checkmate::assert_int(model, lower = 1, upper = length(infos))
  infos[[model]]
}

# Summary ---------------------------------------------------------------------

#' Summarize a foresty figure
#'
#' Reports what the figure was drawn from: the model, its whole coefficient
#' table with standard errors and p-values in the manner of [summary.glm()],
#' the exposure or subgroup estimates on the reported scale, and the joint test
#' of the interaction where there is one.
#'
#' The coefficient table is on the scale the model was fitted on, as a model
#' summary is, so a logistic regression reports log odds. The estimates
#' underneath are on the reported scale, exponentiated where the measure is a
#' ratio.
#'
#' @param object A `foresty` object.
#' @param model Which model to report, when the figure covers several. Defaults
#'   to the only one.
#' @param ... Ignored.
#'
#' @return An object of class `summary.foresty`, a list with elements `call`,
#'   `coefficients`, `estimates` and, for an interaction, `interaction_test`.
#'
#' @examples
#' fit <- glm(asthma ~ no2 + sex + maternal_age, family = binomial,
#'            data = foresty_cohort)
#' summary(foresty_interaction(fit, exposure = "no2", interaction = "sex"))
#'
#' @export
summary.foresty <- function(object, model = NULL, ...) {
  result <- fy_result(object)
  info <- fy_single_info(object, model, what = "this figure")

  structure(
    list(
      call = stats::getCall(info$fit),
      model_class = class(info$fit),
      formula = try(stats::formula(info$fit), silent = TRUE),
      n = info$n,
      events = info$events,
      person_time = info$person_time,
      person_time_unit = result$person_time,
      measure = result$measure,
      measure_label = result$measure_label,
      exponentiate = result$exponentiate,
      ci_level = result$ci_level,
      robust = isTRUE(result$robust),
      exposure = result$exposure,
      modifier = result$modifier,
      reference_group = fy_reference_phrase(result),
      coefficients = fy_coefficient_matrix(info),
      estimates = fy_estimates_frame(object),
      interaction_test = result$interaction_test,
      interaction_tests = result$interaction_tests
    ),
    class = "summary.foresty"
  )
}

# The whole coefficient table, on the scale the model was fitted on, laid out
# the way stats::summary.glm() lays one out.
fy_coefficient_matrix <- function(info) {
  b <- info$coef
  se <- sqrt(diag(info$vcov))
  statistic <- b / se
  use_t <- is.finite(info$error_df)
  p <- if (use_t) {
    2 * stats::pt(abs(statistic), df = info$error_df, lower.tail = FALSE)
  } else {
    2 * stats::pnorm(abs(statistic), lower.tail = FALSE)
  }

  out <- cbind(Estimate = b, `Std. Error` = se, statistic, p)
  colnames(out)[3:4] <- if (use_t) {
    c("t value", "Pr(>|t|)")
  } else {
    c("z value", "Pr(>|z|)")
  }
  out
}

#' @rdname summary.foresty
#' @param x A `summary.foresty` object.
#' @export
print.summary.foresty <- function(x, ...) {
  cat("\nCall:\n", paste(deparse(x$call), collapse = "\n"), "\n\n", sep = "")

  cat("Model:      ", paste(x$model_class, collapse = ", "), "\n", sep = "")
  cat("Measure:    ", x$measure_label,
      if (x$robust) "  (robust standard errors)" else "", "\n", sep = "")
  cat("Observations:", format(x$n, big.mark = ","), sep = " ")
  if (!is.na(x$events)) {
    cat("   Events:", format(x$events, big.mark = ","))
  }
  if (!is.na(x$person_time)) {
    cat("   ",
        fy_person_time_heading("Person-time", x$person_time_unit, sep = " "),
        ": ", fy_format_person_time(x$person_time, x$person_time_unit),
        sep = "")
  }
  cat("\n\nCoefficients:\n")
  stats::printCoefmat(x$coefficients, signif.stars = FALSE, digits = 4)

  cat("\n", x$measure_label, " (", round(x$ci_level * 100), "% CI):\n", sep = "")
  # Which cell the rows are read against, where every one of them is read
  # against one cell rather than each against its own subgroup.
  if (!is.null(x$reference_group)) {
    cat("Every combination compared with ", x$reference_group, "\n", sep = "")
  }
  print(fy_console_table(x$estimates, x$measure, x$person_time_unit),
        row.names = FALSE)

  # Every test that was asked for, one line apiece, so that a figure reporting
  # both says what each of them came to.
  tests <- x$interaction_tests %||% (
    if (!is.null(x$interaction_test)) list(x$interaction_test)
  )
  if (length(tests)) {
    cat("\nInteraction (", x$exposure, " by ", x$modifier, "):\n", sep = "")
    for (test in tests) {
      cat("  ", test$test, " = ", fy_format_number(test$statistic),
          " on ", test$df, " df, p = ", fy_format_p(test$p.value), "\n",
          sep = "")
    }
  }
  invisible(x)
}

fy_console_table <- function(estimates, measure, person_time = NULL) {
  columns <- list()
  if ("block_label" %in% names(estimates)) {
    columns[["Block"]] <- as.character(estimates$block_label)
    columns[["Row"]] <- as.character(estimates$label)
  }
  if ("modifier_label" %in% names(estimates)) {
    columns[["Subgroup"]] <- as.character(estimates$modifier_label)
  }
  columns[["Variable"]] <- as.character(estimates$variable_label)
  if (any(!is.na(estimates$level))) {
    columns[["Level"]] <- ifelse(is.na(estimates$level), "", estimates$level)
  }
  columns[[measure]] <- fy_format_number(estimates$estimate)
  columns[["95% CI"]] <- ifelse(
    estimates$reference, "reference",
    paste0(fy_format_number(estimates$conf.low), "-",
           fy_format_number(estimates$conf.high))
  )
  columns[["p"]] <- fy_format_p(estimates$p.value)
  columns[["N"]] <- fy_format_count(estimates$n)
  if (!all(is.na(estimates$events))) {
    columns[["Events"]] <- fy_format_count(estimates$events)
  }
  if (!all(is.na(estimates$person_time))) {
    heading <- fy_person_time_heading("Person-time", person_time, sep = " ")
    columns[[heading]] <- fy_format_person_time(estimates$person_time,
                                                person_time)
  }
  do.call(data.frame, c(columns, check.names = FALSE,
                        stringsAsFactors = FALSE))
}

# Passing the result on ------------------------------------------------------

#' Turn a foresty figure into a data frame
#'
#' `tidy()` returns the estimates the figure was drawn from, one row apiece, in
#' the columns `broom` uses and in the order it puts them, so the result reads
#' beside a tidied model and drops into the same pipelines. `glance()` returns
#' one row describing the fit.
#'
#' `as.data.frame()` returns everything the figure carries instead, the display
#' labels included, which is what to use when the estimates are going back into
#' a plot rather than into a table. It is the one of the three that needs
#' nothing installed.
#'
#' `tidy()` and `glance()` are `broom`'s generics, and `foresty` registers its
#' methods on them rather than carrying `broom` itself: install it if you want
#' them, and call them as `broom::tidy(x)` or after `library(broom)`.
#'
#' @param x A `foresty` object.
#' @param what `"estimates"`, the default, returns the rows drawn on the
#'   figure. `"coefficients"` returns the whole coefficient table of the model
#'   instead, on the scale it was fitted on.
#' @param conf.int Whether to include the confidence interval. Defaults to
#'   `TRUE`, an interval being the point of a forest plot.
#' @param model Which model to take the coefficients from, when the figure
#'   covers several. `glance()` takes the counts of that model too; without it
#'   a figure of several models is glanced at as a figure -- its measure, its
#'   confidence level, its test and how many models it holds -- and the columns
#'   describing what one model was fitted to are `NA`, no one of them being the
#'   figure's.
#' @param row.names,optional Ignored, present for consistency with the generic.
#' @param ... Ignored.
#'
#' @return A data frame.
#'
#' @examples
#' fit <- glm(asthma ~ no2 + sex + maternal_age, family = binomial,
#'            data = foresty_cohort)
#' x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
#' as.data.frame(x)
#'
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   broom::tidy(x)
#'   broom::glance(x)
#' }
#'
#' @name foresty-tidiers
NULL

#' @rdname foresty-tidiers
#' @exportS3Method broom::tidy
tidy.foresty <- function(x, what = c("estimates", "coefficients"),
                         conf.int = TRUE, model = NULL, ...) {
  what <- match.arg(what)
  if (what == "coefficients") {
    info <- fy_single_info(x, model, what = "this figure")
    cm <- fy_coefficient_matrix(info)
    out <- data.frame(
      term = rownames(cm),
      estimate = cm[, 1],
      std.error = cm[, 2],
      statistic = cm[, 3],
      p.value = cm[, 4],
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    if (conf.int) {
      L <- diag(nrow(cm))
      rownames(L) <- rownames(cm)
      interval <- fy_lincom(info, L, ci_level = fy_result(x)$ci_level,
                            exponentiate = FALSE)
      out$conf.low <- interval$conf.low
      out$conf.high <- interval$conf.high
    }
    return(fy_as_tibble(out))
  }
  fy_as_tibble(fy_tidy_estimates(x, conf.int = conf.int))
}

# The estimates in broom's shape: the columns it names, in the order it puts
# them, and nothing else except the counts, which are the reason a subgroup
# estimate is worth reporting at all. The display labels are left out; they
# belong to the figure.
fy_tidy_estimates <- function(x, conf.int = TRUE) {
  estimates <- fy_result(x)$estimates

  out <- data.frame(
    term = as.character(estimates$label),
    estimate = estimates$estimate,
    std.error = estimates$se,
    statistic = estimates$statistic,
    p.value = estimates$p.value,
    stringsAsFactors = FALSE
  )
  if (conf.int) {
    out$conf.low <- estimates$conf.low
    out$conf.high <- estimates$conf.high
  }

  # Only the columns that say something about this figure. A model with one
  # continuous exposure has no levels and no reference row to report.
  if (any(!is.na(estimates$level))) {
    out$level <- estimates$level
    out$reference <- estimates$reference
  }
  if (!is.null(estimates$block_label)) {
    out$block <- as.character(estimates$block_label)
  }
  if (!is.null(estimates$modifier_level)) {
    out$modifier_level <- estimates$modifier_level
    out$interaction.p.value <- estimates$interaction_p
    if (!is.null(estimates$interaction_p_lrt)) {
      out$interaction.lrt.p.value <- estimates$interaction_p_lrt
    }
  }
  out$n <- estimates$n
  if (!all(is.na(estimates$events))) {
    out$events <- estimates$events
  }
  if (!all(is.na(estimates$person_time))) {
    out$person.time <- estimates$person_time
  }
  rownames(out) <- NULL
  out
}

# broom returns a tibble, so a foresty result prints like one beside it when
# tibble is there to make one, and stays a plain data frame when it is not.
fy_as_tibble <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(x))
  }
  x
}

#' @rdname foresty-tidiers
#' @exportS3Method broom::glance
glance.foresty <- function(x, model = NULL, ...) {
  result <- fy_result(x)
  infos <- fy_infos(x)
  # What the figure is -- its measure, its confidence level, the test beside
  # its rows -- belongs to the whole of it however many models went into it.
  # What a model was fitted to does not: a figure of three models has three
  # counts and no one of them is the figure's, so unless one model is named
  # they are left out rather than reported off whichever came first.
  info <- if (length(infos) == 1L || !is.null(model)) {
    fy_single_info(x, model, what = "this figure")
  } else {
    NULL
  }
  out <- data.frame(
    measure = result$measure,
    n = if (is.null(info)) NA_integer_ else info$n,
    events = if (is.null(info)) NA_integer_ else info$events,
    person_time = if (is.null(info)) NA_real_ else info$person_time,
    conf.level = result$ci_level,
    robust = isTRUE(result$robust),
    n_models = length(infos),
    stringsAsFactors = FALSE
  )
  if (!is.null(result$interaction_test)) {
    out$interaction.statistic <- result$interaction_test$statistic
    out$interaction.df <- result$interaction_test$df
    out$interaction.p.value <- result$interaction_test$p.value
  }
  # The second test, where both were asked for, in columns of its own rather
  # than in place of the first.
  lrt <- result$interaction_tests$lrt
  if (!is.null(lrt) && length(result$interaction_tests) > 1L) {
    out$interaction.lrt.statistic <- lrt$statistic
    out$interaction.lrt.df <- lrt$df
    out$interaction.lrt.p.value <- lrt$p.value
  }
  out
}

#' @rdname foresty-tidiers
#' @export
as.data.frame.foresty <- function(x, row.names = NULL, optional = FALSE, ...) {
  fy_estimates_frame(x)
}

fy_estimates_frame <- function(x, tidy_names = FALSE) {
  estimates <- if (inherits(x, "foresty")) fy_result(x)$estimates else x
  estimates$variable_label <- as.character(estimates$variable_label)
  estimates$label <- as.character(estimates$label)
  if ("modifier_label" %in% names(estimates)) {
    estimates$modifier_label <- as.character(estimates$modifier_label)
  }
  if ("block_label" %in% names(estimates)) {
    estimates$block_label <- as.character(estimates$block_label)
  }
  if (tidy_names) {
    names(estimates)[names(estimates) == "se"] <- "std.error"
    names(estimates)[names(estimates) == "label"] <- "term"
    front <- intersect(
      c("term", "block", "variable", "level", "modifier_level", "estimate",
        "std.error", "statistic", "p.value", "conf.low", "conf.high"),
      names(estimates)
    )
    estimates <- estimates[, c(front, setdiff(names(estimates), front)),
                           drop = FALSE]
  }
  rownames(estimates) <- NULL
  estimates
}

#' Model methods for foresty figures
#'
#' These pass through to the model the figure was drawn from, so that a foresty
#' result can be used wherever the fit itself would have been.
#'
#' @param object,x,formula A `foresty` object. The argument is named `formula`
#'   in `model.frame()` only because the generic names it that.
#' @param model Which model is meant, when the figure covers several.
#' @param ... Passed on to the method for the underlying fit.
#'
#' @return Whatever the corresponding method for the underlying model returns.
#'
#' @examples
#' fit <- glm(asthma ~ no2 + sex, family = binomial, data = foresty_cohort)
#' x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
#' head(predict(x, type = "response"))
#' formula(x)
#' nobs(x)
#'
#' @name foresty-model-methods
NULL

#' @rdname foresty-model-methods
#' @export
predict.foresty <- function(object, ..., model = NULL) {
  stats::predict(fy_single_info(object, model, what = "this figure")$fit, ...)
}

#' @rdname foresty-model-methods
#' @export
coef.foresty <- function(object, ..., model = NULL) {
  fy_single_info(object, model, what = "this figure")$coef
}

#' @rdname foresty-model-methods
#' @export
vcov.foresty <- function(object, ..., model = NULL) {
  fy_single_info(object, model, what = "this figure")$vcov
}

#' @rdname foresty-model-methods
#' @export
formula.foresty <- function(x, ..., model = NULL) {
  stats::formula(fy_single_info(x, model, what = "this figure")$fit)
}

#' @rdname foresty-model-methods
#' @export
nobs.foresty <- function(object, ..., model = NULL) {
  fy_single_info(object, model, what = "this figure")$n
}

#' @rdname foresty-model-methods
#' @export
model.frame.foresty <- function(formula, ..., model = NULL) {
  fy_single_info(formula, model, what = "this figure")$mf
}
