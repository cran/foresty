#' Write a foresty figure and its numbers to a self-contained HTML page
#'
#' Writes what an interaction p-value was standing in for: the exposure effect
#' within each level of the modifier, the joint test of the interaction, the
#' ratio of the effects between levels, the whole coefficient table of the
#' updated model, and the figure. Everything is inlined, so the file can be
#' opened or sent on its own.
#'
#' A figure drawn from several models -- [foresty_main()] over a list of them --
#' describes each of them on the page and gives each its own coefficient table,
#' headed with the exposure it was fitted for. `model` cuts those sections down
#' to one. The rest of the page is the figure itself -- the estimates, the test
#' and the plot -- and is the whole of it whichever model is named, a figure
#' being one figure however many models went into it.
#'
#' @param x An object returned by [foresty_interaction()] or [foresty_main()].
#' @param file Path to write to. A `.html` extension is added if missing.
#' @param title Browser page title. The default names the exposure and the
#'   modifier; it is not displayed in the report body.
#' @param model Which model to report the coefficients of, when the figure
#'   covers several. The default reports every one of them.
#' @param width,height Size of the embedded plot in inches. `height` defaults
#'   to a size that fits the number of rows.
#' @param open Whether to open the file when it has been written. Defaults to
#'   `FALSE`.
#'
#' @return The path written to, invisibly.
#'
#' @examples
#' # Writing the page renders the figure and lays the tables out with `gt`,
#' # which is the slow part, so it is left out of the timed run.
#' \donttest{
#' fit <- glm(asthma ~ no2 + sex + maternal_age, family = binomial,
#'            data = foresty_cohort)
#' x <- foresty_interaction(fit, exposure = "no2", interaction = "sex")
#' foresty_report(x, file = file.path(tempdir(), "no2_by_sex.html"))
#'
#' # The same page can be asked for as the figure is made. `html = TRUE`
#' # writes it under a name taken from the variables it is about -- here
#' # no2_sex.html -- into the working directory; a path writes it there
#' # instead, which is what these examples do so that nothing is written
#' # outside the session's temporary directory.
#' foresty_interaction(fit, exposure = "no2", interaction = "sex",
#'                     html = file.path(tempdir(), "no2_sex.html"))
#' foresty_main(list(fit), exposure = "no2",
#'              html = file.path(tempdir(), "no2.html"))
#' }
#'
#' @export
foresty_report <- function(x, file, title = NULL, model = NULL, width = 10,
                           height = NULL, open = FALSE) {
  if (inherits(x, "foresty_figures")) {
    stop(
      "this is the list of figures foresty_combine() returns when the figures ",
      "it combined report more than one exposure, and a report is written of ",
      "one figure at a time. Pass one of them, as `x[[1]]` or `x[[\"",
      names(x)[1L], "\"]]`.",
      call. = FALSE
    )
  }
  if (!inherits(x, "foresty")) {
    stop("`x` must be the result of foresty_interaction() or foresty_main()",
         call. = FALSE)
  }
  checkmate::assert_string(file)
  fy_require("gt", "write an HTML report")

  if (!grepl("\\.html?$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".html")
  }
  result <- fy_result(x)
  is_interaction <- !is.null(result$interaction_test)
  if (is.null(title)) {
    exposures <- paste(unique(as.character(result$exposure)), collapse = ", ")
    title <- if (is_interaction) {
      paste0(exposures, " by ", result$modifier)
    } else if (nzchar(exposures)) {
      exposures
    } else {
      "Forest plot"
    }
  }

  infos <- fy_infos(x)
  chosen <- if (is.null(model)) {
    seq_along(infos)
  } else {
    checkmate::assert_int(model, lower = 1, upper = length(infos))
    as.integer(model)
  }
  # A page carrying more than one model has to say which is which. A combined
  # figure's models are its Overall and subgroup blocks, so include that block
  # as well as the exposure in the coefficient-table heading.
  named <- rep_len(as.character(result$exposure), length(infos))
  blocks <- rep_len(result$blocks %||% NA_character_, length(infos))
  heading <- function(what, i) {
    block <- blocks[i]
    if (!is.na(block) && nzchar(block) && !identical(block, "Overall")) {
      block <- paste0("Subgroup: ", block)
    }
    suffix <- c(named[i], block)
    suffix <- suffix[!is.na(suffix) & nzchar(suffix)]
    if (length(chosen) > 1L) paste0(what, ": ", paste(suffix, collapse = " - ")) else what
  }

  sections <- c(
    vapply(chosen, function(i) {
      fy_section(heading("Model", i),
                 fy_model_table(infos[[i]], result, named[i]))
    }, character(1)),
    if (is_interaction) fy_section("Interaction test",
                                   fy_interaction_html(x, result)),
    fy_section(
      if (is_interaction) "Subgroup-specific estimates" else "Estimates",
      paste0(fy_estimates_table(x, result), fy_counts_note_html(result))
    ),
    fy_section("Forest plot", fy_plot_html(x, width = width, height = height)),
    vapply(chosen, function(i) {
      fy_section(heading("Full coefficient table", i),
                 fy_coefficient_table(infos[[i]], result))
    }, character(1))
  )

  # The date the page was written goes under the heading rather than at the
  # foot: a directory fills up with pages of the same analysis run again, and
  # which of them is the current one is the first thing a reader has to know,
  # not the last.
  html <- paste0(
    "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", fy_escape(title), "</title>\n<style>", fy_report_css(),
    "</style>\n</head>\n<body>\n<main>\n",
    "<p class=\"date\">", format(Sys.Date(), "%d %B %Y"), "</p>\n",
    paste(sections, collapse = "\n"),
    "\n</main>\n</body>\n</html>\n"
  )

  writeLines(html, file, useBytes = TRUE)
  if (isTRUE(open)) {
    utils::browseURL(file)
  }
  invisible(file)
}

# What `html` in foresty_main() and foresty_interaction() asks for.
#
# FALSE, which is the default, asks for nothing, so nothing leaves the session
# unless it is asked for. TRUE asks for the report under a name written from
# the variables it is about -- the exposure and the modifier, joined by an
# underscore -- so that a report of NO2 by sex is no2_sex.html and a directory
# of them can be read without opening any. A string is the path itself.
fy_report_file <- function(html, exposure, modifier = NULL) {
  if (is.null(html) || isFALSE(html)) {
    return(NULL)
  }
  if (is.character(html)) {
    checkmate::assert_string(html, min.chars = 1L)
    return(html)
  }
  if (!isTRUE(html)) {
    stop(
      "`html` must be TRUE or FALSE, or the path to write the report to; it ",
      "is ", paste(class(html), collapse = "/"), ".",
      call. = FALSE
    )
  }
  name <- fy_file_slug(c(unique(exposure), modifier))
  paste0(if (nzchar(name)) name else "foresty", ".html")
}

# The variable names as a file name: joined by underscores, and with anything a
# file name cannot carry -- a bracket from `ns(no2, 3)`, a slash, a space --
# turned into one as well.
#
# A letter is a letter in any script, not only in Latin. Reducing a name to the
# ASCII in it leaves nothing at all of one written in Japanese or Greek or
# Cyrillic, so every figure whose variables are named in such a script would be
# written to the same fallback file and would overwrite the last one -- which
# is the one thing a file named for its variables is there to prevent.
fy_file_slug <- function(x) {
  out <- gsub("[^\\p{L}\\p{N}._-]+", "_", paste(as.character(x), collapse = "_"),
              perl = TRUE)
  gsub("(^_+)|(_+$)", "", gsub("_+", "_", out))
}

fy_section <- function(heading, body) {
  paste0("<section>\n<h2>", fy_escape(heading), "</h2>\n", body, "\n</section>")
}

fy_model_table <- function(info, result, exposure) {
  rows <- list()
  # What the model is, in the words a methods section uses, rather than the
  # classes of the object it is held in: a reader checking the page against a
  # paper is looking for "logistic regression", not "glm, lm".
  rows[["Model"]] <- fy_model_name(info$fit)
  formula <- try(stats::formula(info$fit), silent = TRUE)
  if (!inherits(formula, "try-error")) {
    # Deparsed wide, so that a long formula comes back as the one line it is
    # rather than as the lines the default width would break it into.
    rows[["Formula"]] <- paste(deparse(formula, width.cutoff = 500L),
                               collapse = " ")
  }
  rows[["Observations"]] <- fy_format_count(info$n)
  if (!is.na(info$events)) {
    rows[["Events"]] <- fy_format_count(info$events)
  }
  if (!is.na(info$person_time)) {
    rows[[fy_person_time_heading("Person-time", result$person_time, sep = " ")]] <-
      fy_format_person_time(info$person_time, result$person_time)
  }
  rows[["Exposure"]] <- exposure
  if (!is.null(result$modifier)) {
    rows[["Effect modifier"]] <- result$modifier
  }
  # Which cell the rows are read against, where they are read against one cell
  # rather than each against its own subgroup's reference level. A table of
  # combinations cannot be read without it.
  if (!is.null(result$reference_cell)) {
    rows[["Reference group"]] <- fy_reference_phrase(result)
  }
  rows[["Effect measure"]] <- result$measure_label
  rows[["Standard errors"]] <- if (isTRUE(result$robust)) "Robust" else "Model-based"
  # The confidence level is not among them: every column of numbers on the page
  # is headed with it -- "Odds ratio (95% CI)" -- so a row of its own says it
  # again in a place nobody reads it.

  fy_gt(
    data.frame(
      Field = names(rows),
      Value = unlist(rows, use.names = FALSE),
      stringsAsFactors = FALSE
    )
  )
}

# The joint test, plus the ratio of the effect between levels. The ratio is the
# size of the interaction, which is the part the p-value on its own leaves out.
#
# What the test is a test of is said by the heading over it and by the table of
# ratios under it, so the test is written as the test: the statistic, its
# degrees of freedom and its p-value, and nothing restating the hypothesis.
fy_interaction_html <- function(x, result) {
  tests <- result$interaction_tests %||% list(result$interaction_test)
  lead <- paste0(
    "<p class=\"lead\">",
    paste(vapply(tests, fy_test_phrase, character(1)), collapse = "; "),
    ".</p>"
  )

  ratios <- fy_ratio_table(x, result)
  paste0(lead, "\n", if (is.null(ratios)) "" else ratios)
}

fy_test_phrase <- function(test) {
  paste0(
    fy_escape(test$test), " = ", fy_format_number(test$statistic),
    " on ", test$df, " degree", if (isTRUE(test$df != 1)) "s" else "",
    " of freedom, p ",
    if (is.na(test$p.value)) "unavailable"
    else if (test$p.value < 0.001) "&lt; 0.001"
    else paste0("= ", fy_format_p(test$p.value))
  )
}

fy_ratio_table <- function(x, result) {
  info <- fy_infos(x)[[1L]]
  columns <- result$interaction_test$terms
  if (is.null(columns) || !length(columns)) {
    return(NULL)
  }
  L <- diag(length(info$coef))[match(columns, names(info$coef)), , drop = FALSE]
  rownames(L) <- columns
  est <- fy_lincom(info, L, ci_level = result$ci_level,
                   exponentiate = info$exponentiate)

  header <- if (info$exponentiate) {
    paste0("Ratio of ", tolower(info$measure_name), "s")
  } else {
    "Difference in effect"
  }
  out <- data.frame(
    Term = est$term_label,
    Value = paste0(
      fy_format_number(est$estimate), " (",
      fy_format_number(est$conf.low), " to ",
      fy_format_number(est$conf.high), ")"
    ),
    `p-value` = fy_format_p(est$p.value),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(out)[2] <- paste0(header, " (", round(result$ci_level * 100), "% CI)")
  fy_gt(out)
}

fy_estimates_table <- function(x, result) {
  est <- result$estimates
  info <- fy_infos(x)[[1L]]
  out <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
                    Variable = as.character(est$variable_label))
  if ("block_label" %in% names(est)) {
    out <- cbind(
      data.frame(Block = as.character(est$block_label),
                 Row = as.character(est$label),
                 check.names = FALSE, stringsAsFactors = FALSE),
      out
    )
  }
  if ("modifier_label" %in% names(est)) {
    out <- cbind(
      data.frame(Subgroup = as.character(est$modifier_label),
                 check.names = FALSE, stringsAsFactors = FALSE),
      out
    )
  }
  if (any(!is.na(est$level))) {
    out[["Level"]] <- ifelse(is.na(est$level), "", est$level)
  }

  header <- fy_estimate_header(info, isTRUE(result$adjusted), result$ci_level)
  out[[header]] <- ifelse(
    est$reference,
    paste0(fy_format_number(est$estimate), " (reference)"),
    paste0(fy_format_number(est$estimate), " (",
           fy_format_number(est$conf.low), " to ",
           fy_format_number(est$conf.high), ")")
  )
  out[["p-value"]] <- fy_format_p(est$p.value)
  # The same two counts the figure carries, held the way the figure held them:
  # a page reporting one thing and a figure beside it reporting another is
  # worse than either.
  pairs <- fy_count_pairs(est, result$counts_mode %||% "compared")
  out[["N"]] <- fy_format_pair(pairs$n)
  if (!all(is.na(est$events)) && !is.null(pairs$events)) {
    out[["Events"]] <- fy_format_pair(pairs$events)
  }
  if (!all(is.na(est$person_time))) {
    out[[fy_person_time_heading("Person-time", result$person_time, sep = " ")]] <-
      fy_format_person_time(est$person_time, result$person_time)
  }
  both <- "interaction_p_lrt" %in% names(est)
  if ("interaction_p" %in% names(est)) {
    heading <- if (both) "p for interaction (Wald)" else "p for interaction"
    out[[heading]] <- fy_format_p(est$interaction_p)
  }
  if (both) {
    out[["p for interaction (LR)"]] <- fy_format_p(est$interaction_p_lrt)
  }
  fy_gt(out)
}

# What the N and Events columns of that table are counts of, for the tables
# where that is not what a reader takes them to be.
#
# The note is written from the figure's own reading of its rows rather than
# from the sentences the figure carries: the table on this page holds N and
# Events whatever the figure was drawn with, so a figure drawn with `table =
# FALSE` still has a table here to explain.
fy_counts_note_html <- function(result) {
  reading <- result$counts_reading
  if (is.null(reading)) {
    return("")
  }
  note <- fy_counts_note(result$estimates,
                         by_level = isTRUE(reading[["by_level"]]),
                         by_outcome = isTRUE(reading[["by_outcome"]]),
                         counts = result$counts_mode %||% "compared")
  if (!length(note)) {
    return("")
  }
  paste0("
<p class=\"note\">", fy_escape(paste(note, collapse = " ")),
         "</p>")
}

fy_coefficient_table <- function(info, result) {
  L <- diag(length(info$coef))
  rownames(L) <- names(info$coef)
  est <- fy_lincom(info, L, ci_level = result$ci_level,
                   exponentiate = info$exponentiate)

  out <- data.frame(
    Coefficient = est$term_label,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  header <- if (info$exponentiate) info$measure_label else "Estimate"
  out[[paste0(header, " (", round(result$ci_level * 100), "% CI)")]] <- paste0(
    fy_format_number(est$estimate), " (",
    fy_format_number(est$conf.low), " to ",
    fy_format_number(est$conf.high), ")"
  )
  out[["SE"]] <- fy_format_number(est$se, digits = 3)
  out[["p-value"]] <- fy_format_p(est$p.value)
  fy_gt(out)
}

# The plot is embedded as a data URI rather than written beside the page, so
# that the report stays a single file that can be sent on.
fy_plot_html <- function(x, width = 10, height = NULL) {
  height <- fy_plot_height(x, height)
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)

  # Written as the figure itself rather than as the plain plot it also is, so
  # that the page shows what the screen and the exports show: the floor on the
  # width of the plot is applied by grid.draw.foresty(), which stripping the
  # class would step around.
  #
  # 200 dpi rather than 150: the page is read on screens that draw two device
  # pixels to the CSS pixel, and a forest plot is read from its numbers.
  ok <- try(
    ggplot2::ggsave(path, plot = x, width = width, height = height,
                    dpi = 200, units = "in"),
    silent = TRUE
  )
  if (inherits(ok, "try-error") || !file.exists(path)) {
    return("<p class=\"note\">The plot could not be rendered.</p>")
  }

  encoded <- fy_base64(path)
  if (is.null(encoded)) {
    return(paste0(
      "<p class=\"note\">The plot could not be embedded; install the ",
      "base64enc package to include it.</p>"
    ))
  }
  paste0("<img alt=\"Forest plot\" src=\"data:image/png;base64,", encoded, "\">")
}

# How tall the figure on the page is drawn, in inches.
#
# The rows are the whole of the panel, so the height is what sets them apart
# from one another: two fifths of an inch apiece. What stands above and below
# them does not grow with them -- a title of two lines, a column heading of
# three and an axis -- and that is an inch and a half whatever the figure
# reports, so it is added rather than shared out.
#
# The floor is three inches for the same reason. A figure of two rows given two
# and a half is mostly title: the rows are pressed into the little the headings
# and the axis leave, which is what a squashed forest plot is. The whitespace
# an over-tall figure leaves costs nothing beside that.
fy_plot_height <- function(x, height = NULL) {
  if (!is.null(height)) {
    return(height)
  }
  rows <- tryCatch(nrow(fy_result(x)$estimates), error = function(e) 0L)
  max(3, 1.6 + 0.4 * rows)
}

fy_base64 <- function(path) {
  # gt imports base64enc, so anything that can write the report can embed the
  # plot in it.
  if (requireNamespace("base64enc", quietly = TRUE)) {
    return(base64enc::base64encode(path))
  }
  NULL
}

fy_gt <- function(data) {
  tbl <- gt::gt(data)
  tbl <- gt::tab_options(tbl, table.font.size = gt::px(13),
                         data_row.padding = gt::px(4))
  as.character(gt::as_raw_html(tbl, inline_css = TRUE))
}

# Escaped for a text node and for an attribute alike, so that a variable name
# carrying a quote stays safe wherever the page happens to put it.
fy_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

fy_require <- function(package, purpose) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "the ", package, " package is needed to ", purpose,
      ". Install it with install.packages(\"", package, "\").",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

fy_report_css <- function() {
  paste0(
    "body{margin:0;background:#f6f7f9;color:#1a1a1a;",
    "font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;",
    "line-height:1.55}",
    "main{max-width:66rem;margin:0 auto;padding:2rem 1.5rem 4rem}",
    "h1{font-size:1.6rem;margin:0 0 .25rem;font-weight:650}",
    "p.date{margin:0 0 1.5rem;color:#6b7280;font-size:.85rem}",
    "h2{font-size:1.1rem;margin:0 0 .75rem;font-weight:650;color:#33475b}",
    "section{background:#fff;border:1px solid #e3e6ea;border-radius:8px;",
    "padding:1.25rem 1.5rem;margin-bottom:1.25rem;overflow-x:auto}",
    "p.lead{margin:0 0 1rem}",
    "p.note{margin:10px 0 0;color:#6b7280;font-style:italic;max-width:46em}",
    "img{max-width:100%;height:auto;display:block}"
  )
}
