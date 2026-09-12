#' Layout and style of a foresty figure
#'
#' Every drawing decision the figure makes is held in one object: the colors,
#' the marks, the rules between the rows, how the numbers beside the plot are
#' written, and where the columns sit. Pass the name of a style to
#' [foresty_main()] or [foresty_interaction()] as `layout = "jama"`, or build
#' one here and change any part of it.
#'
#' The styles are the ones a paper is usually asked for, read off the forest
#' plots those journals print and the layouts of the `meta` package, which
#' `foresty` follows here. They are approximations of a house style, not the
#' style sheet itself: a journal will still ask for its own fonts and sizes,
#' and every element below can be overridden.
#'
#' \describe{
#'   \item{`"classic"`}{The default. Black marks on white, a dashed line at the
#'     null, a rule under the column headings and thin rules between subgroups.}
#'   \item{`"jama"`}{Numbers to the left of the plot, rules above and below the
#'     whole block, navy squares, a solid line at the null, p-values without
#'     their leading zero (`.03`, `<.001`), and "No." and "P value" as
#'     headings. The title and subtitle that [foresty_interaction()] writes for
#'     itself are left off, the caption being where a journal puts them.}
#'   \item{`"nejm"`}{Numbers to the right, blue squares, en dashes between the
#'     confidence limits, "P value" as the heading.}
#'   \item{`"lancet"`}{As `"nejm"`, in the Lancet's blue, with the middle dot
#'     for a decimal point, so that 1.17 is written with one, and en dashes.}
#'   \item{`"bmj"`}{The BMJ's violet, grey rules, and "to" between the
#'     confidence limits.}
#'   \item{`"revman"`}{Cochrane's RevMan: blue squares and intervals written
#'     `1.15 [0.93, 1.42]`.}
#' }
#'
#' @param style Name of the style to start from, one of `"classic"`,
#'   `"jama"`, `"nejm"`, `"lancet"`, `"bmj"` and `"revman"`. A
#'   `foresty_layout` object is also accepted, and is then the thing being
#'   modified.
#' @param base_size Base font size in points. Everything else is drawn relative
#'   to it.
#' @param family Font family, as `"serif"` or `"Times New Roman"`. `NULL`
#'   leaves the device's default. How wide each column of text is made is
#'   worked out from a table of character widths rather than by asking the
#'   device, so a family far from the default one may leave a column slightly
#'   wide or slightly narrow; the columns still hold what is in them.
#' @param color One color for the estimates and their intervals, which is the
#'   usual thing to change. `palette` changes them apart, and `color_by` draws
#'   the rows in colors of their own instead. [foresty_colors()] names a
#'   color of a ColorBrewer palette without pasting a hex code, as
#'   `color = foresty_colors("Dark2")[3]`.
#' @param color_by What the colors change with, for a figure drawn in more
#'   than one. `"none"`, the default, draws the whole figure in `color`.
#'   `"category"` gives every category of the rows a color of its own: the
#'   levels of a categorical exposure -- "Yes" and "No" -- where the rows are
#'   levels, and the subgroups of the modifier where they are subgroups, which
#'   is what an interaction figure draws. A category keeps its color wherever
#'   it appears, so a combined figure reads across its blocks. `"row"` gives
#'   every row a color, whatever it is of. The reference level of a
#'   categorical exposure is drawn hollow either way, being a definition rather
#'   than an estimate. No legend is drawn: every row is labelled already, and a
#'   legend repeating the labels is a second copy of them to keep in step.
#' @param colors The colors `color_by` draws the categories in, in order,
#'   and cycled where there are more categories than colors. The name of a
#'   ColorBrewer palette -- `"Dark2"`, the default, `"Set1"` or `"Set2"` -- or
#'   the colors themselves, as `c("#1B9E77", "#D95F02")`. [foresty_colors()]
#'   builds one starting from a chosen place in a palette, as
#'   `colors = foresty_colors("Dark2", start = 3)`.
#' @param theme The `ggplot2` theme the plot itself is drawn on: `"void"`, the
#'   default, which is the plain panel a forest plot is usually drawn as, or
#'   the name of one of `ggplot2`'s own -- `"minimal"`, `"bw"`, `"classic"`,
#'   `"light"`, `"linedraw"`, `"grey"` or `"dark"` -- for its background, its
#'   border and its grid. A theme object is also accepted, as is a theme
#'   function, which is called with `base_size` and `base_family` and so has to
#'   take them, as `ggplot2`'s own do.
#'   It is the plot's theme, not the figure's: the columns of numbers beside it
#'   are a table rather than a plot and keep their own. Everything the layout
#'   sets -- the sizes, the colors of the text, whether the axis line and the
#'   grid are drawn -- is applied over it, so `grid = TRUE` and `theme = "bw"`
#'   are not two answers to the same question.
#' @param palette Named character vector overriding single colors, as
#'   `c(estimate = "#B24745", null = "grey60")`. The names are `estimate`,
#'   `border` (the outline of the marks), `reference` (the fill of the
#'   reference level's hollow mark), `interval`, `null`, `rule`, `band`,
#'   `text`, `header`, `group` and `axis`.
#' @param point_shape,point_size Plotting symbol and its size. The default is a
#'   filled square, as a forest plot is usually drawn with.
#' @param emphasis_shape,emphasis_height,emphasis_size,emphasis_face How a row
#'   singled out for emphasis is drawn -- the overall estimate on a
#'   [foresty_combine()] figure, which a reader should be able to find without
#'   hunting for it.
#'
#'   `emphasis_shape` is a plotting symbol, and the row is drawn as the other
#'   rows are -- a mark on an interval -- in that symbol at `emphasis_size`,
#'   which may be left `NULL` for `point_size` times 1.5. The default, `23`, is
#'   the filled diamond, which says the row is the summary while leaving its
#'   interval to be read as the others are.
#'
#'   `emphasis_shape = "diamond"` draws instead the wide summary diamond a paper
#'   draws: a filled diamond whose two side vertices sit on the confidence
#'   limits and whose apex sits on the estimate, drawn in place of the interval
#'   rather than on top of it. `emphasis_height` is how tall that diamond is, as
#'   a fraction of the space between one row and the next, and `emphasis_size`
#'   says nothing about it.
#'
#'   `emphasis_face` is the font face of the row's label, and applies either
#'   way.
#' @param emphasis_gap Whether a row singled out for emphasis is set apart from
#'   the rest by a rule of its own, drawn whether or not `separators` rules
#'   between subgroups, so that the overall estimate is read as its own thing
#'   rather than as another subgroup.
#' @param interval_width Line width of the confidence intervals.
#' @param null_line Line type of the line at the null, as `"dashed"` or
#'   `"solid"`. `"none"` draws none.
#' @param grid Whether to draw vertical grid lines behind the estimates.
#' @param axis_line Whether to draw the axis line under the plot.
#' @param band Whether to shade alternate rows, which helps a reader carry the
#'   eye across a wide table of numbers.
#' @param rules Where to rule the figure: `"header"` draws a line under the
#'   column headings, `"full"` adds one above them and one under the last row,
#'   and `"none"` draws neither.
#' @param separators Whether to draw a thin rule between one subgroup and the
#'   next.
#' @param group_position Where the name of a subgroup goes: `"row"` puts it on
#'   a row of its own above the rows it covers, and `"column"` puts it in a
#'   column of its own to the left of them.
#' @param table_side Which side of the plot the numbers are written on,
#'   `"right"` or `"left"`.
#' @param header_face,group_face Font face of the column headings and of the
#'   subgroup names, as `"bold"` or `"plain"`.
#' @param digits Digits the estimates are written to.
#' @param p_format How p-values are written. `"default"` writes `0.032` and
#'   `<0.001`; `"jama"` drops the leading zero and rounds as JAMA asks, to
#'   three places up to `0.01` and two above it.
#' @param decimal_mark Decimal point, `"."` unless a journal asks otherwise.
#' @param ci_separator What goes between the confidence limits. The default
#'   picks `"-"` when every number on the figure is positive and `" to "` when
#'   one is not, a hyphen between negative numbers being unreadable.
#' @param ci_brackets The pair of brackets the interval is written in, as
#'   `c("[", "]")`.
#' @param column_gap Space between one column of numbers and the next, in ems,
#'   an em being about two digits wide. Lower it to draw the numbers tighter
#'   and leave the plot more of the figure.
#' @param counts What the columns of counts hold on a row that compares two
#'   groups of people. `"compared"`, the default, holds both of them, the row's
#'   own group and the group its estimate is compared with, written as
#'   `822 vs 468`: the odds ratio on that row came out of those 822 and those
#'   468, and a row carrying one of the two numbers leaves the reader to find
#'   the other. `"row"` holds the row's own group alone, which is what a figure
#'   drawn before this option existed held. The rows that compare no two groups
#'   of people are unaffected either way -- a step along a continuous exposure,
#'   and the reference rows, which are the group the others are compared with
#'   rather than a comparison of their own. See *What the counts beside the
#'   rows count* in [foresty_main()].
#' @param headings Named character vector renaming the column headings, as
#'   `c(n = "No. of patients")`. The names are `estimate`, `label`, `p`, `n`,
#'   `events`, `person_time`, `interaction_p`, and `interaction_p_wald` and
#'   `interaction_p_lrt` for a figure reporting both tests of the interaction.
#'   The estimate's heading is
#'   written from the model, naming the measure and the confidence level; the
#'   outcome is named on the axis under the plot rather than twice over.
#'   `label` heads the column of row labels, and defaults to `"Exposure"`
#'   in [foresty_main()], the modifier's name in [foresty_interaction()] and
#'   `"Subgroup"` in [foresty_combine()], which leaves it unheaded when the
#'   figure carries an overall estimate as well as subgroups. What `n` and
#'   `events` are counts of, which is not the same thing for every figure, is
#'   in *What the counts beside the rows count* in [foresty_main()]; rename
#'   them to say it where a caption does not.
#' @param xlim Limits of the plot, as `c(0.5, 4)`. Intervals running past them
#'   are drawn with an arrow at the end, which is what keeps one wide interval
#'   from flattening the rest of the figure.
#' @param arrows Two labels for the directions of the effect, as
#'   `c("Favours treatment", "Favours control")`, drawn under the plot with an
#'   arrow apiece. `NULL` draws none.
#' @param arrows_position Whether those labels go at the `"bottom"` of the plot
#'   or at the `"top"`.
#' @param plot_width How much of the figure the plot itself takes. `NULL`, the
#'   default, gives it whatever the columns of text leave, so that widening the
#'   figure widens the plot and nothing else. A fraction, as `0.6`, gives it
#'   that share of the figure whatever the figure is drawn at, and the columns
#'   of text share the rest. A number of centimetres, or a [grid::unit()],
#'   fixes it exactly.
#' @param min_plot_width The least of the figure the plot is allowed to be left
#'   with, as a fraction. What the columns of text leave depends on the width
#'   the figure is drawn at, which is not known while it is being built, so a
#'   wide table -- a survival model carrying N, events and person-time -- can
#'   leave the plot a centimetre and squash the axis under it into a row of
#'   overprinted numbers. This is the floor: where the columns of text would
#'   leave the plot less than this share of the width it is being drawn at, the
#'   plot is given that share and the columns of text share the rest in
#'   proportion to what they hold. `0` turns the floor off and lets the plot
#'   have whatever is left, however little that is. It has no effect where
#'   `plot_width` says outright how wide the plot is.
#' @param auto_labels Whether [foresty_interaction()] writes its own title and
#'   subtitle. The journal styles leave them off, since that text belongs in
#'   the caption; a title passed by hand is always drawn.
#'
#' @return An object of class `foresty_layout`.
#'
#' @seealso [foresty_main()], [foresty_interaction()].
#'
#' @examples
#' fit <- glm(asthma ~ no2 + sex + maternal_smoking + maternal_age,
#'            family = binomial, data = foresty_cohort)
#'
#' foresty_interaction(fit, "no2", "sex", table = TRUE, layout = "jama")
#'
#' # A style, changed where it needs to be.
#' foresty_interaction(
#'   fit, "no2", "sex", table = TRUE,
#'   layout = foresty_layout("jama", color = "#B24745", base_size = 11)
#' )
#'
#' # Trimming a wide interval rather than letting it flatten the figure.
#' fit_urban <- glm(asthma ~ urbanicity + sex + maternal_age,
#'                  family = binomial, data = foresty_cohort)
#' foresty_main(list(fit_urban), "urbanicity", table = TRUE,
#'              layout = foresty_layout("classic", xlim = c(0.8, 2.5),
#'                                      arrows = c("Lower risk", "Higher risk")))
#'
#' @export
foresty_layout <- function(style = c("classic", "jama", "nejm", "lancet",
                                     "bmj", "revman"),
                           base_size = NULL,
                           family = NULL,
                           color = NULL,
                           color_by = NULL,
                           colors = NULL,
                           theme = NULL,
                           palette = NULL,
                           point_shape = NULL,
                           point_size = NULL,
                           emphasis_shape = NULL,
                           emphasis_height = NULL,
                           emphasis_size = NULL,
                           emphasis_face = NULL,
                           emphasis_gap = NULL,
                           interval_width = NULL,
                           null_line = NULL,
                           grid = NULL,
                           axis_line = NULL,
                           band = NULL,
                           rules = NULL,
                           separators = NULL,
                           group_position = NULL,
                           table_side = NULL,
                           header_face = NULL,
                           group_face = NULL,
                           digits = NULL,
                           p_format = NULL,
                           decimal_mark = NULL,
                           ci_separator = NULL,
                           ci_brackets = NULL,
                           column_gap = NULL,
                           counts = NULL,
                           headings = NULL,
                           xlim = NULL,
                           arrows = NULL,
                           arrows_position = NULL,
                           plot_width = NULL,
                           min_plot_width = NULL,
                           auto_labels = NULL) {
  out <- if (inherits(style, "foresty_layout")) {
    style
  } else {
    fy_style(match.arg(style))
  }

  if (!is.null(color)) {
    checkmate::assert_string(color)
    out$palette[c("estimate", "border", "interval")] <- color
  }
  if (!is.null(palette)) {
    out$palette <- fy_merge_palette(out$palette, palette)
  }
  if (!is.null(headings)) {
    out$headings <- fy_merge_headings(out$headings, headings)
  }

  # Everything else is a single value replaced as it stands.
  given <- list(
    base_size = base_size, family = family, color_by = color_by,
    colors = colors, theme = theme, point_shape = point_shape,
    point_size = point_size, emphasis_shape = emphasis_shape,
    emphasis_height = emphasis_height, emphasis_size = emphasis_size,
    emphasis_face = emphasis_face,
    emphasis_gap = emphasis_gap, interval_width = interval_width,
    null_line = null_line, grid = grid, axis_line = axis_line, band = band,
    rules = rules, separators = separators, group_position = group_position,
    table_side = table_side, header_face = header_face,
    group_face = group_face, digits = digits, p_format = p_format,
    decimal_mark = decimal_mark, ci_separator = ci_separator,
    ci_brackets = ci_brackets, column_gap = column_gap, counts = counts,
    xlim = xlim,
    arrows = arrows,
    arrows_position = arrows_position, plot_width = plot_width,
    min_plot_width = min_plot_width, auto_labels = auto_labels
  )
  for (nm in names(given)) {
    if (!is.null(given[[nm]])) {
      out[[nm]] <- given[[nm]]
    }
  }

  fy_check_layout(out)
}

#' @export
print.foresty_layout <- function(x, ...) {
  cat("<foresty layout: ", x$style, ">\n", sep = "")
  cat("  size      ", x$base_size, " pt",
      if (!is.null(x$family)) paste0(", ", x$family), "\n", sep = "")
  cat("  colors   ",
      paste0(names(x$palette), " ", unlist(x$palette), collapse = ", "),
      "\n", sep = "")
  if (fy_colors_rows(x)) {
    colors <- fy_layout_colors(x$colors)
    cat("  one per   ", x$color_by, ", from ",
        paste(utils::head(colors, 3), collapse = ", "),
        if (length(colors) > 3L) ", ...", "\n", sep = "")
  }
  cat("  numbers   ", x$digits, " digits, p-values ", x$p_format,
      ", intervals ", fy_ci_example(x), "\n", sep = "")
  cat("  columns   ", x$table_side, ", subgroups by ", x$group_position,
      "\n", sep = "")
  invisible(x)
}

fy_ci_example <- function(layout) {
  sep <- if (is.null(layout$ci_separator)) "-" else layout$ci_separator
  paste0(layout$ci_brackets[1], "0.93", sep, "1.42", layout$ci_brackets[2])
}

# A layout as given by the caller: a name, an object, or nothing.
fy_as_layout <- function(layout) {
  if (is.null(layout)) {
    return(fy_style("classic"))
  }
  if (inherits(layout, "foresty_layout")) {
    return(fy_check_layout(layout))
  }
  if (is.character(layout) && length(layout) == 1L) {
    return(foresty_layout(layout))
  }
  stop(
    "`layout` must be the name of a style, as `layout = \"jama\"`, or a ",
    "layout built by foresty_layout()",
    call. = FALSE
  )
}

# The styles ------------------------------------------------------------------

# What every style is a departure from. A style names only what it changes.
fy_layout_defaults <- function() {
  list(
    style = "classic",
    base_size = 11,
    family = NULL,
    palette = list(
      estimate = "black",
      border = "black",
      reference = "white",
      interval = "black",
      null = "grey45",
      rule = "grey25",
      band = "grey94",
      text = "black",
      header = "black",
      group = "black",
      axis = "grey25"
    ),
    # One color for the whole figure, which is what a forest plot is usually
    # drawn in. `color_by` is what makes `colors` mean anything.
    color_by = "none",
    colors = NULL,
    # The panel a forest plot is drawn on carries nothing of its own: the rules,
    # the shading and the line at the null are drawn by the figure, at heights
    # it worked out, so a theme under them would be a second set of lines.
    theme = "void",
    point_shape = 22,
    point_size = 2.3,
    # A row singled out for emphasis is the overall estimate on a combined
    # figure. It is drawn as the subgroups are -- a mark on its interval -- in
    # a filled diamond half again the size of their squares, its label in bold,
    # and set apart from them by a rule of its own. The reader then reads every
    # row of the figure the same way, and the diamond says which row is the
    # summary. `emphasis_shape = "diamond"` draws the wide summary diamond
    # instead, spanning the interval in place of it.
    emphasis_shape = 23,
    emphasis_height = 0.25,
    emphasis_size = NULL,
    emphasis_face = "bold",
    emphasis_gap = TRUE,
    interval_width = 0.55,
    null_line = "dashed",
    grid = FALSE,
    axis_line = TRUE,
    band = FALSE,
    rules = "header",
    separators = TRUE,
    group_position = "row",
    table_side = "right",
    header_face = "bold",
    group_face = "bold",
    digits = 2,
    p_format = "default",
    decimal_mark = ".",
    # NULL is "a hyphen unless a number is negative"; see fy_ci_separator().
    ci_separator = NULL,
    ci_brackets = c("(", ")"),
    column_gap = 0.9,
    # Whether the counts beside a row are the row's own or the row's beside the
    # group it is compared with. See the `counts` argument.
    counts = "compared",
    headings = list(
      # NULL is "the one the model wrote for itself".
      estimate = NULL,
      # NULL is "the one the function wrote for itself": "Exposure" over the
      # rows of foresty_main(), the modifier's name over those of
      # foresty_interaction(), "Subgroup" over those of foresty_combine().
      label = NULL,
      p = "p-value",
      n = "N",
      events = "Events",
      person_time = "Person-time",
      interaction_p = "p for\ninteraction",
      # Only used by a figure reporting both tests of the interaction, where
      # the column has to say which of them it holds.
      interaction_p_wald = "p for\ninteraction\n(Wald)",
      interaction_p_lrt = "p for\ninteraction\n(LR)"
    ),
    xlim = NULL,
    arrows = NULL,
    arrows_position = "bottom",
    plot_width = NULL,
    # A quarter is about the least a plot can be and still be a plot rather than
    # a decoration. Higher, and the columns of text are squeezed enough on a
    # crowded figure that one heading runs into the next, which is the same
    # problem moved rather than solved; see fy_floor_plot_width().
    min_plot_width = 0.25,
    auto_labels = TRUE
  )
}

fy_style <- function(name) {
  out <- fy_layout_defaults()
  out$style <- name
  changed <- switch(
    name,
    classic = list(),
    # Numbers to the left of the plot, the whole block ruled top and bottom,
    # p-values written the way the journal asks for them.
    jama = list(
      base_size = 10,
      palette = list(estimate = "#1A476F", border = "#1A476F",
                     interval = "#1A476F", null = "black", rule = "black",
                     axis = "black"),
      null_line = "solid",
      rules = "full",
      separators = FALSE,
      table_side = "left",
      p_format = "jama",
      headings = list(p = "P value", n = "No.", events = "No. of events",
                      interaction_p = "P value for\ninteraction",
                      interaction_p_wald = "Wald P value\nfor interaction",
                      interaction_p_lrt = "LR P value\nfor interaction"),
      auto_labels = FALSE
    ),
    nejm = list(
      base_size = 10,
      palette = list(estimate = "#0072B5", border = "#0072B5",
                     interval = "#0072B5", null = "grey40", rule = "black",
                     axis = "black"),
      separators = FALSE,
      ci_separator = "\u2013",
      headings = list(p = "P value", n = "No.",
                      interaction_p = "P value for\ninteraction",
                      interaction_p_wald = "Wald P value\nfor interaction",
                      interaction_p_lrt = "LR P value\nfor interaction"),
      auto_labels = FALSE
    ),
    lancet = list(
      base_size = 10,
      palette = list(estimate = "#00468B", border = "#00468B",
                     interval = "#00468B", null = "grey40", rule = "black",
                     axis = "black"),
      separators = FALSE,
      decimal_mark = "\u00b7",
      ci_separator = "\u2013",
      headings = list(p = "p value", n = "Number",
                      interaction_p = "p value for\ninteraction",
                      interaction_p_wald = "Wald p value\nfor interaction",
                      interaction_p_lrt = "LR p value\nfor interaction"),
      auto_labels = FALSE
    ),
    bmj = list(
      base_size = 10,
      palette = list(estimate = "#6b58a6", border = "#6b58a6",
                     interval = "#6b58a6", null = "#a7a9ac",
                     rule = "#a7a9ac", axis = "#a7a9ac"),
      separators = FALSE,
      ci_separator = " to ",
      headings = list(p = "P value", n = "No.",
                      interaction_p = "P value for\ninteraction",
                      interaction_p_wald = "Wald P value\nfor interaction",
                      interaction_p_lrt = "LR P value\nfor interaction"),
      auto_labels = FALSE
    ),
    # RevMan writes an interval as 1.15 [0.93, 1.42].
    revman = list(
      base_size = 10,
      palette = list(estimate = "#4472C4", border = "#2F528F",
                     interval = "black", null = "black", rule = "black",
                     axis = "black"),
      null_line = "solid",
      ci_separator = ", ",
      ci_brackets = c("[", "]"),
      headings = list(p = "P", n = "Total"),
      auto_labels = FALSE
    ),
    stop("unknown style \"", name, "\"", call. = FALSE)
  )

  for (nm in names(changed)) {
    out[[nm]] <- if (nm %in% c("palette", "headings")) {
      utils::modifyList(out[[nm]], changed[[nm]])
    } else {
      changed[[nm]]
    }
  }
  structure(out, class = "foresty_layout")
}

fy_merge_palette <- function(current, given) {
  if (!is.character(given) || is.null(names(given)) ||
      any(names(given) == "") || anyNA(given) ||
      anyDuplicated(names(given))) {
    stop("`palette` must be a named character vector naming each color ",
         "once, as `c(estimate = \"#B24745\")`", call. = FALSE)
  }
  unknown <- setdiff(names(given), names(current))
  if (length(unknown)) {
    stop(
      "`palette` names a color foresty does not draw: ",
      paste(unknown, collapse = ", "), ". The ones it does: ",
      paste(names(current), collapse = ", "), ".",
      call. = FALSE
    )
  }
  utils::modifyList(current, as.list(given))
}

fy_merge_headings <- function(current, given) {
  # A heading may be empty -- that is how a column is left unheaded -- but not
  # missing, and naming the same column twice would leave whichever of the two
  # modifyList() happened to keep.
  if (!is.character(given) || is.null(names(given)) ||
      any(names(given) == "") || anyNA(given) ||
      anyDuplicated(names(given))) {
    stop("`headings` must be a named character vector heading each column ",
         "once, as `c(n = \"No. of patients\")`", call. = FALSE)
  }
  unknown <- setdiff(names(given), names(current))
  if (length(unknown)) {
    stop(
      "`headings` names a column foresty does not draw: ",
      paste(unknown, collapse = ", "), ". The ones it does: ",
      paste(names(current), collapse = ", "), ".",
      call. = FALSE
    )
  }
  utils::modifyList(current, as.list(given))
}

fy_check_layout <- function(layout) {
  checkmate::assert_number(layout$base_size, lower = 3, upper = 40)
  checkmate::assert_number(layout$point_size, lower = 0)
  checkmate::assert_number(layout$interval_width, lower = 0)
  checkmate::assert_number(layout$digits, lower = 0, upper = 8)
  checkmate::assert_number(layout$column_gap, lower = 0, upper = 20)
  checkmate::assert_number(layout$min_plot_width, lower = 0, upper = 0.9)
  checkmate::assert_flag(layout$grid)
  checkmate::assert_flag(layout$axis_line)
  checkmate::assert_flag(layout$band)
  checkmate::assert_flag(layout$separators)
  checkmate::assert_flag(layout$auto_labels)
  checkmate::assert_flag(layout$emphasis_gap)
  checkmate::assert_number(layout$emphasis_height, lower = 0, upper = 0.5)
  if (!is.null(layout$emphasis_size)) {
    checkmate::assert_number(layout$emphasis_size, lower = 0)
  }
  if (!is.null(layout$family)) {
    checkmate::assert_string(layout$family, min.chars = 1L,
                             .var.name = "family")
  }
  fy_check_shape(layout$point_shape, "point_shape")
  # Either the summary diamond, which is drawn from the interval, or a plotting
  # symbol, which is drawn on it.
  if (is.character(layout$emphasis_shape)) {
    layout$emphasis_shape <- match.arg(layout$emphasis_shape, "diamond")
  } else {
    fy_check_shape(layout$emphasis_shape, "emphasis_shape")
  }
  layout$null_line <- match.arg(layout$null_line,
                                c("dashed", "solid", "none"))
  layout$counts <- match.arg(layout$counts, c("compared", "row"))
  checkmate::assert_string(layout$decimal_mark, min.chars = 1L,
                           .var.name = "decimal_mark")
  if (!is.null(layout$ci_separator)) {
    checkmate::assert_string(layout$ci_separator, .var.name = "ci_separator")
  }
  fy_check_plot_width(layout$plot_width)
  # The palette is a color apiece, and a name that is not one is drawn as
  # black by grid without a word said about it.
  wrong <- fy_not_colors(unlist(layout$palette, use.names = FALSE))
  if (length(wrong)) {
    stop(
      "the palette holds ",
      paste0("\"", wrong, "\"", collapse = ", "),
      ", which ",
      if (length(wrong) == 1L) "is not a color" else "are not colors",
      " R can draw. Give a hex code, as \"#B24745\", or one of the names in ",
      "grDevices::colors().",
      call. = FALSE
    )
  }
  layout$color_by <- match.arg(layout$color_by %||% "none",
                                c("none", "category", "row"))
  if (!is.null(layout$colors)) {
    checkmate::assert_character(layout$colors, min.len = 1L,
                                any.missing = FALSE)
    # One name is a palette or a color; several are the colors themselves,
    # and every one of them has to be one. A name that is not a color is
    # drawn as black by grid rather than refused, so the row it was meant for
    # is drawn in the wrong color and nothing says so.
    named_palette <- length(layout$colors) == 1L &&
      layout$colors %in% names(fy_brewer_palettes)
    unknown <- !named_palette && length(fy_not_colors(layout$colors)) > 0L
    if (unknown) {
      stop(
        "`colors` is the name of a palette -- ",
        paste0("\"", names(fy_brewer_palettes), "\"", collapse = ", "),
        " -- or the colors themselves, as `c(\"#1B9E77\", \"#D95F02\")`. ",
        "foresty_colors(\"Dark2\", start = 3) builds one starting where you ",
        "want it to.",
        call. = FALSE
      )
    }
  }
  # Resolved here rather than when the figure is drawn, so that a theme that
  # does not exist is refused where it was named.
  fy_base_theme(layout)
  layout$rules <- match.arg(layout$rules, c("header", "full", "none"))
  layout$group_position <- match.arg(layout$group_position, c("row", "column"))
  layout$table_side <- match.arg(layout$table_side, c("right", "left"))
  layout$p_format <- match.arg(layout$p_format, c("default", "jama"))
  layout$arrows_position <- match.arg(layout$arrows_position,
                                      c("bottom", "top"))
  if (!is.null(layout$xlim)) {
    checkmate::assert_numeric(layout$xlim, len = 2L, any.missing = FALSE,
                              sorted = TRUE)
  }
  if (!is.null(layout$arrows)) {
    checkmate::assert_character(layout$arrows, len = 2L, any.missing = FALSE)
  }
  checkmate::assert_character(layout$ci_brackets, len = 2L,
                              any.missing = FALSE)
  layout
}

# A plotting symbol, which ggplot2 takes either as one of its 26 numbered
# shapes or as the single character to draw instead.
fy_check_shape <- function(shape, what) {
  if (is.character(shape)) {
    checkmate::assert_string(shape, min.chars = 1L, .var.name = what)
    if (nchar(shape) > 1L) {
      stop(
        "`", what, "` is a plotting symbol: a number from 0 to 25, or the ",
        "single character to draw in its place. \"", shape, "\" is neither.",
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }
  checkmate::assert_number(shape, lower = 0, upper = 25, .var.name = what)
  invisible(TRUE)
}

# How wide the plot is drawn: whatever the columns of text leave, a share of
# the figure, a number of centimetres, or a unit saying so outright. Checked
# here rather than when the figure is drawn, so that a width that is not one is
# refused where it was named.
fy_check_plot_width <- function(width) {
  if (is.null(width) || grid::is.unit(width)) {
    if (grid::is.unit(width) && length(width) != 1L) {
      stop("`plot_width` must be one width, not ", length(width),
           call. = FALSE)
    }
    return(invisible(TRUE))
  }
  checkmate::assert_number(width, lower = 0, finite = TRUE,
                           .var.name = "plot_width")
  invisible(TRUE)
}

# Whether each of these is a color R can draw: a hex code, with or without an
# alpha pair, or one of the names R knows. Vectorised, since a palette and a
# set of category colors are both checked a whole vector at a time.
fy_is_color <- function(x) {
  x <- as.character(x)
  hex <- grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x)
  !is.na(x) & (hex | x %in% grDevices::colors())
}

# The ones that are not, named, for a message that says which to fix.
fy_not_colors <- function(x) {
  unique(as.character(x)[!fy_is_color(x)])
}

# The themes ------------------------------------------------------------------

# The ggplot2 themes a figure can be drawn on, by the name the layout takes.
fy_themes <- function() {
  list(
    void = ggplot2::theme_void,
    minimal = ggplot2::theme_minimal,
    bw = ggplot2::theme_bw,
    classic = ggplot2::theme_classic,
    light = ggplot2::theme_light,
    linedraw = ggplot2::theme_linedraw,
    grey = ggplot2::theme_grey,
    gray = ggplot2::theme_grey,
    dark = ggplot2::theme_dark
  )
}

# The theme the plot is drawn on, at the size and in the family the layout asks
# for. A theme object is taken as it stands, since it carries its own sizes.
fy_base_theme <- function(layout) {
  theme <- layout$theme %||% "void"
  if (inherits(theme, "theme")) {
    return(theme)
  }
  if (is.function(theme)) {
    return(theme(base_size = layout$base_size,
                 base_family = layout$family %||% ""))
  }
  themes <- fy_themes()
  if (!is.character(theme) || length(theme) != 1L || !theme %in% names(themes)) {
    stop(
      "`theme` is the name of a ggplot2 theme -- ",
      paste0("\"", setdiff(names(themes), "gray"), "\"", collapse = ", "),
      " -- or a theme object, as `theme = ggplot2::theme_bw()`",
      call. = FALSE
    )
  }
  themes[[theme]](base_size = layout$base_size,
                  base_family = layout$family %||% "")
}

# A mark drawn for emphasis is larger than the rest, so that it is found
# without being looked for. NULL is half again the size of the others.
fy_emphasis_size <- function(layout) {
  layout$emphasis_size %||% (layout$point_size * 1.5)
}

# Sizes -----------------------------------------------------------------------

# geom_text() sizes text in millimetres, themes in points.
fy_text_size <- function(pt) pt / ggplot2::.pt

# How wide a column of text is, in centimetres, at a given size. The figure is
# laid out in ems and converted here, so that a column is as wide as the widest
# thing in it and no wider.
fy_text_width <- function(em, size_pt) {
  grid::unit(em * size_pt / 28.3465, "cm")
}

# The width of each character, in thousandths of an em, as Helvetica sets them,
# which is what the device draws with unless a family is named and is close
# enough to Times and to the other sans faces for a column of numbers.
fy_char_em <- local({
  named <- function(chars, width) {
    stats::setNames(rep(width / 1000, length(chars)), chars)
  }
  unicode <- function(...) intToUtf8(c(...), multiple = TRUE)
  c(
    named(as.character(0:9), 556),
    named(c(" ", ".", ",", ":", ";"), 278),
    named(c("'", "|"), 200),
    named(c("\"", "*"), 370),
    named(c("(", ")", "-"), 333),
    named(c("[", "]", "/", "!"), 278),
    named(c("<", ">", "=", "+", "$", "?"), 584),
    named(c("%", "@"), 900),
    # An en dash between the confidence limits, a middle dot for the decimal
    # point, and the rest of the marks a journal's style asks for. Written by
    # their code points so that the file stays plain ASCII.
    named(unicode(0x2013), 556),
    named(unicode(0x00b7, 0x2019), 278),
    named(unicode(0x2014), 1000),
    named(unicode(0x2264, 0x2265, 0x00b1, 0x00d7), 584),
    named(c("a", "b", "d", "e", "g", "h", "n", "o", "p", "q", "u"), 556),
    named(c("c", "k", "s", "v", "x", "y", "z"), 500),
    named(c("i", "j", "l"), 222),
    named(c("f", "t"), 278),
    named("r", 333),
    named("m", 833),
    named("w", 722),
    named(c("A", "B", "E", "K", "R"), 686),
    named(c("C", "D", "G", "H", "N", "O", "Q", "U", "V", "X", "Y"), 733),
    named(c("F", "L", "P", "S", "T", "Z", "J"), 600),
    named("I", 278),
    named("M", 833),
    named("W", 944)
  )
})

# How wide a string is, in ems, counting the longest of its lines. A digit is
# a little over half an em in the fonts a paper is set in, but a decimal point
# is half that again and a bracket narrower still, so measuring a column of
# numbers by the count of its characters leaves it a fifth wider than it has to
# be and takes that width from the plot. Bold is set a little wider than plain.
fy_em <- function(x, face = "plain") {
  if (!length(x)) {
    return(0)
  }
  lines <- unlist(strsplit(as.character(x), "\n", fixed = TRUE))
  if (!length(lines)) {
    return(0)
  }
  widths <- vapply(lines, function(line) {
    chars <- strsplit(line, "", fixed = TRUE)[[1L]]
    if (!length(chars)) {
      return(0)
    }
    em <- unname(fy_char_em[chars])
    # A character the table does not name -- an accent, a Greek letter, a
    # Japanese one -- is measured by the columns it takes, which is one for a
    # letter and two for a full-width one.
    unknown <- is.na(em)
    if (any(unknown)) {
      em[unknown] <- 0.5 * nchar(chars[unknown], type = "width")
    }
    sum(em)
  }, numeric(1), USE.NAMES = FALSE)

  max(widths, 0) * if (identical(face, "bold")) 1.06 else 1
}

fy_lines <- function(x) {
  if (!length(x)) {
    return(0L)
  }
  max(lengths(strsplit(as.character(x), "\n", fixed = TRUE)), 0L)
}
