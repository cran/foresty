# The colors a figure is drawn in, where they are more than one.
#
# A forest plot is usually drawn in one color, and `color` in
# foresty_layout() is that color. `color_by` is the other thing a figure is
# sometimes asked for: a color per category of the rows, so that the two
# levels of a modifier, or the levels of a categorical exposure, are told apart
# by their color as well as by their labels. The colors themselves come from
# here.

#' The qualitative palettes a figure can be colored from
#'
#' The ColorBrewer qualitative palettes, as `RColorBrewer::brewer.pal()` gives
#' them, so that `color` and `colors` in [foresty_layout()] can be answered
#' by naming a palette and a place in it rather than by pasting a hex code.
#' The values are the palettes' own and are held here so that the package
#' needs nothing installed to draw in them.
#'
#' @param palette Name of the palette: `"Dark2"`, the default, which is the
#'   one a figure printed in color is usually drawn from, `"Set1"` or
#'   `"Set2"`.
#' @param n How many colors to return. `NULL`, the default, returns the whole
#'   palette. More than the palette holds cycles round it, since a figure with
#'   more categories than the palette has colors has to draw them somehow.
#' @param start Which color of the palette to begin at, counting from 1. The
#'   palette is rotated rather than trimmed, so that every color is still
#'   available after it.
#'
#' @return A character vector of colors.
#'
#' @seealso [foresty_layout()], whose `color` and `colors` arguments these
#'   are for.
#'
#' @examples
#' foresty_colors("Dark2")
#'
#' # The third color of Dark2, for one figure drawn in one color.
#' foresty_colors("Dark2")[3]
#'
#' # A palette beginning there, for a figure whose rows are colored by their
#' # category.
#' foresty_colors("Dark2", start = 3)
#'
#' @export
foresty_colors <- function(palette = c("Dark2", "Set1", "Set2"), n = NULL,
                            start = 1) {
  palette <- match.arg(palette)
  out <- fy_brewer_palettes[[palette]]
  checkmate::assert_int(start, lower = 1)
  start <- (as.integer(start) - 1L) %% length(out) + 1L
  out <- out[c(seq(start, length(out)), seq_len(start - 1L))]
  if (is.null(n)) {
    return(out)
  }
  checkmate::assert_int(n, lower = 1)
  out[(seq_len(n) - 1L) %% length(out) + 1L]
}

# ColorBrewer's qualitative palettes, taken from RColorBrewer::brewer.pal().
fy_brewer_palettes <- list(
  Dark2 = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02",
            "#A6761D", "#666666"),
  Set1 = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
           "#A65628", "#F781BF", "#999999"),
  Set2 = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
           "#E5C494", "#B3B3B3")
)

# The colors a layout draws its categories in: a palette named, or the colors
# themselves.
fy_layout_colors <- function(colors) {
  if (is.null(colors)) {
    return(foresty_colors("Dark2"))
  }
  if (length(colors) == 1L && colors %in% names(fy_brewer_palettes)) {
    return(fy_brewer_palettes[[colors]])
  }
  colors
}

# Whether this figure is drawn in more than one color.
fy_colors_rows <- function(layout) {
  !identical(layout$color_by %||% "none", "none")
}

# Which category each row is drawn in the color of.
#
# "category" is the row's own: the level of a categorical exposure -- "Yes" and
# "No" -- where it has one, and the subgroup it sits in otherwise, which is
# what the rows of an interaction figure are. The same category is drawn in the
# same color wherever it appears on the figure, so that a combined figure
# reads across its blocks.
fy_color_keys <- function(estimates, color_by) {
  if (identical(color_by, "row")) {
    return(factor(seq_len(nrow(estimates))))
  }
  # The level of a categorical exposure first, then the subgroup the row is the
  # estimate within, then the label the row is drawn against, then the block it
  # sits in, then the variable it is of. Each of them is what the row is
  # labelled with where it has one, so the colors change with the labels a
  # reader is already reading, and a category drawn twice -- in two blocks of a
  # combined figure -- is drawn in one color both times.
  column <- function(name) {
    out <- estimates[[name]]
    if (is.null(out)) {
      return(rep(NA_character_, nrow(estimates)))
    }
    out <- as.character(out)
    out[!is.na(out) & !nzchar(out)] <- NA_character_
    out
  }
  key <- column("level")
  for (name in c("modifier_label", "row_label", "label", "group", "variable")) {
    missing <- is.na(key)
    if (!any(missing)) {
      break
    }
    key[missing] <- column(name)[missing]
  }
  key[is.na(key)] <- ""
  factor(key, levels = unique(key))
}

# The color of each row, and the fill of its mark. The reference level of a
# categorical exposure is drawn hollow whatever the figure is colored by: it
# is a definition rather than an estimate, and a filled mark would say it had
# been estimated.
fy_row_colors <- function(estimates, layout) {
  if (!fy_colors_rows(layout)) {
    estimates$row_color <- layout$palette$estimate
    estimates$fill_color <- ifelse(fy_reference_flags(estimates),
                                    layout$palette$reference,
                                    layout$palette$estimate)
    return(estimates)
  }
  keys <- fy_color_keys(estimates, layout$color_by)
  colors <- fy_layout_colors(layout$colors)
  estimates$row_color <- colors[(as.integer(keys) - 1L) %% length(colors) + 1L]
  estimates$fill_color <- ifelse(fy_reference_flags(estimates),
                                  layout$palette$reference,
                                  estimates$row_color)
  estimates
}

fy_reference_flags <- function(estimates) {
  out <- estimates$reference
  if (is.null(out)) {
    return(rep(FALSE, nrow(estimates)))
  }
  !is.na(out) & as.logical(out)
}
