# The forest plot, shared by foresty_main() and foresty_interaction().
#
# The figure is a row of ggplots joined by patchwork: the labels of the rows,
# the estimates and their intervals, and the numbers they were drawn from,
# in whichever order the layout asks for.
#
# Three things hold it together.
#
# Every panel is drawn on the same continuous y scale, running from the bottom
# of the last row to the top of the first, with the expansion turned off. A row
# therefore sits at the same height in every panel by construction, and
# anything that belongs to the figure as a whole rather than to one panel of it
# -- the rules, the shaded rows, the lines between subgroups -- is drawn in
# every panel at the same place and reads across the whole width.
#
# The rows are the whole of the panel, so they take the height the device gives
# them and the figure fills it. Everything above them -- the column headings
# and the rules around them -- is drawn in the margin over the panel, at a
# height in points rather than in rows, since a heading is a size the device
# does not change. A band measured in rows would be half of a figure carrying
# two of them and too shallow for one carrying forty.
#
# Nothing the layout depends on is set in a theme. The figure is handed back as
# a ggplot for the caller to retune, and `& theme_minimal()` replaces the theme
# of every panel; a row label written with geom_text survives that, an axis
# label blanked with element_blank() does not. Text is therefore drawn, and the
# axes the text replaces are blanked through their scales.
#
# The columns of text are as wide as the widest thing in them, in centimetres,
# and the plot takes what they leave. That is what keeps a figure compact
# whatever it happens to carry.

fy_forest_plot <- function(estimates,
                           exponentiate,
                           measure_label,
                           estimate_header,
                           table = FALSE,
                           columns = NULL,
                           group = NULL,
                           title = NULL,
                           subtitle = NULL,
                           layout = NULL,
                           label_header = NULL,
                           fold_singletons = FALSE,
                           person_time = NULL) {
  layout <- fy_as_layout(layout)
  null_value <- if (exponentiate) 1 else 0
  estimates <- fy_row_order(estimates, group)
  estimates$emphasis <- fy_emphasis_flags(estimates)

  # A group holding a single row does not need a heading of its own: the row is
  # labelled with the group instead. That is what turns the usual interaction
  # figure, one estimate per level of the modifier, into rows reading "Female"
  # and "Male" rather than the exposure's name repeated under a heading.
  counts <- table(as.character(estimates$group))
  folded <- !is.null(group) && all(counts == 1L)
  grouped <- !is.null(group) && !folded
  as_rows <- grouped && layout$group_position == "row"

  # The same fold, applied one group at a time. A figure combining an overall
  # estimate with several subgroup analyses has one group holding a single row
  # and the rest holding several, and the single one reads as a row of its own
  # rather than as a heading with one row under it.
  fold <- if (as_rows && isTRUE(fold_singletons)) {
    names(counts)[counts == 1L]
  } else {
    character(0)
  }

  # A block drawn for emphasis is read apart from the blocks around it, so it
  # is set off from them by a rule of its own.
  gaps <- if (as_rows) fy_emphasis_gaps(estimates, layout) else character(0)

  rows <- fy_layout_rows(estimates, grouped = grouped, as_rows = as_rows,
                         separators = layout$separators, fold = fold,
                         gaps = gaps)
  estimates <- fy_row_colors(rows$estimates, layout)
  row_labels <- if (folded) {
    as.character(estimates$group)
  } else {
    fy_row_labels_for(estimates)
  }
  in_fold <- as.character(estimates$group) %in% fold
  row_labels[in_fold] <- as.character(estimates$group)[in_fold]

  cells <- if (table) {
    fy_table_cells(estimates, estimate_header, columns, layout,
                   person_time = person_time)
  } else {
    NULL
  }

  # The labels leave the plot's own axis as soon as there is a panel beside it
  # to hold them, where they can be aligned and indented as a column of a table
  # is, and as soon as there is a heading to write over them. A figure that is
  # nothing but the plot keeps them on the axis.
  labelled_apart <- table || grouped || !is.null(label_header)
  headers <- c(
    if (!is.null(cells)) cells$headers$label,
    if (labelled_apart && !is.null(label_header)) label_header
  )
  geo <- fy_geometry(rows$n, headers, layout, rows$separators)

  panels <- list()
  if (grouped && !as_rows) {
    panels <- c(panels, list(fy_group_panel(rows$headings, geo, layout)))
  }
  if (labelled_apart) {
    # Rows sitting under a heading are indented beneath it; a folded group is
    # its own heading, so it is not.
    indent <- rep(if (as_rows) 1.2 else 0, length(row_labels))
    indent[in_fold] <- 0
    panels <- c(panels, list(fy_label_panel(
      labels = row_labels, positions = estimates$position, indent = indent,
      headings = if (as_rows) rows$headings else NULL,
      header = label_header, geo = geo, layout = layout,
      faces = ifelse(estimates$emphasis, layout$emphasis_face, "plain")
    )))
  }

  forest <- fy_forest_panel(
    estimates, exponentiate, measure_label, null_value, geo, layout,
    axis_labels = if (labelled_apart) NULL else row_labels,
    title = if (labelled_apart) NULL else title,
    subtitle = if (labelled_apart) NULL else subtitle
  )
  table_panel <- if (!is.null(cells)) fy_table_panel(cells, geo, layout)

  if (identical(layout$table_side, "left") && !is.null(table_panel)) {
    panels <- c(panels, list(table_panel), list(forest))
    forest_at <- length(panels)
  } else {
    panels <- c(panels, list(forest))
    forest_at <- length(panels)
    if (!is.null(table_panel)) {
      panels <- c(panels, list(table_panel))
    }
  }

  if (length(panels) == 1L) {
    return(fy_as_foresty_plot(forest))
  }
  fy_as_foresty_plot(fy_compose(panels, forest_at, layout, title, subtitle))
}

# Joins the panels, leaving the forest the one that `+` reaches.
#
# patchwork adds a ggplot element to the last plot it was given, so a figure
# assembled left to right would hand `+ coord_cartesian()` to the table of
# numbers and leave the estimates untouched. The forest is therefore passed
# last and put back in its place by the layout, which is what makes
# `foresty_main(...) + coord_cartesian(xlim = c(0.8, 2))` do what it reads as.
# `&` still reaches every panel, and rescaling the table with it would be a
# mistake, so `+` is the one to retune a figure with.
fy_compose <- function(panels, forest_at, layout, title = NULL,
                       subtitle = NULL) {
  order <- c(setdiff(seq_along(panels), forest_at), forest_at)
  design <- paste(LETTERS[match(seq_along(panels), order)], collapse = "")
  widths <- fy_panel_widths(panels, forest_at, layout)
  out <- patchwork::wrap_plots(
    lapply(panels[order], function(p) {
      attr(p, "fy_width") <- NULL
      p
    }),
    design = design,
    widths = widths
  )

  # How little of the figure the plot may be left with, carried on the figure
  # because it can only be acted on when the figure is drawn and the width it
  # is being drawn at is known. See fy_floor_plot_width().
  attr(out, "fy_min_plot_width") <- layout$min_plot_width

  # The title belongs to the figure rather than to the plot in the middle of
  # it, which is where it would sit if it were the forest's own.
  if (is.null(title) && is.null(subtitle)) {
    return(out)
  }
  annotated <- out + patchwork::plot_annotation(
    title = title, subtitle = subtitle,
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = layout$base_size * 1.15, face = "bold",
        color = layout$palette$text, family = layout$family,
        margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        size = layout$base_size * 0.95, color = layout$palette$text,
        family = layout$family, margin = ggplot2::margin(b = 6)
      )
    )
  )
  # `+` rebuilds the object and drops what it was carrying, so the floor is put
  # back on the figure that is returned rather than on the one it was made from.
  attr(annotated, "fy_min_plot_width") <- layout$min_plot_width
  annotated
}

# How the width of the figure is divided.
#
# The widths belong to the columns of the design, which are in the order the
# panels are drawn in, not the order they are handed to patchwork in.
#
# A column of text is given the centimetres its longest entry needs, and the
# plot is given what they leave, so that widening the figure widens the plot
# and nothing else. `plot_width` as a fraction divides the figure by that
# proportion instead: the plot keeps its share of a page of any width, and the
# columns of text share the rest between them in proportion to what they hold.
fy_panel_widths <- function(panels, forest_at, layout) {
  cm <- vapply(seq_along(panels), function(i) {
    if (i == forest_at) NA_real_ else as.numeric(attr(panels[[i]], "fy_width"))
  }, numeric(1))
  share <- layout$plot_width

  if (is.numeric(share) && length(share) == 1L && share > 0 && share < 1) {
    cm[forest_at] <- sum(cm, na.rm = TRUE) * share / (1 - share)
    return(grid::unit(cm, "null"))
  }
  do.call(grid::unit.c, lapply(seq_along(panels), function(i) {
    attr(panels[[i]], "fy_width")
  }))
}

# Emphasis --------------------------------------------------------------------

# Which rows are drawn for emphasis. A figure that names none says so by
# carrying no column, which is every figure but a combined one.
fy_emphasis_flags <- function(estimates) {
  out <- estimates$emphasis
  if (is.null(out)) {
    return(rep(FALSE, nrow(estimates)))
  }
  !is.na(out) & as.logical(out)
}

# Where a rule is drawn between one block and the next whatever the style says:
# on either side of a block drawn for emphasis, which is what separates the
# overall estimate from the subgroups under it. A figure whose blocks are all
# emphasised has nothing to be separated from.
fy_emphasis_gaps <- function(estimates, layout) {
  if (!isTRUE(layout$emphasis_gap)) {
    return(character(0))
  }
  groups <- as.character(estimates$group)
  order <- unique(groups)
  emphasised <- vapply(order, function(g) all(estimates$emphasis[groups == g]),
                       logical(1))
  if (!any(emphasised) || all(emphasised) || length(order) < 2L) {
    return(character(0))
  }
  after <- seq_along(order)[-1L]
  order[after][emphasised[after] | emphasised[after - 1L]]
}

# Rows and geometry -----------------------------------------------------------

# Rows are drawn top to bottom in the order they were given, grouped together.
# `position` is the height a row sits at, counted from the bottom, which is the
# direction a y axis runs in.
fy_row_order <- function(estimates, group) {
  if (!is.null(group)) {
    # The blocks come out in the order the grouping column puts them in, which
    # for the factor every caller builds is the order of its levels: those of
    # the modifier, the blocks of a combined figure, those of the outcome.
    # Anything else is blocked in the order it first appears, since a
    # figure of two models is drawn in the order the models were given and
    # sorting the blocks alphabetically would reorder them behind the caller.
    keys <- estimates[[group]]
    keys <- if (is.factor(keys)) {
      as.integer(keys)
    } else {
      match(as.character(keys), unique(as.character(keys)))
    }
    estimates <- estimates[order(keys), , drop = FALSE]
    estimates$group <- estimates[[group]]
  } else {
    estimates$group <- factor("")
  }
  n <- nrow(estimates)
  estimates$row <- factor(seq_len(n), levels = rev(seq_len(n)))
  estimates$position <- n - seq_len(n) + 1
  rownames(estimates) <- NULL
  estimates
}

# Where every row of the finished figure sits, the headings of the subgroups
# included when they take a row of their own, and where one subgroup is ruled
# off from the next.
#
# `gaps` names the groups that are ruled off from what is above them whether or
# not the style rules between subgroups, which is how a block drawn for
# emphasis is set apart from the rest.
#
# Every rule sits half a row above the line under it, wherever it comes from,
# so that a row reads the same distance from the rule above it as from the one
# below it and the block drawn for emphasis is spaced as the rest are.
fy_layout_rows <- function(estimates, grouped, as_rows, separators,
                           fold = character(0), gaps = character(0)) {
  groups <- as.character(estimates$group)
  n <- nrow(estimates)
  starts <- c(TRUE, groups[-1L] != groups[-n])
  ruled <- starts & (groups %in% gaps)
  # The first block has nothing above it to be set apart from.
  ruled[1L] <- FALSE
  block_top <- NULL

  if (as_rows) {
    # A folded group takes no heading row of its own, so the slots below it are
    # not pushed down by one.
    heads <- starts & !(groups %in% fold)
    slots <- seq_len(n) + cumsum(heads)
    total <- n + sum(heads)
    headings <- data.frame(
      position = total - (slots[heads] - 1L) + 1,
      label = groups[heads],
      stringsAsFactors = FALSE
    )
    # The top of each block, which is its heading where it has one and its
    # first row where it does not; that is where one group is ruled off the
    # next.
    block_top <- total - (slots[starts] - as.integer(heads[starts])) + 1
  } else {
    slots <- seq_len(n)
    total <- n
    headings <- if (grouped) {
      do.call(rbind, lapply(unique(groups), function(g) {
        data.frame(position = mean(n - which(groups == g) + 1), label = g,
                   stringsAsFactors = FALSE)
      }))
    }
  }
  estimates$position <- total - slots + 1

  breaks <- numeric(0)
  if (grouped) {
    after <- which(starts)[-1L]
    keep <- isTRUE(separators) | ruled[after]
    breaks <- as.numeric(if (as_rows) {
      # Immediately above the first line of the block, which is its heading
      # where it has one.
      (block_top[-1L] + 0.5)[keep]
    } else {
      (estimates$position[after - 1L] - 0.5)[keep]
    })
  }

  list(estimates = estimates, headings = headings, separators = breaks,
       n = total)
}

# The heights the figure is drawn between. The rows are the panel; the band
# above it holding the column headings is measured in points, and is carried
# here as the margin the panel is drawn with rather than as more scale.
fy_geometry <- function(n, headers, layout, separators = numeric(0)) {
  header_lines <- if (length(headers)) fy_lines(headers) else 0L
  size <- layout$base_size

  arrows <- !is.null(layout$arrows)
  at_top <- arrows && identical(layout$arrows_position, "top")
  # The labels of the directions are the one thing still measured in rows: they
  # belong under the last of them, above the axis, and are drawn at the end of
  # the panel they are read against rather than beside it.
  arrow_pad <- if (arrows) 0.9 else 0

  top <- n + 0.5
  bottom <- 0.5

  # The headings sit on the rule under them rather than floating in the middle
  # of the band, the way the head of a column in a table does, so they are
  # drawn upwards from just above the top of the panel and the band is only as
  # deep as they are.
  head_gap <- size * 0.35
  header_pt <- if (header_lines > 0L) {
    head_gap + size * 0.95 * header_lines
  } else {
    0
  }
  rule_top_pt <- header_pt + size * 0.2

  geo <- list(
    n = n,
    top = top,
    bottom = bottom,
    separators = separators,
    y_min = bottom - if (arrows && !at_top) arrow_pad else 0,
    y_max = top + if (at_top) arrow_pad else 0,
    header_lines = header_lines,
    head_gap_pt = head_gap,
    header_pt = header_pt,
    rule_top_pt = rule_top_pt,
    arrow_y = if (at_top) top + 0.62 else bottom - 0.3,
    arrow_label_y = if (at_top) top + 0.24 else bottom - 0.68
  )
  # What the panel is drawn with above it: the band, and the rule over it when
  # the style asks for one.
  geo$pad_top_pt <- if (identical(layout$rules, "full") && header_lines > 0L) {
    rule_top_pt + 2
  } else {
    header_pt + 2
  }
  geo
}

# The band the headings are drawn in is outside the panel, so every panel is
# drawn with its clipping off. The coord is left the default one it replaces,
# so that `+ coord_cartesian(xlim = ...)` is not answered with a note that the
# figure already had a coordinate system; a coord passed that way does turn the
# clipping back on, which is why `xlim` is better set in the layout.
fy_unclipped_coord <- function() {
  ggplot2::coord_cartesian(clip = "off", default = TRUE)
}

# A grob drawn in the band above the panel, positioned in points from the top
# of it.
fy_above_panel <- function(grob) {
  ggplot2::annotation_custom(grob, xmin = -Inf, xmax = Inf,
                             ymin = Inf, ymax = Inf)
}

# The column headings, drawn upwards from just above the top of the panel. `at`
# is where each one sits across the panel, as a fraction of its width.
fy_header_layer <- function(labels, at, hjust, geo, layout) {
  if (!length(labels)) {
    return(NULL)
  }
  fy_above_panel(grid::textGrob(
    label = labels,
    x = grid::unit(at, "npc"),
    y = grid::unit(geo$head_gap_pt, "pt"),
    hjust = hjust, vjust = 0,
    gp = grid::gpar(
      fontsize = layout$base_size,
      fontface = layout$header_face,
      col = layout$palette$header,
      fontfamily = layout$family %||% "",
      lineheight = 0.95
    )
  ))
}

# Everything that belongs to the figure rather than to one panel of it: the
# shaded rows, the rules between subgroups, and the rules around the block.
# Drawn in every panel, at the same heights, so that they read across.
fy_furniture <- function(geo, layout) {
  out <- list()
  separators <- geo$separators %||% numeric(0)

  if (isTRUE(layout$band) && geo$n > 0) {
    shaded <- seq(geo$n, 1, by = -2)
    out <- c(out, list(ggplot2::annotate(
      "rect", xmin = -Inf, xmax = Inf,
      ymin = shaded - 0.5, ymax = shaded + 0.5,
      fill = layout$palette$band, color = NA
    )))
  }
  if (length(separators)) {
    out <- c(out, list(ggplot2::annotate(
      "segment", x = -Inf, xend = Inf, y = separators, yend = separators,
      color = layout$palette$rule, linewidth = 0.25
    )))
  }

  # The rule under the headings is the top edge of the panel, and is drawn
  # there rather than in the band above it so that it survives a coord passed
  # by hand, which takes the band with it.
  ruled <- !identical(layout$rules, "none") && geo$header_lines > 0L
  if (ruled) {
    out <- c(out, list(ggplot2::annotate(
      "segment", x = -Inf, xend = Inf, y = Inf, yend = Inf,
      color = layout$palette$rule, linewidth = 0.45
    )))
  }
  # The rule over the headings has no height in the panel to be drawn at, so it
  # is measured in points from the top of it, as the headings themselves are.
  if (ruled && identical(layout$rules, "full")) {
    out <- c(out, list(fy_above_panel(grid::segmentsGrob(
      x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
      y0 = grid::unit(geo$rule_top_pt, "pt"),
      y1 = grid::unit(geo$rule_top_pt, "pt"),
      gp = grid::gpar(col = layout$palette$rule, lwd = 0.45 * ggplot2::.pt)
    ))))
  }
  if (identical(layout$rules, "full")) {
    out <- c(out, list(ggplot2::annotate(
      "segment", x = -Inf, xend = Inf, y = geo$bottom, yend = geo$bottom,
      color = layout$palette$rule, linewidth = 0.45
    )))
  }
  out
}

# The scales every panel shares. The expansion is off, so that the heights are
# the ones fy_geometry() worked out and a rule drawn at one of them sits where
# it was put.
fy_shared_y <- function(geo, labels = NULL, positions = NULL) {
  if (is.null(labels)) {
    ggplot2::scale_y_continuous(
      limits = c(geo$y_min, geo$y_max), breaks = NULL,
      expand = ggplot2::expansion(0)
    )
  } else {
    ggplot2::scale_y_continuous(
      limits = c(geo$y_min, geo$y_max), breaks = positions, labels = labels,
      expand = ggplot2::expansion(0)
    )
  }
}

# The panels of text carry no axes, no grid and no background of their own:
# they are columns of a table set beside a figure, not figures. The margin
# above the panel is the band the headings are drawn in.
fy_text_theme <- function(layout, geo) {
  ggplot2::theme_void(base_size = layout$base_size,
                      base_family = layout$family %||% "") +
    ggplot2::theme(
      plot.margin = ggplot2::margin(2 + geo$pad_top_pt, 0, 2, 0),
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# Text panels -----------------------------------------------------------------

# A column of row labels, with the subgroups on rows of their own above the
# rows they cover and those rows indented under them.
fy_label_panel <- function(labels, positions, headings, header, geo, layout,
                           indent = NULL, faces = NULL) {
  if (is.null(indent)) {
    indent <- rep(if (!is.null(headings)) 1.2 else 0, length(labels))
  }
  if (is.null(faces)) {
    faces <- rep("plain", length(labels))
  }
  size <- layout$base_size

  cells <- data.frame(x = indent, y = positions, label = labels, face = faces,
                      stringsAsFactors = FALSE)
  # Measured row by row, since a folded group sits at the left margin while the
  # rows under a heading are indented, and a row drawn for emphasis is set in
  # bold and takes a little more room than the rest.
  label_em <- as.numeric(mapply(fy_em, labels, faces, USE.NAMES = FALSE))
  width <- max(c(indent + label_em,
                 fy_em(header, layout$header_face),
                 if (!is.null(headings)) fy_em(headings$label,
                                               layout$group_face)))

  out <- ggplot2::ggplot() +
    fy_furniture(geo, layout) +
    ggplot2::geom_text(
      data = cells,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label,
                   fontface = .data$face),
      hjust = 0, vjust = 0.5, size = fy_text_size(size),
      color = layout$palette$text, family = layout$family %||% ""
    )

  if (!is.null(headings) && nrow(headings)) {
    out <- out + ggplot2::geom_text(
      data = headings,
      ggplot2::aes(x = 0, y = .data$position, label = .data$label),
      hjust = 0, vjust = 0.5, size = fy_text_size(size),
      fontface = layout$group_face, color = layout$palette$group,
      family = layout$family %||% ""
    )
  }
  if (!is.null(header)) {
    out <- out + fy_header_layer(header, at = 0, hjust = 0, geo = geo,
                                 layout = layout)
  }

  fy_finish_text_panel(out, width + 0.6, geo, layout)
}

# The name of the subgroup in a column of its own, for the layout that puts it
# there rather than on a row above its rows.
fy_group_panel <- function(headings, geo, layout) {
  out <- ggplot2::ggplot() +
    fy_furniture(geo, layout) +
    ggplot2::geom_text(
      data = headings,
      ggplot2::aes(x = 0, y = .data$position, label = .data$label),
      hjust = 0, vjust = 0.5, size = fy_text_size(layout$base_size),
      fontface = layout$group_face, color = layout$palette$group,
      family = layout$family %||% ""
    )
  fy_finish_text_panel(out, fy_em(headings$label, layout$group_face) + 0.6,
                       geo, layout)
}

# The numbers beside the plot, each column as wide as the widest thing in it.
fy_table_panel <- function(cells, geo, layout) {
  out <- ggplot2::ggplot() +
    fy_furniture(geo, layout) +
    ggplot2::geom_text(
      data = cells$values,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      hjust = 0.5, vjust = 0.5, size = fy_text_size(layout$base_size),
      color = layout$palette$text, family = layout$family %||% ""
    ) +
    fy_header_layer(cells$headers$label, at = cells$headers$x / cells$width,
                    hjust = 0.5, geo = geo, layout = layout)
  fy_finish_text_panel(out, cells$width, geo, layout)
}

fy_finish_text_panel <- function(plot, width_em, geo, layout) {
  out <- plot +
    ggplot2::scale_x_continuous(limits = c(0, width_em), breaks = NULL,
                                expand = ggplot2::expansion(0)) +
    fy_shared_y(geo) +
    fy_unclipped_coord() +
    ggplot2::labs(x = NULL, y = NULL) +
    fy_text_theme(layout, geo)
  attr(out, "fy_width") <- fy_text_width(width_em, layout$base_size)
  out
}

# The plot itself -------------------------------------------------------------

fy_forest_panel <- function(estimates, exponentiate, measure_label, null_value,
                            geo, layout, axis_labels = NULL, title = NULL,
                            subtitle = NULL) {
  limits <- layout$xlim
  drawn <- fy_clip_estimates(estimates, limits)

  out <- ggplot2::ggplot() + fy_furniture(geo, layout)

  if (!identical(layout$null_line, "none")) {
    out <- out + ggplot2::annotate(
      "segment", x = null_value, xend = null_value,
      y = geo$bottom, yend = geo$top,
      linetype = layout$null_line, linewidth = 0.4,
      color = layout$palette$null
    )
  }

  # A row drawn as a summary diamond has its interval drawn as the diamond
  # itself, so it is kept out of the layers that draw a mark on a line.
  diamonds <- fy_diamond_rows(drawn, layout)

  out <- out +
    fy_interval_layers(drawn[!diamonds, , drop = FALSE], layout) +
    fy_point_layers(drawn[!diamonds, , drop = FALSE], layout) +
    fy_diamond_layer(drawn[diamonds, , drop = FALSE], layout) +
    fy_color_scales(layout) +
    fy_arrow_labels(geo, layout, null_value) +
    ggplot2::labs(title = title, subtitle = subtitle,
                  x = fy_wrap(measure_label, 34), y = NULL) +
    fy_shared_y(geo, labels = axis_labels,
                positions = if (!is.null(axis_labels)) estimates$position) +
    fy_x_scale(null_value, limits, layout) +
    fy_unclipped_coord() +
    fy_forest_theme(layout, has_axis_labels = !is.null(axis_labels), geo = geo)

  # The line of no effect is what every interval is read against, so it is kept
  # on the panel even when no interval comes near it.
  if (is.null(limits)) {
    out <- out + ggplot2::expand_limits(x = null_value)
  }
  attr(out, "fy_width") <- fy_plot_width(layout)
  out
}

# How the marks are filled.
#
# A figure drawn in one color maps the fill to whether the row is the
# reference level, so that a scale added by hand still reaches it. A figure
# whose rows carry colors of their own has those colors on the rows already,
# and takes them as they are.
fy_color_scales <- function(layout) {
  if (fy_colors_rows(layout)) {
    return(list(ggplot2::scale_fill_identity(),
                ggplot2::scale_color_identity()))
  }
  ggplot2::scale_fill_manual(
    values = c(`FALSE` = layout$palette$estimate,
               `TRUE` = layout$palette$reference),
    guide = "none"
  )
}

fy_plot_width <- function(layout) {
  width <- layout$plot_width
  if (is.null(width)) {
    return(grid::unit(1, "null"))
  }
  if (grid::is.unit(width)) {
    return(width)
  }
  checkmate::assert_number(width, lower = 0)
  # A fraction is a share of the figure, worked out in fy_panel_widths() once
  # the columns of text have been measured; anything else is centimetres.
  if (width < 1) {
    return(grid::unit(1, "null"))
  }
  grid::unit(width, "cm")
}

# An interval running past the limits of the plot is drawn with an arrow at
# that end rather than being left off, so a reader can see that it is wider
# than the figure and by which side it leaves.
fy_clip_estimates <- function(estimates, limits) {
  out <- estimates
  out$low <- out$conf.low
  out$high <- out$conf.high
  out$estimate_drawn <- out$estimate
  out$clip_low <- FALSE
  out$clip_high <- FALSE

  if (is.null(limits)) {
    return(out)
  }
  out$clip_low <- !is.na(out$low) & out$low < limits[1L]
  out$clip_high <- !is.na(out$high) & out$high > limits[2L]
  out$low <- pmax(out$low, limits[1L])
  out$high <- pmin(out$high, limits[2L])
  out$estimate_drawn[!is.na(out$estimate) &
                       (out$estimate < limits[1L] |
                          out$estimate > limits[2L])] <- NA_real_
  out
}

# Which rows are drawn as the summary diamond a paper draws: the ones singled
# out for emphasis, when the layout asks for a diamond rather than a plotting
# symbol and the row has an interval for the diamond to be as wide as. The
# reference level of a categorical exposure has none, having not been
# estimated, so it keeps its hollow mark whatever else the figure does.
fy_diamond_rows <- function(drawn, layout) {
  if (!identical(layout$emphasis_shape, "diamond")) {
    return(rep(FALSE, nrow(drawn)))
  }
  fy_emphasis_flags(drawn) &
    !is.na(drawn$estimate_drawn) & !is.na(drawn$low) & !is.na(drawn$high)
}

# The summary diamond: the confidence limits at its two side vertices and the
# estimate at its apex, drawn instead of the interval rather than on top of it.
#
# It is drawn as a polygon in the coordinates of the panel rather than as a
# plotting symbol, so that its two diagonals are exactly the interval and the
# height asked for, whatever size the figure is drawn at. R's own diamond
# symbol is a little taller than it is wide, which reads as a lopsided mark
# beside the squares of the rows under it.
fy_diamond_layer <- function(part, layout) {
  if (!nrow(part)) {
    return(NULL)
  }
  half <- layout$emphasis_height
  corners <- data.frame(
    id = rep(seq_len(nrow(part)), each = 4L),
    x = as.vector(rbind(part$low, part$estimate_drawn,
                        part$high, part$estimate_drawn)),
    y = as.vector(rbind(part$position, part$position + half,
                        part$position, part$position - half)),
    stringsAsFactors = FALSE
  )
  if (fy_colors_rows(layout)) {
    corners$color <- rep(part$row_color, each = 4L)
    return(ggplot2::geom_polygon(
      data = corners,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id,
                   fill = .data$color, color = .data$color),
      linewidth = 0.35, na.rm = TRUE, show.legend = FALSE
    ))
  }
  ggplot2::geom_polygon(
    data = corners,
    ggplot2::aes(x = .data$x, y = .data$y, group = .data$id),
    fill = layout$palette$estimate, color = layout$palette$border,
    linewidth = 0.35, na.rm = TRUE
  )
}

# The marks. Rows drawn for emphasis are a layer of their own rather than a
# shape mapped over all of them, so that a figure with none of them is drawn
# exactly as it was before and a scale added by hand still reaches the fill.
fy_point_layers <- function(drawn, layout) {
  drawn <- drawn[!is.na(drawn$estimate_drawn), , drop = FALSE]
  emphasis <- fy_emphasis_flags(drawn)
  # A row that asks for the wide summary diamond and has no interval to make one
  # out of falls back to a mark, which is R's filled diamond.
  shape <- if (identical(layout$emphasis_shape, "diamond")) {
    23
  } else {
    layout$emphasis_shape
  }
  list(
    fy_point_layer(drawn[!emphasis, , drop = FALSE], layout$point_shape,
                   layout$point_size, layout),
    fy_point_layer(drawn[emphasis, , drop = FALSE], shape,
                   fy_emphasis_size(layout), layout)
  )
}

# The reference level is drawn hollow, because it is a definition rather than
# an estimate and has no interval of its own.
fy_point_layer <- function(part, shape, size, layout) {
  if (!nrow(part)) {
    return(NULL)
  }
  if (fy_colors_rows(layout)) {
    return(ggplot2::geom_point(
      data = part,
      ggplot2::aes(x = .data$estimate_drawn, y = .data$position,
                   fill = .data$fill_color, color = .data$row_color),
      shape = shape, size = size, stroke = 0.5, show.legend = FALSE,
      na.rm = TRUE
    ))
  }
  ggplot2::geom_point(
    data = part,
    ggplot2::aes(x = .data$estimate_drawn, y = .data$position,
                 fill = .data$reference),
    shape = shape, size = size, stroke = 0.5,
    color = layout$palette$border, show.legend = FALSE, na.rm = TRUE
  )
}

fy_interval_layers <- function(drawn, layout) {
  drawn <- drawn[!is.na(drawn$low) & !is.na(drawn$high), , drop = FALSE]
  if (!nrow(drawn)) {
    return(NULL)
  }
  head <- grid::unit(0.055, "inches")
  kinds <- list(
    list(rows = !drawn$clip_low & !drawn$clip_high, arrow = NULL),
    list(rows = drawn$clip_low & !drawn$clip_high,
         arrow = grid::arrow(length = head, ends = "first", type = "closed")),
    list(rows = !drawn$clip_low & drawn$clip_high,
         arrow = grid::arrow(length = head, ends = "last", type = "closed")),
    list(rows = drawn$clip_low & drawn$clip_high,
         arrow = grid::arrow(length = head, ends = "both", type = "closed"))
  )

  lapply(kinds, function(kind) {
    part <- drawn[kind$rows, , drop = FALSE]
    if (!nrow(part)) {
      return(NULL)
    }
    if (fy_colors_rows(layout)) {
      return(ggplot2::geom_segment(
        data = part,
        ggplot2::aes(x = .data$low, xend = .data$high,
                     y = .data$position, yend = .data$position,
                     color = .data$row_color),
        linewidth = layout$interval_width, lineend = "butt",
        arrow = kind$arrow, na.rm = TRUE, show.legend = FALSE
      ))
    }
    ggplot2::geom_segment(
      data = part,
      ggplot2::aes(x = .data$low, xend = .data$high,
                   y = .data$position, yend = .data$position),
      linewidth = layout$interval_width, lineend = "butt",
      color = layout$palette$interval, arrow = kind$arrow, na.rm = TRUE
    )
  })
}

# "Favours treatment" and "Favours control", with an arrow apiece, drawn inside
# the panel so that they stay with the plot whatever theme is added to it.
fy_arrow_labels <- function(geo, layout, null_value) {
  if (is.null(layout$arrows)) {
    return(NULL)
  }
  size <- fy_text_size(layout$base_size * 0.9)
  head <- grid::arrow(length = grid::unit(0.05, "inches"), type = "open")
  list(
    ggplot2::annotate(
      "segment", x = null_value, xend = -Inf,
      y = geo$arrow_y, yend = geo$arrow_y,
      color = layout$palette$axis, linewidth = 0.3, arrow = head
    ),
    ggplot2::annotate(
      "segment", x = null_value, xend = Inf,
      y = geo$arrow_y, yend = geo$arrow_y,
      color = layout$palette$axis, linewidth = 0.3, arrow = head
    ),
    ggplot2::annotate(
      "text", x = -Inf, y = geo$arrow_label_y, label = layout$arrows[1L],
      hjust = -0.02, vjust = 0.5, size = size, color = layout$palette$text,
      family = layout$family %||% ""
    ),
    ggplot2::annotate(
      "text", x = Inf, y = geo$arrow_label_y, label = layout$arrows[2L],
      hjust = 1.02, vjust = 0.5, size = size, color = layout$palette$text,
      family = layout$family %||% ""
    )
  )
}

fy_forest_theme <- function(layout, has_axis_labels, geo) {
  # theme_void() unless the layout named one of ggplot2's own, in which case
  # the plot is drawn on that -- its background, its border, its grid -- with
  # everything the layout sets applied over it.
  out <- fy_base_theme(layout) + ggplot2::theme(
    # The same band above the panel as the columns of text carry, so that the
    # rows of one panel meet the rows of the next.
    plot.margin = ggplot2::margin(2 + geo$pad_top_pt, 4, 2, 4),
    axis.line.x = if (layout$axis_line) {
      ggplot2::element_line(color = layout$palette$axis, linewidth = 0.35)
    } else {
      ggplot2::element_blank()
    },
    axis.ticks.x = ggplot2::element_line(color = layout$palette$axis,
                                         linewidth = 0.35),
    axis.ticks.length.x = grid::unit(2.5, "pt"),
    axis.text.x = ggplot2::element_text(
      size = layout$base_size * 0.9, color = layout$palette$text,
      margin = ggplot2::margin(t = 2)
    ),
    axis.title.x = ggplot2::element_text(
      size = layout$base_size, color = layout$palette$text,
      margin = ggplot2::margin(t = 4)
    ),
    # The grid is the layout's to say, whatever theme it is drawn over: a
    # theme's own would be a second answer to the same question. The rows are
    # the figure's own rules, so there is never a horizontal one.
    panel.grid.major.x = if (layout$grid) {
      ggplot2::element_line(color = layout$palette$band, linewidth = 0.3)
    } else {
      ggplot2::element_blank()
    },
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(
      size = layout$base_size * 1.15, face = "bold",
      color = layout$palette$text, margin = ggplot2::margin(b = 2)
    ),
    plot.subtitle = ggplot2::element_text(
      size = layout$base_size * 0.95, color = layout$palette$text,
      margin = ggplot2::margin(b = 6)
    )
  )
  if (has_axis_labels) {
    out <- out + ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = layout$base_size, color = layout$palette$text, hjust = 1,
        margin = ggplot2::margin(r = 3)
      )
    )
  }
  out
}

# The heading the model wrote for itself -- "Adjusted odds ratio for asthma
# (95% CI)" -- is the widest thing on the figure, and the column beneath it is
# half its width. It is broken over the interval first and then wrapped, so
# that the heading takes the depth and the plot takes the width. A heading
# given by hand is written as it was given.
fy_wrap_heading <- function(header, width = 16) {
  interval <- regmatches(header, regexpr(" \\([0-9.]+% CI\\)$", header))
  if (length(interval)) {
    header <- substr(header, 1L, nchar(header) - nchar(interval))
  }
  paste(c(fy_wrap(header, width), trimws(interval)), collapse = "\n")
}

# A label too long for the column it sits under is broken across lines rather
# than run off the edge of the figure.
fy_wrap <- function(x, width) {
  if (is.null(x) || !nzchar(x) || nchar(x) <= width) {
    return(x)
  }
  paste(strwrap(x, width = width + 1), collapse = "\n")
}

# A continuous exposure has no level to name, so its row is labelled with the
# variable. When the subgroup heading beside it says the same thing, the row is
# left blank rather than repeating it.
fy_row_labels_for <- function(estimates) {
  labels <- as.character(estimates$label)
  labels[labels == as.character(estimates$group)] <- ""
  labels
}

# The axis is linear, ratios included. A log axis would put a halving and a
# doubling the same distance from the null, but it also spaces the ticks in a
# way that has to be learned before the figure can be read, so it is left to
# the caller to ask for: `+ ggplot2::scale_x_continuous(transform = "log")`.
#
# The null is always among the breaks, since the line drawn there is what the
# intervals are read against and an unlabelled line leaves the reader guessing
# where it sits.
fy_x_scale <- function(null_value, limits, layout) {
  breaks <- function(range) {
    default <- scales::breaks_pretty(n = 4)(range)
    # A break that rounds to the null is the null: `!=` on doubles let
    # breaks_pretty()'s own 1 through beside the one added here, and the two
    # were drawn one on top of the other as a thickened label.
    keep <- abs(default - null_value) > 1e-8 * max(1, abs(null_value))
    out <- sort(c(null_value, default[keep]))
    out[out >= range[1L] & out <= range[2L]]
  }
  ggplot2::scale_x_continuous(
    limits = limits,
    breaks = breaks,
    labels = function(x) fy_axis_labels(x, layout),
    # A plot squeezed narrow by the table beside it has room for fewer labels
    # than the breaks come to, and how many is only known once the figure is
    # drawn, so ggplot2 is left to drop the ones that would overprint rather
    # than a number of breaks being guessed at here. The floor on the width of
    # the plot keeps that from being most of them; see fy_floor_plot_width().
    guide = ggplot2::guide_axis(check.overlap = TRUE),
    expand = if (is.null(limits)) {
      ggplot2::expansion(mult = 0.05)
    } else {
      ggplot2::expansion(mult = 0.02)
    }
  )
}

# The numbers beside the plot -------------------------------------------------

# Lays the chosen columns out: where each one sits across the panel, how wide
# the panel has to be to hold them, and the heading each carries.
fy_table_cells <- function(estimates, estimate_header, columns, layout,
                           person_time = NULL) {
  available <- fy_table_columns(estimates, estimate_header, layout,
                                person_time = person_time)
  chosen <- fy_choose_columns(available, columns)

  gap <- layout$column_gap
  widths <- vapply(seq_along(chosen), function(i) {
    max(fy_em(names(chosen)[i], layout$header_face), fy_em(chosen[[i]])) + gap
  }, numeric(1))
  centres <- cumsum(widths) - widths / 2

  values <- do.call(rbind, lapply(seq_along(chosen), function(i) {
    data.frame(x = centres[i], y = estimates$position, label = chosen[[i]],
               stringsAsFactors = FALSE)
  }))

  list(
    values = values,
    headers = data.frame(x = centres, label = names(chosen),
                         stringsAsFactors = FALSE),
    width = sum(widths)
  )
}

# The estimate and its interval are one column rather than two, written the way
# a result is written in a paper.
fy_table_columns <- function(estimates, estimate_header,
                             layout = fy_style("classic"),
                             person_time = NULL) {
  sep <- fy_ci_separator(estimates, layout)
  number <- function(x) fy_format_number(x, layout$digits, layout$decimal_mark)

  estimate <- ifelse(
    estimates$reference,
    paste0(number(estimates$estimate), " ", layout$ci_brackets[1L],
           "reference", layout$ci_brackets[2L]),
    paste0(
      number(estimates$estimate), " ", layout$ci_brackets[1L],
      number(estimates$conf.low), sep, number(estimates$conf.high),
      layout$ci_brackets[2L]
    )
  )

  out <- list()
  header <- layout$headings$estimate
  if (is.null(header)) {
    header <- fy_wrap_heading(estimate_header)
  }
  out[[header]] <- estimate
  keys <- "estimate"
  # A column no row supplies says nothing, and an empty column in the table
  # takes width from the plot. Every model-driven figure carries a p-value and
  # a count for each row; a figure drawn from a table of numbers by
  # foresty_data() carries whichever the table held.
  if (!all(is.na(estimates$p.value))) {
    # The reference level has no p-value, having not been estimated.
    out[[layout$headings$p]] <- ifelse(
      estimates$reference, "",
      fy_format_p(estimates$p.value, layout$p_format, layout$decimal_mark)
    )
    keys <- c(keys, "p")
  }
  available <- fy_available_counts(estimates)
  pairs <- fy_count_pairs(estimates, layout$counts)
  if (available[["n"]]) {
    out[[layout$headings$n]] <- fy_format_pair(pairs$n)
    keys <- c(keys, "n")
  }

  if (available[["events"]] && !is.null(pairs$events)) {
    out[[layout$headings$events]] <- fy_format_pair(pairs$events)
    keys <- c(keys, "events")
  }
  if (available[["person_time"]]) {
    heading <- fy_person_time_heading(layout$headings$person_time, person_time)
    out[[heading]] <- fy_format_person_time(estimates$person_time, person_time)
    keys <- c(keys, "person_time")
  }
  # One test covers every subgroup it was taken across, so it is written once,
  # beside the first of them, rather than repeated down the column. Where both
  # tests of the interaction were asked for, each column says which one it
  # holds.
  tested <- fy_tested_blocks(estimates)
  both_tests <- "interaction_p_lrt" %in% names(estimates)
  if ("interaction_p" %in% names(estimates)) {
    p <- fy_format_p(estimates$interaction_p, layout$p_format,
                     layout$decimal_mark)
    heading <- if (both_tests) {
      layout$headings$interaction_p_wald
    } else {
      layout$headings$interaction_p
    }
    out[[heading]] <- fy_once_per_group(p, tested)
    keys <- c(keys, "interaction_p")
  }
  if (both_tests) {
    p <- fy_format_p(estimates$interaction_p_lrt, layout$p_format,
                     layout$decimal_mark)
    out[[layout$headings$interaction_p_lrt]] <- fy_once_per_group(p, tested)
    keys <- c(keys, "interaction_p_lrt")
  }
  attr(out, "keys") <- keys
  # Two columns of p-values side by side, one of them a test of the interaction
  # and the other a test of each subgroup effect against the null, are read for
  # each other however they are headed, and the one a reader reaches for is the
  # test of the interaction. So a figure carrying that test leaves the other
  # off unless it is asked for by name.
  attr(out, "default") <- if ("interaction_p" %in% keys) {
    setdiff(keys, "p")
  } else {
    keys
  }
  out
}

# Which of the columns of counts a set of estimates can supply.
#
# A column no row supplies says nothing, and an empty column in the table takes
# width from the plot, so the table leaves it out -- and the note under the
# figure says nothing about a column that is not there.
fy_available_counts <- function(estimates) {
  c(n = !all(is.na(estimates$n)),
    events = !all(is.na(estimates$events)),
    person_time = !all(is.na(estimates$person_time)))
}

# Which of them the figure actually draws, once `table` and `columns` have had
# their say. `columns` has already been through fy_choose_columns() by the time
# this is asked, so a name in it is one of the names it is allowed to be.
fy_drawn_counts <- function(estimates, table = TRUE, columns = NULL) {
  available <- fy_available_counts(estimates)
  if (!isTRUE(table)) {
    return(available & FALSE)
  }
  if (is.null(columns)) {
    return(available)
  }
  available & names(available) %in% columns
}

# Which two numbers the column of sizes and the column of events hold.
#
# `counts = "row"`, and every set of estimates drawn before the option existed,
# holds the row's own group in each and nothing beside it. `"compared"` puts
# the group the row's estimate is compared with next to it, which is what makes
# an odds ratio on a row of 822 suburban children readable: it came out of
# those 822 and the 468 rural ones, and the 468 were on another row.
#
# Where a row compares two levels of a multinomial outcome rather than two
# levels of the exposure, the two groups are the people at those two levels --
# and that is what the events column already holds, one level to a row. So the
# pair goes under the column of sizes, where a reader looks for how many people
# a row is about, and the events column is left off rather than repeating it.
fy_count_pairs <- function(estimates, counts = "compared") {
  plain <- list(n = list(a = estimates$n, b = NULL),
                events = list(a = estimates$events, b = NULL))
  if (!identical(counts, "compared") || is.null(estimates$counts_pair)) {
    return(plain)
  }
  pair <- as.character(estimates$counts_pair)
  along <- unique(pair[!is.na(pair)])
  if (!length(along)) {
    return(plain)
  }

  # A figure whose rows compare along the outcome only: the pair of counts it
  # has is the pair of outcome levels, and it is reported once.
  if (identical(along, "outcome")) {
    return(list(
      n = list(a = estimates$events, b = fy_where(pair, estimates$events_compared)),
      events = NULL
    ))
  }
  # Otherwise the rows compare along the exposure, and each column carries its
  # own pair. A multinomial fit of a categorical exposure is here too: its
  # sizes are the two levels of the exposure and its events the people at the
  # row's outcome level within each of them, which is the same table a binary
  # outcome would have drawn.
  list(
    n = list(a = estimates$n, b = fy_where(pair, estimates$n_compared)),
    events = list(a = estimates$events,
                  b = fy_where(pair, estimates$events_compared))
  )
}

# The compared counts, blanked on the rows that compare nothing -- a reference
# row, or a row of a figure whose other rows pair and this one does not.
fy_where <- function(pair, values) {
  ifelse(is.na(pair), NA, values)
}

# One column of counts: "822 vs 468" where the row has a group to be compared
# with, and "468" where it is that group.
fy_format_pair <- function(column) {
  a <- fy_format_count(column$a)
  if (is.null(column$b) || all(is.na(column$b))) {
    return(a)
  }
  b <- fy_format_count(column$b)
  ifelse(is.na(column$b) | !nzchar(b), a, paste0(a, " vs ", b))
}

# What the counts beside the rows are counts of, as the sentences that say so.
#
# A figure of a continuous exposure and a binary outcome needs none of this: a
# row is a group of people, the number beside it is how many of them there were
# and the events column is how many had the outcome. The figures that do need
# it are the ones whose rows are half of a comparison -- a level of a
# categorical exposure, a level of a multinomial outcome -- and what has to be
# said about them depends on whether the other half is beside them.
#
# Returned as sentences rather than as one string so that foresty_combine() can
# put the notes of the figures it combines together without repeating the one
# they share.
fy_counts_note <- function(estimates, by_level = FALSE, by_outcome = FALSE,
                           table = TRUE, columns = NULL,
                           counts = "compared") {
  drawn <- fy_drawn_counts(estimates, table = table, columns = columns)
  if (!any(drawn[c("n", "events")])) {
    return(character(0))
  }
  pairs <- fy_count_pairs(estimates, counts)
  along <- if (identical(counts, "compared") && !is.null(estimates$counts_pair)) {
    unique(stats::na.omit(as.character(estimates$counts_pair)))
  } else {
    character(0)
  }
  # Whether the events column is drawn as its own pair of counts, which is what
  # decides whether there is anything to say about it.
  events_drawn <- drawn[["events"]] && !is.null(pairs$events)

  said <- if (length(along)) {
    fy_compared_sentences(along, by_outcome, events_drawn)
  } else {
    fy_row_sentences(by_level, by_outcome, drawn[["events"]])
  }
  if (!length(said)) {
    return(character(0))
  }

  # The rows the counts are over is worth saying wherever they are worth
  # explaining at all: it is the commonest thing a reader assumes wrongly about
  # them, and it is what the figure and the model disagree about when a
  # covariate is missing for some of the cohort.
  c(paste0(
    "The counts are over the rows the model was fitted to, so anyone missing ",
    "the outcome, the exposure or a covariate is left out of them."
  ), said)
}

# What to say where each row carries its own group and the group it is compared
# with. The comparison is on the figure, so what is left to say is which two
# groups they are -- and, for a multinomial fit, that the estimate did not come
# out of those two groups alone.
fy_compared_sentences <- function(along, by_outcome, events_drawn) {
  said <- character(0)
  if ("exposure" %in% along) {
    said <- c(said, paste0(
      "Each row carries its own group and the group its estimate is compared ",
      "with, side by side, so the whole comparison is beside the estimate; a ",
      "reference row is that group and carries one number."
    ))
  }
  if ("outcome" %in% along) {
    said <- c(said, paste0(
      "A row compares two levels of the outcome, and the two numbers beside ",
      "it are how many people were at each of them."
    ))
  }
  if (isTRUE(by_outcome)) {
    if ("exposure" %in% along && events_drawn) {
      said <- c(said, paste0(
        "The sizes are the two groups whatever their outcome, and the events ",
        "are the people at the level the row is of within each of them."
      ))
    }
    said <- c(said, paste0(
      "The estimate did not come out of those groups alone: a multinomial ",
      "model fits every level of the outcome at once, so the people at the ",
      "levels a row is not about bear on it too."
    ))
  }
  said
}

# What to say where each row carries its own group and nothing beside it, which
# is `counts = "row"`. The other half of every comparison is then somewhere
# else on the figure, or nowhere, and that is the thing to say.
fy_row_sentences <- function(by_level, by_outcome, events_drawn) {
  said <- character(0)
  if (isTRUE(by_outcome)) {
    said <- c(said, paste0(
      "Every level of the outcome was fitted at once, so the number of people ",
      "beside a row is everyone it was taken over whatever their outcome",
      if (events_drawn) {
        paste0(
          ", and the events column counts those at the level the row is of ",
          "rather than those at the level it is compared with"
        )
      } else {
        ""
      },
      "."
    ))
  }
  if (isTRUE(by_level)) {
    said <- c(said, paste0(
      "A row counts the people at its own level of the exposure; the level it ",
      "is compared with is counted on the reference row, so no row carries ",
      "the total behind its own estimate."
    ))
  }
  said
}

# The rows one test of an interaction was taken across. A figure of one
# subgroup analysis has a single test covering all of its rows, whatever the
# levels of the modifier are; a combined figure has one per block.
fy_tested_blocks <- function(estimates) {
  if (!is.null(estimates$block)) {
    return(as.character(estimates$block))
  }
  rep("", nrow(estimates))
}

# Written against the first row it applies to, blank underneath, the way a
# figure carrying one test per subgroup is set in a paper.
fy_once_per_group <- function(x, group) {
  group <- as.character(group)
  first <- !duplicated(group)
  ifelse(first, x, "")
}

fy_choose_columns <- function(available, columns) {
  keys <- attr(available, "keys")
  if (is.null(columns)) {
    columns <- attr(available, "default") %||% keys
  } else {
    valid <- c("estimate", "p", "n", "events", "person_time", "interaction_p",
               "interaction_p_lrt")
    columns <- match.arg(columns, valid, several.ok = TRUE)
  }
  wanted <- keys %in% columns
  if (!any(wanted)) {
    stop(
      "none of the columns named in `columns` are available for these models; ",
      "the ones that are: ", paste(keys, collapse = ", "),
      call. = FALSE
    )
  }
  out <- available[wanted]
  attr(out, "keys") <- keys[wanted]
  out
}

# A hyphen between two negative numbers cannot be read, so a figure carrying
# one is written with "to" instead unless the layout says otherwise.
fy_ci_separator <- function(estimates, layout) {
  if (!is.null(layout$ci_separator)) {
    return(layout$ci_separator)
  }
  values <- c(estimates$estimate, estimates$conf.low, estimates$conf.high)
  if (any(!is.na(values) & values < 0)) " to " else "-"
}

# The numbers under the axis, written to as few decimals as tell the breaks
# apart rather than to the figure's own.
#
# The breaks are round numbers, chosen by scales::breaks_pretty(): 0.4 to 1.2 by
# 0.2 needs one decimal, and writing them to the two the estimates are written
# to -- "0.40", "0.60" -- makes every label a third wider for nothing. Width is
# what the axis is short of on a figure whose table has taken most of it, and
# labels a third narrower are labels that go on being read where the wider ones
# would have had to be dropped.
fy_axis_labels <- function(x, layout) {
  finite <- x[is.finite(x)]
  digits <- 0L
  while (digits < layout$digits &&
         anyDuplicated(round(finite, digits))) {
    digits <- digits + 1L
  }
  fy_format_number(x, digits, layout$decimal_mark)
}

# Formatting ------------------------------------------------------------------

# Person-time is rounded, since a fractional person-year is noise on a figure.
fy_format_number <- function(x, digits = 2, decimal_mark = ".") {
  out <- ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
  if (!identical(decimal_mark, ".")) {
    out <- gsub(".", decimal_mark, out, fixed = TRUE)
  }
  out
}

fy_format_count <- function(x) {
  ifelse(is.na(x), "", formatC(round(as.numeric(x)), format = "d", big.mark = ","))
}

# Person-time, in whatever unit it is being reported in.
#
# The total is a count and is written as one, a fractional person-year being
# noise on a figure. A total divided by a unit is not a count -- 4,318 person-
# years is 4.3 thousands, not 4 -- so it keeps the one decimal that makes the
# division worth doing.
fy_format_person_time <- function(x, spec = NULL) {
  if (is.null(spec) || isTRUE(all.equal(spec$unit, 1))) {
    return(fy_format_count(x))
  }
  scaled <- as.numeric(x) / spec$unit
  ifelse(is.na(scaled), "",
         formatC(scaled, format = "f", digits = 1, big.mark = ","))
}

# "default" writes 0.032 and <0.001. "jama" drops the leading zero and rounds
# the way the journal asks: three places below 0.01 and two from 0.01 up, so
# that .01 itself is written .01, and neither an exact 0 nor an exact 1 written
# as though it had been measured.
fy_format_p <- function(x, style = "default", decimal_mark = ".") {
  x <- as.numeric(x)
  out <- if (identical(style, "jama")) {
    rounded <- ifelse(x < 0.01, formatC(x, digits = 3, format = "f"),
                      formatC(x, digits = 2, format = "f"))
    ifelse(
      is.na(x), "",
      ifelse(x < 0.001, "<.001",
             ifelse(x > 0.99, ">.99", sub("^0", "", rounded)))
    )
  } else {
    ifelse(is.na(x), "",
           ifelse(x < 0.001, "<0.001", formatC(x, digits = 3, format = "f")))
  }
  if (!identical(decimal_mark, ".")) {
    out <- gsub(".", decimal_mark, out, fixed = TRUE)
  }
  out
}
