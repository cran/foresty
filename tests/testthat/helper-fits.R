# Shared fixtures. Everything is fitted from foresty_cohort so that the tests
# run without any suggested package installed.

fy_test_data <- function() {
  foresty_cohort
}

fy_test_logistic <- function() {
  glm(asthma ~ no2 + sex + maternal_smoking + maternal_age,
      family = binomial, data = foresty_cohort)
}

fy_test_linear <- function() {
  lm(no2 ~ black_carbon + sex + maternal_age, data = foresty_cohort)
}

# The estimates behind a figure, and the rest of what it carries.
fy_est <- function(x) {
  as.data.frame(x)
}

fy_test <- function(x) {
  fy_result(x)$interaction_test
}

fy_temp_html <- function() {
  tempfile(fileext = ".html")
}

# The title and subtitle of a figure, wherever they were put: a figure of one
# panel carries its own, and one of several carries the annotation patchwork
# draws over the lot.
fy_title <- function(x) {
  if (inherits(x, "patchwork")) {
    return(x$patches$annotation$title)
  }
  ggplot2::ggplot_build(x)$plot$labels$title
}

fy_subtitle <- function(x) {
  if (inherits(x, "patchwork")) {
    return(x$patches$annotation$subtitle)
  }
  ggplot2::ggplot_build(x)$plot$labels$subtitle
}

# The forest itself, which is the last panel of a figure that has several.
fy_forest_of <- function(x) {
  if (inherits(x, "patchwork")) x[[length(x)]] else x
}

# The label under the plot, on one line. It is wrapped when it is drawn, so a
# test reading it back puts it together again rather than matching around the
# line breaks.
fy_axis_text <- function(x) {
  out <- ggplot2::ggplot_build(fy_forest_of(x))$plot$labels$x
  if (is.null(out)) {
    return(NULL)
  }
  gsub("\n", " ", out, fixed = TRUE)
}

# Every piece of text drawn in one panel, which is how the column of row labels
# beside the plot is read back. The rows are drawn with geom_text and the
# heading over them as a grob in the margin, so both are collected.
fy_panel_text <- function(p) {
  out <- character(0)
  for (layer in p$layers) {
    d <- layer$data
    if (is.data.frame(d) && "label" %in% names(d)) {
      out <- c(out, as.character(d$label))
    }
    grob <- layer$geom_params$grob
    if (!is.null(grob) && !is.null(grob$label)) {
      out <- c(out, as.character(grob$label))
    }
  }
  out
}

# The rows of the column of labels, with the face each is set in, which is how
# a row drawn for emphasis is told from the rest.
fy_label_cells <- function(x) {
  panel <- if (inherits(x, "patchwork")) x[[1]] else x
  for (layer in panel$layers) {
    d <- layer$data
    if (is.data.frame(d) && "face" %in% names(d)) {
      return(d)
    }
  }
  NULL
}

# The rows of a combined figure as the plot lays them out: how many there are
# once the headings are counted, and where it is ruled.
fy_rows_of <- function(x, layout = "classic") {
  layout <- fy_as_layout(layout)
  estimates <- fy_row_order(fy_result(x)$estimates, "block_label")
  estimates$emphasis <- fy_emphasis_flags(estimates)
  counts <- table(as.character(estimates$group))
  fy_layout_rows(
    estimates, grouped = TRUE, as_rows = TRUE,
    separators = layout$separators, fold = names(counts)[counts == 1L],
    gaps = fy_emphasis_gaps(estimates, layout)
  )
}

# The plotting symbol of every layer of marks on the forest itself.
fy_point_shapes <- function(x) {
  forest <- fy_forest_of(x)
  out <- vapply(forest$layers, function(layer) {
    if (!inherits(layer$geom, "GeomPoint")) {
      return(NA_real_)
    }
    as.numeric(layer$aes_params$shape %||% NA_real_)
  }, numeric(1))
  out[!is.na(out)]
}

# The four corners of the summary diamond, or NULL where the figure draws
# none: it is the one polygon on the forest.
fy_diamond_corners <- function(x) {
  forest <- fy_forest_of(x)
  for (layer in forest$layers) {
    if (inherits(layer$geom, "GeomPolygon")) {
      return(layer$data)
    }
  }
  NULL
}

# The rows that had a confidence interval drawn for them, which are the
# segments carrying the estimates rather than the ones drawn as furniture.
fy_interval_ends <- function(x) {
  forest <- fy_forest_of(x)
  parts <- lapply(forest$layers, function(layer) {
    d <- layer$data
    if (inherits(layer$geom, "GeomSegment") && is.data.frame(d) &&
        all(c("low", "high") %in% names(d))) {
      d
    }
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) {
    return(data.frame())
  }
  do.call(rbind, parts)
}

# The subgroup effect worked out by hand from the interaction model, which is
# what the package is supposed to reproduce.
fy_hand_lincom <- function(fit, exposure, interaction_term) {
  b <- stats::coef(fit)
  v <- stats::vcov(fit)
  terms <- c(exposure, interaction_term)
  list(
    estimate = sum(b[terms]),
    se = sqrt(sum(v[terms, terms]))
  )
}

# The layer that draws the marks, and the one that draws the intervals. A
# figure is several panels joined, and which layer is which depends on what
# the figure carries, so they are found by what they hold rather than by
# where they sit. The columns are ggplot2's own, so they are spelled the way
# ggplot2 spells them -- `colour` -- rather than the way the package does.
fy_marks <- function(x) {
  data <- ggplot2::ggplot_build(x)$data
  hit <- Filter(function(d) all(c("shape", "fill") %in% names(d)), data)
  hit[[length(hit)]]
}

fy_bars <- function(x) {
  data <- ggplot2::ggplot_build(x)$data
  hit <- Filter(function(d) all(c("xend", "colour") %in% names(d)) &&
                  nrow(d) > 0L, data)
  hit[[length(hit)]]
}

# Runs `f` on a device of a given width, since how the panels of a figure are
# laid out is settled against the width it is being drawn at.
fy_with_device_width <- function(cm, f) {
  file <- tempfile(fileext = ".png")
  grDevices::png(file, width = cm, height = 10, units = "cm", res = 72)
  on.exit({
    grDevices::dev.off()
    unlink(file)
  }, add = TRUE)
  f()
}
