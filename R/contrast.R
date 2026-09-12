# Building the contrast vector for an exposure effect.
#
# The contrast is not assembled by picking coefficients out by name. It is the
# difference between two rows of the model's own design matrix, one with the
# exposure at its baseline and one with the exposure at the value being
# compared, everything else held equal. Every column that does not involve the
# exposure cancels, so the difference is exactly the exposure effect.

# Terms involving the exposure, split into its own effect and its interactions.
fy_exposure_terms <- function(info, exposure) {
  vars <- info$term_vars
  # How many variables the term is of, not how many coefficients it produced:
  # a spline is one variable spread over several columns, and an interaction is
  # several variables whether or not either of them is splined.
  order <- info$term_order %||% lengths(vars)
  is_exposure <- vapply(vars, function(v) exposure %in% v, logical(1))
  list(
    main = names(vars)[is_exposure & order == 1L],
    interaction = names(vars)[is_exposure & order > 1L]
  )
}

# The coefficients produced by a set of term labels.
fy_term_columns <- function(info, labels) {
  unique(unlist(info$term_map[labels], use.names = FALSE))
}

# The variables an exposure is interacted with in this model.
fy_interacting_vars <- function(info, exposure) {
  terms <- fy_exposure_terms(info, exposure)
  setdiff(unique(unlist(info$term_vars[terms$interaction], use.names = FALSE)), exposure)
}

# The design matrix rows for `newdata`, aligned to info$coef by position.
#
# The matrix is rebuilt from the fit's own terms object and matched to the
# coefficient vector by position, once the intercept column has been reconciled.
fy_design_matrix <- function(info, newdata) {
  fit <- info$fit
  tt <- stats::delete.response(stats::terms(fit))
  # The terms object carries `predvars`, which records the knots and centring
  # chosen when the model was fitted. Going back through model.frame() makes
  # model.matrix() use them, so a basis such as ns(no2, 3) or poly(age, 2) is
  # reproduced as it was fitted rather than recomputed from these two rows.
  frame <- stats::model.frame(tt, data = newdata, xlev = fy_xlevels(fit),
                              na.action = stats::na.pass)
  mm <- stats::model.matrix(tt, data = frame,
                            contrasts.arg = fy_contrasts(fit))

  # A multi-equation fit builds every one of its equations from this same
  # design, so the matrix is as wide as one equation rather than as wide as the
  # coefficient vector.
  n_base <- info$n_base %||% info$n_full

  if (ncol(mm) == n_base + 1L && fy_is_intercept_column(colnames(mm)[1L])) {
    # The fit carries no intercept, as survival fits do not.
    mm <- mm[, -1L, drop = FALSE]
  }

  if (ncol(mm) != n_base) {
    stop(
      "the design matrix of this fit has ", ncol(mm), " columns but the model ",
      "has ", n_base, " coefficients, so an exposure contrast cannot be ",
      "built for a fit of class ", paste(class(fit), collapse = "/"),
      call. = FALSE
    )
  }

  if (!is.null(info$equations)) {
    # Left in the design's own space; fy_equation_row() places it in the block
    # of whichever equation the row is of.
    return(mm)
  }

  # Drop the same positions that were dropped from the coefficient vector,
  # so aliased coefficients do not shift the alignment.
  mm[, info$kept, drop = FALSE]
}

# One row of the contrast matrix for one comparison between two outcome levels.
#
# `dx` is the difference between two rows of the design, in the design's own
# space. The estimate wanted is the log odds ratio of `level` against
# `reference`, and each equation is that level against the level the fit took
# as its baseline, so the contrast is the block of one minus the block of the
# other. The fitted reference level has no block, being the zero the others are
# measured from, and drops out of the difference on its own.
fy_equation_row <- function(info, dx, level, reference) {
  out <- numeric(length(info$coef))
  place <- function(lv, sign) {
    block <- info$equations$blocks[[lv]]
    if (is.null(block)) {
      # The level the fit itself was referred to.
      return(NULL)
    }
    keep <- !is.na(block)
    out[block[keep]] <<- out[block[keep]] + sign * dx[keep]
    NULL
  }
  place(level, 1)
  place(reference, -1)
  out
}

# The comparisons between outcome levels a figure of a multi-equation fit is
# made of: every level except the one they are all read against, one row apiece.
#
# `reference` names that level. The default is the level the fit was referred
# to, which is the first level of the outcome factor; naming another is not a
# refit, because the odds ratio of A against B is the odds ratio of A against
# the baseline less that of B against it, and the covariance of the two is
# already in the model.
fy_outcome_comparisons <- function(info, reference = NULL,
                                   reference_row = FALSE) {
  if (is.null(info$equations)) {
    if (!is.null(reference)) {
      stop(
        "`outcome_reference` names the outcome level every estimate is read ",
        "against, which only a model of more than two outcome levels has. ",
        "This is a ", paste(class(info$fit), collapse = "/"),
        " fit of one equation, whose reference is set by how the outcome ",
        "itself is coded.",
        call. = FALSE
      )
    }
    return(NULL)
  }

  levels <- info$equations$levels
  if (is.null(reference)) {
    reference <- info$equations$reference
  } else {
    checkmate::assert_string(reference, min.chars = 1L)
    if (!reference %in% levels) {
      stop(
        "\"", reference, "\" is not a level of the outcome of this model. Its ",
        "levels are ", paste0("\"", levels, "\"", collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  out <- lapply(setdiff(levels, reference), function(lv) {
    list(level = lv, reference = reference, is_reference = FALSE,
         label = paste0(lv, " vs ", reference))
  })

  # The level everything is read against, drawn as a row of its own. It carries
  # no estimate -- it is the definition the others are differences from, and is
  # 1 on a ratio scale and 0 on the scale the model was fitted on, with no
  # interval either way -- but a reader who cannot see it on the figure has to
  # take the reference from the row labels. It comes first, as the reference
  # level of a categorical exposure does.
  if (isTRUE(reference_row)) {
    out <- c(
      list(list(level = reference, reference = reference, is_reference = TRUE,
                label = reference)),
      out
    )
  }
  out
}

fy_is_intercept_column <- function(x) {
  isTRUE(x %in% c("(Intercept)", "Intercept"))
}

# A reference row holding every covariate at a fixed value. The value itself is
# irrelevant, because the covariate columns cancel in the difference, but it
# has to be one the design can be evaluated at, so an observed row is reused.
#
# The source data is preferred over the model frame, because a model frame
# holds terms as they were evaluated: a variable entered as ns(no2, 3) or
# rcs(no2, 4) is stored there as its basis matrix, and setting `no2` on such a
# row would change nothing. The model frame is used only when the source data
# cannot be found, which is workable as long as every term is a plain variable.
fy_reference_row <- function(info) {
  where <- fy_reference_index(info)
  where$frame[where$index, , drop = FALSE]
}

# Which row that is, and which frame it came from, so that the code the app
# writes out can name the row foresty actually used rather than a row of its
# own choosing.
fy_reference_index <- function(info) {
  frame <- if (!is.null(info$data)) info$data else info$mf
  if (is.null(frame)) {
    stop(
      "the data this model was fitted from could not be found, so a contrast ",
      "cannot be built. Keep the data frame in scope under the name used in ",
      "the call, or refit with the data still available.",
      call. = FALSE
    )
  }
  # Completeness is judged on the variables the model uses, so that an unrelated
  # column full of missing values does not rule every row out.
  used <- intersect(all.vars(stats::formula(info$fit)), names(frame))
  complete <- which(stats::complete.cases(frame[, used, drop = FALSE]))
  list(
    frame = frame,
    index = if (length(complete)) complete[1L] else 1L,
    from_data = !is.null(info$data)
  )
}

# The values a model variable takes. The model frame is asked first, so that
# the values seen are the ones actually analysed, but a variable transformed on
# its way into the model is stored there as its basis matrix and is only
# recoverable from the source data.
fy_variable <- function(info, name) {
  from_frame <- info$mf[[name]]
  if (!is.null(from_frame) && !is.matrix(from_frame)) {
    return(from_frame)
  }
  if (!is.null(info$data)) {
    return(info$data[[name]])
  }
  from_frame
}

fy_is_categorical <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

# The increment an effect is reported per, as a number.
#
# A number is taken as it stands. `"iqr"` is taken from the data: the
# interquartile range of the exposure as the model saw it, which is what a
# reader wants for an exposure whose units mean nothing on their own -- a
# pollutant, a biomarker, a score. The range it came to is carried back as a
# label, because an effect per interquartile range is not readable without the
# range: two cohorts have two of them, and a figure that does not say which was
# used cannot be compared with anything.
fy_resolve_contrast <- function(contrast, x, exposure) {
  if (is.numeric(contrast)) {
    checkmate::assert_number(contrast)
    return(contrast)
  }
  if (!is.character(contrast) || length(contrast) != 1L ||
      !identical(tolower(contrast), "iqr")) {
    stop(
      "`contrast` must be a number, as `contrast = 10`, or \"iqr\" for the ",
      "interquartile range of \"", exposure, "\" in the data the model was ",
      "fitted to",
      call. = FALSE
    )
  }
  if (!is.numeric(x)) {
    stop(
      "`contrast = \"iqr\"` is the interquartile range of \"", exposure,
      "\", which a categorical exposure does not have: its comparisons are ",
      "its levels. Use `at` to compare two of them.",
      call. = FALSE
    )
  }

  iqr <- stats::IQR(x, na.rm = TRUE)
  if (!is.finite(iqr) || iqr <= 0) {
    stop(
      "the interquartile range of \"", exposure, "\" is ", fy_trim_number(iqr),
      ", so there is no difference to report an effect per. Name the increment ",
      "yourself, as `contrast = 10`.",
      call. = FALSE
    )
  }
  # The range itself is used as it is; the label carries it to three figures,
  # which is how a paper writes it and is enough to know which range was meant.
  structure(iqr, label = paste0(
    "per IQR, ", format(iqr, digits = 3, trim = TRUE, drop0trailing = TRUE)
  ))
}

# The comparisons to make for an exposure.
#
# A categorical exposure yields one comparison per level, the reference level
# included: it carries no estimate of its own but is shown, because a row
# labelled only "Urban" cannot be read without seeing what it is being
# compared with. `contrast` says nothing about such an exposure, whose
# comparisons are its levels, and `at` names two of those levels.
#
# A continuous exposure yields a single comparison, of a value against that
# value plus `contrast`. An increment that was asked for is written beside the
# exposure -- "NO2 (per 10)" -- because a figure whose numbers change with
# `contrast` and whose labels do not cannot be read. No unit is invented for
# it, since the package has no way of knowing what the numbers in a column
# mean; give one through `labels`.
#
# An increment is written wherever it was named, one included: `contrast = 1`
# draws "NO2 (per 1)", since a figure saying what its rows are per is read the
# same way whatever the number is. `NULL`, the default, is one unit and says
# nothing, which is what leaves a plain "NO2" on a figure nobody asked the
# question of.
#
# `contrast = "iqr"` is the increment taken from the data rather than named:
# an exposure whose units mean nothing to a reader -- a pollutant, a biomarker,
# a score -- is reported per interquartile range, and the range that was used
# is written beside the exposure so that the figure still says what a row is
# the effect of.
fy_exposure_values <- function(info, exposure, contrast = NULL, at = NULL) {
  # Whether the increment was named or left to the default, which is what
  # decides whether the figure says it. The number itself is one either way.
  named <- !is.null(contrast)
  if (!named) {
    contrast <- 1
  }
  x <- fy_variable(info, exposure)
  if (is.null(x)) {
    stop(
      "\"", exposure, "\" is not a variable in this model. Its terms are ",
      paste0("\"", names(info$term_map), "\"", collapse = ", "), ".",
      fy_transformed_hint(exposure),
      call. = FALSE
    )
  }
  # A basis built before the model was fitted and entered as columns of its own
  # is not a variable this package can contrast two values of. The columns
  # record nothing that ties them to the variable they were built from -- not
  # the knots, not the variable's name -- so there is no way to ask what the
  # design matrix looks like at two values of it. A basis built inside the
  # formula does record all of that, in the `predvars` of the terms object, and
  # is rebuilt from them with the knots it was fitted with.
  if (is.matrix(x) && ncol(x) > 1L) {
    stop(
      "\"", exposure, "\" is a matrix of ", ncol(x), " columns in the data ",
      "this model was fitted to -- a spline basis built before fitting, by ",
      "the look of it. Its columns record nothing that ties them to the ",
      "variable they were built from, so foresty cannot say what the design ",
      "matrix looks like at two values of that variable. Build the basis in ",
      "the formula instead, as `ns(x, 3)` or `rcs(x, 4)` with the same knots, ",
      "refit, and pass the variable itself as the exposure with ",
      "`at = c(from, to)`: the knots are then taken from the fitted terms ",
      "and the two values are contrasted on the curve the model actually has.",
      call. = FALSE
    )
  }
  contrast <- fy_resolve_contrast(contrast, x, exposure)
  named_contrast <- attr(contrast, "label")
  contrast <- as.numeric(contrast)

  if (!is.null(at)) {
    if (length(at) != 2L) {
      stop("`at` must give exactly two values of \"", exposure,
           "\": the baseline and the value compared with it", call. = FALSE)
    }
    # Both say which two values are being compared, and one of them would have
    # to be ignored.
    if (!isTRUE(all.equal(contrast, 1))) {
      said <- if (is.null(named_contrast)) {
        fy_trim_number(contrast)
      } else {
        "\"iqr\""
      }
      stop(
        "`at` and `contrast` both say which two values of \"", exposure,
        "\" are compared, so only one of them can be given: `at = c(",
        at[[1L]], ", ", at[[2L]], ")` names the values, `contrast = ", said,
        "` asks for the effect per ", fy_trim_number(contrast), " of them.",
        call. = FALSE
      )
    }
    # Which two values were compared is a fact about the whole figure rather
    # than about one row of it -- every row is that same comparison, taken
    # within another subgroup -- so it is written beside the exposure wherever
    # the exposure is named, and not as a level of it.
    return(structure(
      list(list(
        from = at[[1L]], to = at[[2L]],
        level = NA_character_,
        contrast_label = paste0(fy_trim_number(at[[1L]]), " \u2192 ",
                                fy_trim_number(at[[2L]])),
        reference = FALSE
      )),
      explicit = TRUE
    ))
  }

  if (fy_is_categorical(x)) {
    if (!isTRUE(all.equal(contrast, 1))) {
      warning(
        "\"", exposure, "\" is categorical, so `contrast` says nothing about ",
        "it: its comparisons are its levels. Use `at` to compare two of them.",
        call. = FALSE
      )
    }
    x <- as.factor(x)
    levs <- levels(droplevels(x))
    if (length(levs) < 2L) {
      stop("\"", exposure, "\" has only one level in the fitted data",
           call. = FALSE)
    }
    return(lapply(seq_along(levs), function(i) {
      list(
        from = factor(levs[1L], levels = levels(x)),
        to = factor(levs[i], levels = levels(x)),
        level = levs[i],
        contrast_label = NA_character_,
        reference = i == 1L
      )
    }))
  }

  if (!is.numeric(x)) {
    stop("\"", exposure, "\" is neither numeric nor categorical, so its effect ",
         "is not defined; use `at` to name the two values to contrast",
         call. = FALSE)
  }

  base <- stats::median(x, na.rm = TRUE)
  if (!is.null(named_contrast)) {
    return(list(list(
      from = base, to = base + contrast, level = NA_character_,
      contrast_label = named_contrast, reference = FALSE
    )))
  }
  list(list(
    from = base, to = base + contrast, level = NA_character_,
    contrast_label = if (!named) {
      NA_character_
    } else {
      paste0("per ", fy_trim_number(contrast))
    },
    reference = FALSE
  ))
}

# Builds the contrast matrix for one exposure, optionally within one level of
# an effect modifier. One row per non-reference comparison.
#
# `reference_cell` moves the baseline row. Without it the two rows differ in the
# exposure alone and the modifier is held at the level being drawn, so the
# estimate is the exposure effect inside that subgroup. With it the baseline row
# is one named combination of the two variables -- the exposure at one of its
# levels and the modifier at one of its -- and the row compared with it carries
# both the level of the exposure and the level of the modifier the row is of, so
# the estimate is that whole combination against the one reference combination.
fy_contrast_matrix <- function(info, exposure, values, modifier = NULL,
                               modifier_level = NULL, reference_cell = NULL,
                               comparisons = NULL) {
  reference <- fy_reference_row(info)

  # A splined exposure has no single effect to report, because the difference
  # depends on where along the spline it is taken, so the two values have to be
  # named rather than guessed at.
  if (fy_is_splined(info, exposure) && is.null(attr(values, "explicit"))) {
    stop(
      "\"", exposure, "\" is continuous and enters this model through more ",
      "than one coefficient, a spline for instance, so its effect is not a ",
      "single number. Pass `at = c(from, to)` to name the two values to ",
      "contrast, for example `at = c(10, 20)`.",
      call. = FALSE
    )
  }

  wanted <- values[!vapply(values, function(v) isTRUE(v$reference), logical(1))]
  if (!length(wanted)) {
    return(NULL)
  }

  # The difference between the two design rows, one comparison of the exposure
  # at a time. It is the same difference whichever equation it is later placed
  # in, so it is worked out once.
  differences <- lapply(wanted, function(v) {
    baseline <- reference
    compared <- reference
    baseline[[exposure]] <- fy_coerce_like(
      if (is.null(reference_cell)) v$from else reference_cell$exposure_level,
      fy_variable(info, exposure)
    )
    compared[[exposure]] <- fy_coerce_like(v$to, fy_variable(info, exposure))
    if (!is.null(modifier)) {
      lvl <- fy_coerce_like(modifier_level, fy_variable(info, modifier))
      baseline[[modifier]] <- if (is.null(reference_cell)) {
        lvl
      } else {
        fy_coerce_like(reference_cell$modifier_level, fy_variable(info, modifier))
      }
      compared[[modifier]] <- lvl
    }
    nd <- rbind(baseline, compared)
    mm <- fy_design_matrix(info, nd)
    mm[2L, ] - mm[1L, ]
  })

  value_name <- function(v) {
    if (is.na(v$level)) exposure else as.character(v$level)
  }

  # Without equations there is one row per comparison of the exposure. With
  # them there is one per comparison of the exposure within each comparison of
  # the outcome, taken outcome by outcome so that the rows come out in the
  # order the figure draws them.
  if (is.null(comparisons)) {
    L <- do.call(rbind, differences)
    rownames(L) <- vapply(wanted, value_name, character(1))
  } else {
    rows <- list()
    names_out <- character(0)
    for (cmp in comparisons) {
      # The reference level is a definition rather than an estimate, so there
      # is no contrast to take for it.
      if (isTRUE(cmp$is_reference)) {
        next
      }
      for (i in seq_along(differences)) {
        rows[[length(rows) + 1L]] <- fy_equation_row(
          info, differences[[i]], cmp$level, cmp$reference
        )
        names_out <- c(names_out, paste0(value_name(wanted[[i]]), ": ",
                                         cmp$label))
      }
    }
    if (!length(rows)) {
      return(NULL)
    }
    L <- do.call(rbind, rows)
    rownames(L) <- names_out
  }
  colnames(L) <- names(info$coef)
  L
}

# Keeps a replacement value the same type as the column it replaces, so that a
# factor keeps every one of its levels and the design matrix keeps its shape.
fy_coerce_like <- function(value, template) {
  if (is.factor(template)) {
    return(factor(as.character(value), levels = levels(template)))
  }
  if (is.logical(template)) {
    return(as.logical(value))
  }
  if (is.numeric(template)) {
    return(as.numeric(value))
  }
  value
}

# A factor exposure also produces more than one coefficient, but each of those
# is a comparison with the reference level and is reported on its own row. Only
# a numeric exposure spread over several columns, as a spline is, has no single
# effect to report.
fy_is_splined <- function(info, exposure) {
  x <- fy_variable(info, exposure)
  if (fy_is_categorical(x)) {
    return(FALSE)
  }
  terms <- fy_exposure_terms(info, exposure)
  # Counted per equation, since a multi-equation fit produces the same
  # coefficient once in each of them and that is not a spread-out exposure.
  per_equation <- if (is.null(info$equations)) {
    1L
  } else {
    length(info$equations$blocks)
  }
  length(fy_term_columns(info, terms$main)) > per_equation
}

# The rows of the model frame an estimate was taken over, so that the counts
# beside it describe the same people. A categorical exposure is counted level
# by level; a continuous one over everybody in the subgroup.
#
# `whole_exposure` counts over every level of it instead, for the one row of a
# figure that is not about a value of the exposure at all: the level of a
# multinomial outcome the other rows are read against is drawn once for the
# whole of it, and counting it at one level of the exposure would put a number
# beside it that its own label does not account for.
fy_estimate_rows <- function(info, exposure, value, modifier = NULL,
                             modifier_level = NULL, whole_exposure = FALSE) {
  x <- info$mf[[exposure]]
  # The level counted is the one the comparison is of, which is the value the
  # contrast was taken to -- named by `at` where it was given, and the level of
  # the row otherwise.
  level <- if (!whole_exposure && !is.null(x) && fy_is_categorical(x)) {
    value$to
  }
  fy_rows_at(info, exposure, level, modifier, modifier_level)
}

# The rows of the model frame at one level of the exposure, one level of the
# modifier, or both. `NULL` for either takes every level of it, so that the
# people a row is counted over and the people it is compared with are picked
# out the same way and differ only in which level was asked for.
fy_rows_at <- function(info, exposure = NULL, exposure_level = NULL,
                       modifier = NULL, modifier_level = NULL) {
  keep <- rep(TRUE, info$n)
  if (!is.null(exposure_level) && !is.null(exposure)) {
    x <- info$mf[[exposure]]
    keep <- keep & !is.na(x) & as.character(x) == as.character(exposure_level)
  }
  if (!is.null(modifier_level) && !is.null(modifier)) {
    m <- info$mf[[modifier]]
    keep <- keep & !is.na(m) & as.character(m) == as.character(modifier_level)
  }
  which(keep)
}
