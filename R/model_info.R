# One normalized description of a fitted model.
#
# Every place in the package that needs to know something about a fit goes
# through fy_model_info(). Model classes are special-cased here and nowhere
# else, so adding support for another class means adding a method here rather
# than editing the estimation or plotting code.

# Fixed-effect coefficients and their covariance, aligned to each other -------

# Returns a list of `coef` (named numeric) and `vcov` (matching square matrix).
# The two are intersected by name, because the classes do not agree on what
# either holds: MASS::polr() keeps the cutpoints in vcov() only, and an aliased
# coefficient is NA in coef() and, depending on the class, either dropped from
# vcov() or kept there as a row of nothing. Taking the names they have in
# common, once the NA coefficients are out of the way, leaves the coefficients
# that were actually estimated and a covariance of the same ones.
fy_coefs <- function(fit, vcov_matrix = NULL) {
  UseMethod("fy_coefs")
}

#' @export
fy_coefs.default <- function(fit, vcov_matrix = NULL) {
  b <- stats::coef(fit)
  if (is.null(b) || !is.numeric(b)) {
    stop(
      "no usable coef() method for a fit of class ",
      paste(class(fit), collapse = "/"),
      call. = FALSE
    )
  }
  fy_align_coefs(b, vcov_matrix %||% stats::vcov(fit), fit)
}

#' @export
fy_coefs.merMod <- function(fit, vcov_matrix = NULL) {
  fy_align_coefs(lme4::fixef(fit), vcov_matrix %||% stats::vcov(fit), fit)
}

#' @export
fy_coefs.multinom <- function(fit, vcov_matrix = NULL) {
  b <- stats::coef(fit)
  if (is.matrix(b)) {
    b <- stats::setNames(
      as.vector(t(b)),
      paste(rep(rownames(b), each = ncol(b)), rep(colnames(b), nrow(b)), sep = ":")
    )
  }
  fy_align_coefs(b, vcov_matrix %||% stats::vcov(fit), fit)
}

fy_align_coefs <- function(b, v, fit) {
  if (is.null(names(b))) {
    stop(
      "the coefficients of a fit of class ",
      paste(class(fit), collapse = "/"), " are unnamed, so they cannot be ",
      "matched to their covariance matrix",
      call. = FALSE
    )
  }
  v <- as.matrix(v)

  # When the covariance matrix is unnamed, the coefficients are taken to be in
  # the order they were reported and any extra trailing parameters are dropped.
  # There is nothing to check that against: a matrix passed to `vcov` without
  # dimnames is trusted to be in the order coef() reports, and one in another
  # order would be used as though it were in that one. Name its rows and
  # columns and it is matched by name instead.
  if (is.null(colnames(v)) && nrow(v) >= length(b)) {
    v <- v[seq_along(b), seq_along(b), drop = FALSE]
    dimnames(v) <- list(names(b), names(b))
  }

  aliased <- is.na(b)
  keep <- intersect(names(b)[!aliased], colnames(v))
  if (!length(keep)) {
    stop(
      "the coefficient names and the covariance matrix of a fit of class ",
      paste(class(fit), collapse = "/"), " have nothing in common",
      call. = FALSE
    )
  }
  list(
    coef = b[keep],
    vcov = v[keep, keep, drop = FALSE],
    # Positions of the retained coefficients within the full coefficient
    # vector, so that a design matrix can be lined up with them by position.
    kept = match(keep, names(b)),
    n_full = length(b)
  )
}

# Robust and cluster-robust covariance ---------------------------------------

# Returns the covariance matrix to use, or NULL to keep the model's own.
#
# The sandwich estimators come from the sandwich package rather than being
# written here. Where sandwich does not apply, the fitting function's own
# mechanism is pointed at instead of returning a number that would look
# plausible and be wrong.
fy_robust_vcov <- function(fit, vcov = NULL, cluster = NULL, data = NULL) {
  if (is.null(vcov) && is.null(cluster)) {
    return(NULL)
  }
  if (is.matrix(vcov)) {
    return(vcov)
  }
  if (is.function(vcov)) {
    return(vcov(fit))
  }

  fy_check_sandwich_supported(fit, cluster)
  fy_require("sandwich", "compute robust standard errors")

  if (!is.null(cluster)) {
    cluster <- fy_cluster_vector(fit, cluster, data)
    type <- if (is.null(vcov)) "HC0" else fy_hc_type(vcov)
    return(sandwich::vcovCL(fit, cluster = cluster, type = type))
  }
  sandwich::vcovHC(fit, type = fy_hc_type(vcov))
}

fy_hc_type <- function(vcov) {
  checkmate::assert_string(vcov)
  if (identical(vcov, "robust")) {
    # HC1 is the small-sample correction that "robust" means in most other
    # software, so it is what an unqualified request gets here.
    return("HC1")
  }
  if (!grepl("^HC[0-4]?$", vcov)) {
    stop(
      "`vcov` must be \"robust\", one of \"HC0\" to \"HC4\", a function, or a ",
      "matrix, not \"", vcov, "\"",
      call. = FALSE
    )
  }
  vcov
}

# The classes sandwich cannot do this for, each with the thing to do instead.
fy_check_sandwich_supported <- function(fit, cluster) {
  if (inherits(fit, "coxph") && is.null(cluster)) {
    stop(
      "a heteroskedasticity-consistent estimator is not the robust variance ",
      "for a Cox model. Refit with `coxph(..., robust = TRUE)`, or pass ",
      "`cluster` here for the grouped version; a fit that is already robust ",
      "is used as it stands.",
      call. = FALSE
    )
  }
  # sandwich works from the estimating function of a fit, and has no method for
  # a multinomial one. Left to itself it comes back as a failure to dispatch
  # estfun(), which says nothing about what was asked for.
  if (inherits(fit, "multinom")) {
    stop(
      "the sandwich package has no estimating function for a ",
      "nnet::multinom() fit, so robust standard errors cannot be computed ",
      "for one here. Pass a covariance matrix of your own as `vcov`, or a ",
      "function that returns one.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Accepts a variable name, a one-sided formula, or the values themselves.
#
# `cluster` says which observations belong together, so it has to identify
# them. A bare TRUE is the commonest way of getting this wrong, and left to
# itself it reaches sandwich and comes back as a mismatch between the length
# of the cluster and the number of observations, which says nothing useful.
fy_cluster_vector <- function(fit, cluster, data) {
  if (inherits(cluster, "formula")) {
    return(cluster)
  }
  if (is.logical(cluster)) {
    stop(
      "`cluster` names the variable that identifies the clusters, not whether ",
      "to cluster. Pass a column name, a vector of identifiers, or a ",
      "one-sided formula: cluster = \"practice_id\", cluster = data$practice_id ",
      "or cluster = ~practice_id.",
      call. = FALSE
    )
  }
  if (is.character(cluster) && length(cluster) == 1L) {
    # The source data rather than the model frame: a cluster identifier is
    # rarely a term of the model -- a practice, a household, a centre -- so the
    # model frame usually does not carry it at all.
    frame <- data %||% fy_model_frame(fit)
    if (is.null(frame[[cluster]])) {
      stop(
        "\"", cluster, "\" is not a column of the data this model was fitted ",
        "from, so it cannot identify the clusters",
        call. = FALSE
      )
    }
    return(fy_cluster_column(fit, frame[[cluster]], cluster))
  }

  used <- stats::nobs(fit)
  if (!is.null(used) && !is.na(used) && length(cluster) != used) {
    stop(
      "`cluster` has ", length(cluster), " value",
      if (length(cluster) == 1L) "" else "s", " but the model was fitted to ",
      used, " observations. Give one cluster identifier per observation, or ",
      "name a column of the data with cluster = \"variable_name\".",
      call. = FALSE
    )
  }
  cluster
}

# A cluster column taken from the source data, checked against the rows the
# model was actually fitted to.
#
# The two differ whenever the fit dropped rows, and sandwich puts back only the
# ones it can account for: it drops the rows recorded in the fit's `na.action`,
# so a column with a missing value in it needs nothing done to it here. Rows
# left out any other way -- `subset =` most of all -- are recorded nowhere, and
# left to itself the mismatch surfaces from inside sandwich as a complaint
# about estfun() that says nothing about what was asked for or what to do.
fy_cluster_column <- function(fit, values, name) {
  used <- stats::nobs(fit)
  if (is.null(used) || is.na(used) || length(values) == used) {
    return(values)
  }
  dropped <- fit[["na.action"]]
  if (!is.null(dropped) && length(values) - length(dropped) == used) {
    return(values)
  }
  stop(
    "\"", name, "\" has ", length(values), " value",
    if (length(values) == 1L) "" else "s", " in the data this model was ",
    "fitted from, but the model was fitted to ", used, " observation",
    if (used == 1L) "" else "s", ", so the clusters cannot be matched to the ",
    "rows behind the estimates. That happens when rows were left out by ",
    "something the fit does not record, `subset =` most often. Pass the ",
    "clusters as a one-sided formula, `cluster = ~", name,
    "`, which sandwich evaluates over the rows the model kept, or fit the ",
    "model to the subset itself.",
    call. = FALSE
  )
}

# Fits holding more than one equation -----------------------------------------

# A multinomial logistic regression is K - 1 logistic regressions sharing one
# likelihood: one equation per non-reference level of the outcome, each
# comparing that level with the level the fit took as its reference. So the
# exposure has K - 1 effects rather than one, and a figure of it has K - 1 rows.
#
# Returns NULL for a fit holding one equation, which is every other class and
# also a multinom() of a two-level outcome -- that one is an ordinary logistic
# regression fitted by a different algorithm, and coef() gives it back as a
# plain named vector rather than as a matrix.
#
# `blocks` gives, for each non-reference level, where that equation's
# coefficients sit in the flattened coefficient vector, in the order of `base`.
# The reference level has no block: it is the zero every other equation is
# measured from, which is what makes a comparison between two non-reference
# levels the difference of their two blocks.
fy_equations <- function(fit, coef_names) {
  if (!inherits(fit, "multinom")) {
    return(NULL)
  }
  b <- try(stats::coef(fit), silent = TRUE)
  if (inherits(b, "try-error") || !is.matrix(b)) {
    return(NULL)
  }
  fitted_levels <- rownames(b)
  observed <- fy_outcome_levels(fit)
  reference <- setdiff(observed, fitted_levels)
  if (length(observed) != length(fitted_levels) + 1L ||
      length(reference) != 1L) {
    stop(
      "the outcome levels of this nnet::multinom() fit could not be matched to ",
      "its equations: it has coefficients for ",
      paste0("\"", fitted_levels, "\"", collapse = ", "),
      " and an outcome taking ", length(observed), " level",
      if (length(observed) == 1L) "" else "s",
      ". Refit with the outcome as a factor of the levels you want compared.",
      call. = FALSE
    )
  }

  base <- colnames(b)
  blocks <- lapply(fitted_levels, function(lv) {
    match(paste(lv, base, sep = ":"), coef_names)
  })
  names(blocks) <- fitted_levels
  list(
    # In the order the outcome's levels are in, the reference included, since
    # that is the order the rows of a figure are drawn in.
    levels = observed,
    reference = reference,
    base = base,
    blocks = blocks
  )
}

# The levels the outcome of a fit takes, in their own order and dropping any
# the data never showed, since a level nobody was in is not a comparison.
fy_outcome_levels <- function(fit) {
  y <- try(stats::model.response(fy_model_frame(fit)), silent = TRUE)
  if (inherits(y, "try-error") || is.null(y) || is.matrix(y)) {
    return(character(0))
  }
  levels(droplevels(as.factor(y)))
}

# The term map of a multi-equation fit: every term contributes the coefficient
# it produced in each of the equations, so a joint test of a term tests it
# across all of them, which is the test of "does this term do anything at all".
fy_expand_term_map <- function(base_map, equations, coef_names) {
  out <- lapply(base_map, function(cols) {
    flat <- as.vector(outer(names(equations$blocks), cols, paste, sep = ":"))
    intersect(flat, coef_names)
  })
  out[lengths(out) > 0]
}

# Model terms mapped to the coefficients they produced ------------------------

# Returns a named list: term label -> names of the coefficients it contributes.
# A term contributes more than one coefficient whenever it is a factor with
# more than two levels or a spline, so this cannot be done by name matching.
fy_term_map <- function(fit, coef_names) {
  # An S4 fit -- lme4's lmer() and glmer() among them -- cannot be subsetted at
  # all, so it is not asked for an `assign` element it could not carry; its
  # terms are read off the design matrix below like any other fit's.
  assign <- if (isS4(fit)) NULL else fit[["assign"]]

  # Some model classes carry `assign` as a named list of coefficient positions.
  # It indexes the coefficient vector, so it is of no use to a class whose
  # coef() is a matrix -- the positions would be read down the wrong shape --
  # and such a fit is left to the design matrix below.
  if (is.list(assign) && !is.null(names(assign)) && length(assign)) {
    full <- names(stats::coef(fit))
    if (!is.null(full)) {
      out <- lapply(assign, function(idx) intersect(full[idx], coef_names))
      return(out[lengths(out) > 0])
    }
  }

  # lm and glm need the design matrix, whose `assign` attribute indexes into
  # the term labels, with 0 standing for the intercept.
  mm <- try(stats::model.matrix(fit), silent = TRUE)
  if (inherits(mm, "try-error")) {
    return(list())
  }
  positions <- attr(mm, "assign")
  labels <- attr(stats::terms(fit), "term.labels")
  if (is.null(positions) || is.null(labels)) {
    return(list())
  }
  out <- lapply(seq_along(labels), function(j) {
    intersect(colnames(mm)[positions == j], coef_names)
  })
  names(out) <- labels
  out[lengths(out) > 0]
}

# The variables behind each term label, and how many variables the term is of.
#
# A term label is one or more variable expressions joined by colons, so it is
# split on the colons at the top of the parsed expression rather than on the
# character: `no2:sex` is a term of two variables, and `splines::ns(no2, 3)`,
# which carries two colons and no interaction, is a term of one.
#
# The variables of an expression cannot simply be all.vars() of it, because an
# argument naming an object is not a variable of the model: the knots in
# `rcs(no2, knots)` or `ns(no2, knots = k)` are a vector that happens to be
# named there, and reading them as variables makes the term look like an
# interaction between the exposure and its own knots. So the symbols are
# matched against the columns the model was fitted from, and where that cannot
# settle it the term is taken to be a term in its first argument, which is the
# convention every basis function in R follows.
fy_term_structure <- function(labels, known = NULL) {
  parts <- lapply(labels, fy_term_parts)
  list(
    vars = stats::setNames(
      lapply(parts, function(p) {
        unique(unlist(lapply(p, fy_part_vars, known = known),
                      use.names = FALSE))
      }),
      labels
    ),
    order = stats::setNames(
      vapply(parts, function(p) max(1L, length(p)), integer(1)),
      labels
    )
  )
}

# The variable expressions a term label is built from.
fy_term_parts <- function(label) {
  expr <- try(str2lang(label), silent = TRUE)
  if (inherits(expr, "try-error")) {
    return(list())
  }
  split <- function(e) {
    if (is.call(e) && length(e) == 3L && identical(e[[1L]], as.name(":"))) {
      return(c(split(e[[2L]]), split(e[[3L]])))
    }
    list(e)
  }
  split(expr)
}

fy_part_vars <- function(part, known = NULL) {
  vars <- all.vars(part)
  if (length(vars) < 2L) {
    return(vars)
  }
  hit <- intersect(vars, known)
  if (length(hit)) {
    return(hit)
  }
  if (is.call(part) && length(part) >= 2L) {
    return(all.vars(part[[2L]]))
  }
  vars
}

# Effect measure --------------------------------------------------------------

fy_measure <- function(fit, outcome = fy_outcome_label(fit)) {
  fam <- try(stats::family(fit), silent = TRUE)
  fam_name <- if (inherits(fam, "try-error") || is.null(fam)) NA_character_ else fam$family
  link <- if (inherits(fam, "try-error") || is.null(fam)) NA_character_ else fam$link

  if (inherits(fit, "coxph")) {
    return(fy_measure_spec("HR", outcome))
  }
  if (inherits(fit, "polr")) {
    return(fy_measure_spec("OR", outcome))
  }
  # A multinomial logit reports no family, and each of its equations is a
  # logistic regression of one outcome level against the reference level, so
  # what it produces is an odds ratio like any other logistic regression.
  if (inherits(fit, "multinom")) {
    return(fy_measure_spec("OR", outcome))
  }
  if (!is.na(fam_name)) {
    if (fam_name == "binomial" && identical(link, "logit")) {
      return(fy_measure_spec("OR", outcome))
    }
    if (fam_name %in% c("poisson", "quasipoisson") && identical(link, "log")) {
      # A Poisson model carrying person-time is a rate model, and its ratio is
      # a rate ratio rather than a risk ratio.
      measure <- if (fy_has_offset(fit)) "IRR" else "RR"
      return(fy_measure_spec(measure, outcome))
    }
    if (fam_name %in% c("binomial", "quasibinomial") && identical(link, "log")) {
      return(fy_measure_spec("RR", outcome))
    }
    if (fam_name == "gaussian" && identical(link, "identity")) {
      return(fy_measure_spec("MD", outcome))
    }
  }
  if (inherits(fit, "lm") && !inherits(fit, "glm")) {
    return(fy_measure_spec("MD", outcome))
  }
  fy_measure_spec("Coefficient", outcome)
}

fy_reject_rms <- function(fit) {
  if (!inherits(fit, "rms")) {
    return(invisible(TRUE))
  }
  stop(
    "foresty does not support rms fits. Fit the model with glm(), lm(), or ",
    "survival::coxph() and pass that fit instead.",
    call. = FALSE
  )
}

# MASS::polr() can fit several cumulative-link models.  An odds ratio is only
# defined for its logistic link, so accepting another link and exponentiating
# its coefficients would give a number that looks like an odds ratio but is
# not one.
fy_check_polr_link <- function(fit) {
  if (!inherits(fit, "polr")) {
    return(invisible(TRUE))
  }
  method <- fit[["method"]]
  if (!identical(method, "logistic")) {
    stop(
      "foresty supports MASS::polr() only with method = \"logistic\". ",
      "This fit uses method = \"", method, "\", whose coefficients are not ",
      "log odds and cannot be reported as odds ratios.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# What the model is called, in the words a paper calls it.
#
# A report saying "glm, lm" names the function that fitted the model rather
# than the model, and a reader checking a figure against a methods section is
# looking for "logistic regression". The name follows from the same two things
# the effect measure does -- the class of the fit and its family -- so the two
# always agree: a page reporting odds ratios says logistic regression.
#
# The class is the fallback rather than the answer, since a fit this package
# has never heard of still has to be named something, and what it was fitted
# by is the only honest thing left to say.
fy_model_name <- function(fit) {
  fam <- try(stats::family(fit), silent = TRUE)
  failed <- inherits(fam, "try-error") || is.null(fam)
  fam_name <- if (failed) NA_character_ else fam$family
  link <- if (failed || is.null(fam$link) || is.na(fam$link)) "" else fam$link

  # A negative binomial is a glm whose family carries its theta in its name --
  # "Negative Binomial(1.32)" -- so it is matched on the front of it.
  if (!is.na(fam_name) && grepl("^Negative Binomial", fam_name)) {
    fam_name <- "negbin"
  }

  base <- if (inherits(fit, "coxph")) {
    "Cox proportional hazards model"
  } else if (inherits(fit, "survreg")) {
    "Accelerated failure time model"
  } else if (inherits(fit, "polr")) {
    "Ordinal (Ordered) logistic regression model"
  } else if (inherits(fit, "multinom")) {
    # A two-level outcome leaves multinom() fitting one equation, which is an
    # ordinary logistic regression however it was reached.
    if (is.matrix(stats::coef(fit))) {
      "Multinomial (polytomous) logistic regression model"
    } else {
      "Logistic regression model"
    }
  } else if (identical(fam_name, "negbin")) {
    "Negative binomial regression model"
  } else if (fy_family_is(fam_name, c("binomial", "quasibinomial"))) {
    switch(
      link,
      logit = "Logistic regression model",
      log = "Log-binomial regression model",
      probit = "Probit regression model",
      identity = "Binomial regression model with an identity link",
      cloglog = "Complementary log-log regression model",
      "Binomial regression model"
    )
  } else if (fy_family_is(fam_name, c("poisson", "quasipoisson"))) {
    quasi <- identical(fam_name, "quasipoisson")
    if (identical(link, "log")) {
      if (fy_has_offset(fit)) {
        if (quasi) "Quasi-Poisson rate model" else "Poisson rate model"
      } else if (quasi) {
        "Quasi-Poisson regression model"
      } else {
        "Poisson regression model"
      }
    } else if (quasi) {
      "Quasi-Poisson regression model"
    } else {
      "Poisson regression model"
    }
  } else if (fy_family_is(fam_name, "Gamma")) {
    "Gamma regression model"
  } else if (fy_family_is(fam_name, "inverse.gaussian")) {
    "Inverse Gaussian regression model"
  } else if (fy_family_is(fam_name, "gaussian") && identical(link, "identity")) {
    "Linear regression model"
  } else if (inherits(fit, c("lmerMod", "lme"))) {
    "Linear mixed-effects model"
  } else if (inherits(fit, "lm") && !inherits(fit, "glm")) {
    "Linear regression model"
  } else {
    paste0(class(fit)[1L], " model")
  }

  # How it was fitted, where that is part of what the model is: a mixed model
  # and a GEE answer different questions from the same family and link, and a
  # methods section names which of the two it was.
  if (inherits(fit, c("glmerMod", "lmerMod", "glmmTMB", "lme"))) {
    return(paste0("Mixed-effects ", fy_lower_first(base)))
  }
  if (inherits(fit, c("geeglm", "gee"))) {
    return(paste0(sub(" model$", "", base),
                  " fitted by generalized estimating equations"))
  }
  base
}

fy_family_is <- function(fam_name, wanted) {
  !is.na(fam_name) && fam_name %in% wanted
}

fy_lower_first <- function(x) {
  paste0(tolower(substring(x, 1L, 1L)), substring(x, 2L))
}

# The outcome a measure is a measure of, taken from the left of the formula, so
# that an axis reads "Adjusted odds ratio for asthma" rather than leaving the
# reader to remember which model produced it.
fy_outcome_label <- function(fit) {
  formula <- try(stats::formula(fit), silent = TRUE)
  if (inherits(formula, "try-error") || length(formula) < 3L) {
    return(NA_character_)
  }
  lhs <- formula[[2L]]
  if (is.name(lhs)) {
    return(deparse(lhs))
  }
  variables <- all.vars(lhs)
  if (!length(variables)) {
    return(NA_character_)
  }
  # A survival outcome is written Surv(time, status); the event is what the
  # hazard ratio is about, and it is named last. Anything else built from a
  # call, cbind(events, non_events) for instance, leads with its outcome.
  if (is.call(lhs) && identical(fy_call_name(lhs), "Surv")) {
    return(variables[length(variables)])
  }
  variables[1L]
}

# The function a call names, with any namespace stripped, so that Surv() and
# survival::Surv() are recognised as the same thing.
fy_call_name <- function(x) {
  sub("^.*::", "", paste(deparse(x[[1L]]), collapse = ""))
}

# The measures the package knows by name, which is what tells one of them from
# a measure a caller has described in words of their own.
fy_measure_codes <- function() {
  c("OR", "RR", "HR", "IRR", "MD", "Coefficient")
}

fy_measure_spec <- function(measure, outcome = NA_character_) {
  spec <- switch(
    measure,
    OR = list(label = "Odds ratio", exponentiate = TRUE),
    RR = list(label = "Risk ratio", exponentiate = TRUE),
    HR = list(label = "Hazard ratio", exponentiate = TRUE),
    IRR = list(label = "Incidence rate ratio", exponentiate = TRUE),
    MD = list(label = "Mean difference", exponentiate = FALSE),
    Coefficient = list(label = "Coefficient", exponentiate = FALSE),
    stop(
      "`measure` must be one of \"OR\", \"RR\", \"HR\", \"IRR\", \"MD\" or ",
      "\"Coefficient\", not \"", measure, "\"",
      call. = FALSE
    )
  )
  # `name` is the measure on its own, for the places that build a phrase out of
  # it; `label` names the outcome too, for the places that stand alone.
  spec$name <- spec$label
  spec$label <- fy_measure_label(spec$name, outcome)
  # Whether the measure is a ratio, which stays true however it is drawn. The
  # display scale can be turned off by `exponentiate = FALSE`, but a ratio is
  # still a ratio for the purpose of choosing between a z and a t.
  spec$ratio <- spec$exponentiate
  c(list(measure = measure, outcome = outcome), spec)
}

fy_measure_label <- function(name, outcome) {
  if (is.na(outcome) || !nzchar(outcome)) {
    return(name)
  }
  paste0(name, " for ", outcome)
}

# The same measure left on the scale the model was fitted on: a log odds ratio
# rather than an odds ratio, drawn about zero rather than about one.
#
# `TRUE`, which is the default, is what the measure asks for: a ratio is drawn
# as a ratio. Only a ratio has anything to leave, so it says nothing about a
# mean difference or a coefficient, which are on that scale already and are not
# exponentiated on request -- exp(a mean difference) is not a measure of
# anything.
fy_apply_exponentiate <- function(spec, exponentiate) {
  if (is.null(exponentiate) || isTRUE(exponentiate)) {
    return(spec)
  }
  checkmate::assert_flag(exponentiate)
  if (!isTRUE(spec$ratio)) {
    return(spec)
  }
  spec$exponentiate <- FALSE
  spec$name <- paste0("Log ", tolower(substring(spec$name, 1, 1)),
                      substring(spec$name, 2))
  spec$label <- fy_measure_label(spec$name, spec$outcome)
  spec
}

# The outcome as the caller wants it named.
#
# The name taken from the left of the formula is the name of a column, and a
# column is called what the data set calls it -- `asthma_ever_dx`, `evt5`, `y`
# -- which is not what a figure should say the ratio is a ratio of. Naming it
# here rather than in each title and heading means every place the outcome
# appears says the same thing: the axis, the estimate column, the titles the
# package writes and the HTML report.
#
# `NA` names none, which leaves "Adjusted odds ratio" on its own, for a figure
# whose caption says what the outcome was.
fy_apply_outcome <- function(spec, outcome) {
  if (is.null(outcome)) {
    return(spec)
  }
  if (length(outcome) == 1L && is.na(outcome)) {
    spec$outcome <- NA_character_
  } else {
    checkmate::assert_string(outcome, min.chars = 1L)
    spec$outcome <- outcome
  }
  spec$label <- fy_measure_label(spec$name, spec$outcome)
  spec
}

# Model frame, counts and residual degrees of freedom -------------------------

fy_model_frame <- function(fit) {
  mf <- try(stats::model.frame(fit), silent = TRUE)
  if (inherits(mf, "try-error")) {
    stop(
      "the model frame of this fit could not be recovered, so subgroup sizes ",
      "and contrasts cannot be built; refit with the data still in scope",
      call. = FALSE
    )
  }
  mf
}

# The data frame the model was fitted from, or NULL if it cannot be found.
#
# The model frame is not a substitute for it. A model frame holds terms as they
# were evaluated, so `ns(maternal_age, 3)` is stored as its three-column basis
# under that name, and the original `maternal_age` is not in there at all. The
# design matrix is rebuilt from those evaluated columns and so needs nothing
# more, but naming two values of a splined exposure does: the values are set on
# the untransformed variable and the basis is recomputed from the knots the fit
# recorded.
fy_source_data <- function(fit) {
  call <- stats::getCall(fit)
  if (is.null(call) || is.null(call$data)) {
    return(NULL)
  }
  env <- environment(stats::formula(fit))
  if (is.null(env)) {
    env <- parent.frame()
  }
  data <- try(eval(call$data, envir = env), silent = TRUE)
  if (inherits(data, "try-error") || !is.data.frame(data)) {
    return(NULL)
  }
  data
}

# Number of events behind a fit, or NA when the outcome is not an event.
# model.response() gives a Surv matrix for survival fits, a factor for a
# logistic regression on a factor, and a 0/1 numeric otherwise.
fy_events <- function(mf, rows = NULL) {
  y <- try(stats::model.response(mf), silent = TRUE)
  if (inherits(y, "try-error") || is.null(y)) {
    return(NA_integer_)
  }
  if (!is.null(rows)) {
    y <- if (is.matrix(y)) y[rows, , drop = FALSE] else y[rows]
  }
  if (inherits(y, "Surv")) {
    status <- y[, ncol(y)]
    seen <- unique(status[!is.na(status)])
    # The event is the higher of the two codes, which reads 0/1 and the 1/2
    # coding survival also takes. An outcome of more than one kind of event --
    # a competing risk, a multi-state model -- has no single count to put
    # beside a hazard ratio, and the highest code is one cause of several
    # rather than the events, so nothing is reported rather than one cause
    # labelled as though it were all of them.
    if (length(seen) > 2L || !is.null(attr(y, "states"))) {
      return(NA_integer_)
    }
    return(as.integer(sum(status == max(seen), na.rm = TRUE)))
  }
  if (is.matrix(y)) {
    # A two-column binomial response: successes and failures.
    return(as.integer(sum(y[, 1], na.rm = TRUE)))
  }
  if (is.factor(y)) {
    if (nlevels(y) != 2L) return(NA_integer_)
    return(as.integer(sum(y == levels(y)[2L], na.rm = TRUE)))
  }
  if (is.logical(y)) {
    return(as.integer(sum(y, na.rm = TRUE)))
  }
  if (is.numeric(y) && all(y %in% c(0, 1, NA))) {
    return(as.integer(sum(y == 1, na.rm = TRUE)))
  }
  NA_integer_
}

# Person-time behind a fit, or NA when the model carries no time.
#
# A rate is only interpretable against the time it was accumulated over, so a
# model that has that time reports it. It comes from the survival outcome for
# a Cox model, and from the offset for a Poisson rate model, where the offset
# is the log of the time.
fy_person_time <- function(mf, rows = NULL) {
  y <- try(stats::model.response(mf), silent = TRUE)
  if (!inherits(y, "try-error") && inherits(y, "Surv")) {
    time <- if (identical(attr(y, "type"), "counting")) {
      y[, "stop"] - y[, "start"]
    } else if ("time" %in% colnames(y)) {
      y[, "time"]
    } else {
      NULL
    }
    if (!is.null(time)) {
      if (!is.null(rows)) time <- time[rows]
      return(sum(time, na.rm = TRUE))
    }
  }

  offset <- try(stats::model.offset(mf), silent = TRUE)
  if (!inherits(offset, "try-error") && !is.null(offset)) {
    if (!is.null(rows)) offset <- offset[rows]
    return(sum(exp(offset), na.rm = TRUE))
  }
  NA_real_
}

fy_has_offset <- function(fit) {
  mf <- try(stats::model.frame(fit), silent = TRUE)
  if (inherits(mf, "try-error")) {
    return(FALSE)
  }
  !is.null(stats::model.offset(mf))
}

# Gaussian models are reported with t and F, everything else with the normal
# approximation and a Wald chi-square, so the two need different error degrees
# of freedom when the linear combination is tested.
fy_error_df <- function(fit, ratio) {
  if (ratio) {
    return(Inf)
  }
  df <- try(stats::df.residual(fit), silent = TRUE)
  if (inherits(df, "try-error") || is.null(df) || is.na(df) || df <= 0) {
    return(Inf)
  }
  as.numeric(df)
}

# Assembly --------------------------------------------------------------------

fy_model_info <- function(fit, measure = NULL, exponentiate = NULL, vcov = NULL,
                          cluster = NULL, outcome = NULL) {
  fy_reject_rms(fit)
  fy_check_polr_link(fit)
  data <- fy_source_data(fit)
  robust <- fy_robust_vcov(fit, vcov = vcov, cluster = cluster, data = data)
  cv <- fy_coefs(fit, vcov_matrix = robust)
  from_formula <- fy_outcome_label(fit)
  spec <- if (is.null(measure)) {
    fy_measure(fit, from_formula)
  } else {
    fy_measure_spec(measure, from_formula)
  }
  spec <- fy_apply_exponentiate(spec, exponentiate)
  # An outcome named by the caller replaces the column name taken from the left
  # of the formula, so that the axis reads "Adjusted odds ratio for incident
  # asthma" rather than "for asthma_ever_dx".
  spec <- fy_apply_outcome(spec, outcome)
  # A multi-equation fit has one design matrix and several blocks of
  # coefficients built from it, so its terms are mapped in the design's own
  # names and then spread over the equations.
  equations <- fy_equations(fit, names(cv$coef))
  term_map <- if (is.null(equations)) {
    fy_term_map(fit, names(cv$coef))
  } else {
    fy_expand_term_map(fy_term_map(fit, equations$base), equations,
                       names(cv$coef))
  }
  mf <- fy_model_frame(fit)
  # The names a symbol in a term has to be one of to be a variable of the
  # model rather than something named in the call, such as a knot vector.
  terms <- fy_term_structure(names(term_map),
                             known = unique(c(names(data), names(mf))))

  structure(
    list(
      fit = fit,
      coef = cv$coef,
      vcov = cv$vcov,
      kept = cv$kept,
      n_full = cv$n_full,
      # The equations the fit holds, or NULL where it holds one, and how wide
      # its design matrix is: a multi-equation fit has more coefficients than
      # design columns, one block of them per equation.
      equations = equations,
      n_base = if (is.null(equations)) cv$n_full else length(equations$base),
      robust = !is.null(robust) || fy_fit_is_robust(fit),
      term_map = term_map,
      term_vars = terms$vars,
      # How many variables each term is of, which is what tells an interaction
      # from a basis expanded over several coefficients.
      term_order = terms$order,
      mf = mf,
      data = data,
      n = nrow(mf),
      events = fy_events(mf),
      person_time = fy_person_time(mf),
      measure = spec$measure,
      measure_label = spec$label,
      measure_name = spec$name,
      outcome = spec$outcome,
      exponentiate = spec$exponentiate,
      # Which distribution the estimates are referred to follows from the model,
      # not from the scale it is drawn on, so a logistic regression reported as
      # a log odds ratio is still a z rather than a t.
      error_df = fy_error_df(fit, spec$ratio),
      has_intercept = fy_has_intercept(cv$coef)
    ),
    class = "foresty_model_info"
  )
}

# Whether the fit's own vcov() is already a robust one, so that a model fitted
# with coxph(robust = TRUE) is reported as robust even though foresty did
# nothing to it.
fy_fit_is_robust <- function(fit) {
  if (inherits(fit, "coxph") && !is.null(fit[["naive.var"]])) {
    return(TRUE)
  }
  # A GEE is fitted to give the sandwich estimator in the first place.
  fy_is_gee(fit)
}

# Whether the fit has a likelihood to take a likelihood ratio test over.
#
# A GEE has none: it is estimating equations rather than a likelihood, and its
# standard errors are the sandwich ones from the start. A quasi-likelihood
# family has none either -- logLik() returns NA for it -- since the dispersion
# is estimated rather than assumed.
fy_is_gee <- function(fit) {
  inherits(fit, c("geeglm", "gee", "geese", "geeM", "wgee", "ordgee"))
}

fy_has_no_likelihood <- function(fit) {
  if (fy_is_gee(fit)) {
    return(TRUE)
  }
  fam <- try(stats::family(fit), silent = TRUE)
  if (inherits(fam, "try-error") || is.null(fam$family) || is.na(fam$family)) {
    return(FALSE)
  }
  grepl("^quasi", fam$family)
}

# What to call the thing that has no likelihood, so that the sentence saying
# the Wald test was reported instead says why.
fy_no_likelihood_reason <- function(fit) {
  if (fy_is_gee(fit)) {
    return(paste0(
      "a GEE estimates its coefficients from estimating equations rather than ",
      "from a likelihood, and its standard errors are the sandwich ones from ",
      "the start"
    ))
  }
  paste0(
    "a quasi-likelihood family has no likelihood to take one over, the ",
    "dispersion being estimated rather than assumed"
  )
}

# Pieces of a fit that most fitting functions store in the object itself.
#
# An S4 fit cannot be subsetted at all -- `fit[["xlevels"]]` on an lme4 fit is
# an error rather than a NULL -- so it is never asked, and what it does not
# carry is recovered from its model frame and design matrix instead. That is
# where a base R fit's own copies came from in the first place, so the two
# agree.
fy_fit_element <- function(fit, name) {
  if (isS4(fit)) NULL else fit[[name]]
}

# The levels every factor was fitted with, so that a contrast built from two
# rows of data keeps the whole factor and the design matrix keeps its shape.
fy_xlevels <- function(fit) {
  out <- fy_fit_element(fit, "xlevels")
  if (!is.null(out)) {
    return(out)
  }
  mf <- try(stats::model.frame(fit), silent = TRUE)
  if (inherits(mf, "try-error")) {
    return(NULL)
  }
  factors <- vapply(mf, is.factor, logical(1))
  # Only the variables the fixed effects are built from. A model frame carries
  # more than they do -- the grouping factor of a mixed model among them -- and
  # naming a variable the design does not use makes model.frame() complain that
  # it cannot find it.
  #
  # They are matched on the terms object's own variable labels rather than on
  # all.vars(), because a factor entered as `factor(visit)` is a model frame
  # column of that name and all.vars() would report the bare `visit`. Dropping
  # it left the design matrix to recompute the factor from the two rows of a
  # contrast, where it has one level and no contrasts can be applied.
  terms <- try(stats::terms(fit), silent = TRUE)
  if (!inherits(terms, "try-error")) {
    factors <- factors & names(mf) %in% fy_variable_labels(terms)
  }
  lapply(mf[factors], levels)
}

# The names a terms object's variables take as model frame columns: the term
# labels as written, so `factor(visit)` rather than `visit`. This is how
# model.frame() names them, and so how `xlev` and `contrasts.arg` are matched.
fy_variable_labels <- function(terms) {
  vars <- attr(terms, "variables")
  if (is.null(vars) || length(vars) < 2L) {
    return(character(0))
  }
  vapply(as.list(vars)[-1L], function(v) {
    paste(deparse(v, width.cutoff = 500L), collapse = " ")
  }, character(1))
}

# The contrast coding each factor was fitted under.
fy_contrasts <- function(fit) {
  out <- fy_fit_element(fit, "contrasts")
  if (!is.null(out)) {
    return(out)
  }
  mm <- try(stats::model.matrix(fit), silent = TRUE)
  if (inherits(mm, "try-error")) {
    return(NULL)
  }
  attr(mm, "contrasts")
}

fy_has_intercept <- function(b) {
  length(b) > 0 && names(b)[1] %in% c("(Intercept)", "Intercept")
}

# Counts behind one set of rows of the model frame.
fy_counts <- function(info, rows = NULL, outcome_level = NULL) {
  list(
    n = if (is.null(rows)) info$n else length(rows),
    # A row of a multinomial figure is about one level of the outcome, so what
    # it counts is the people who were in that level rather than the events of
    # a binary outcome it does not have.
    events = if (is.null(outcome_level)) {
      fy_events(info$mf, rows = rows)
    } else {
      fy_level_count(info$mf, outcome_level, rows = rows)
    },
    person_time = fy_person_time(info$mf, rows = rows)
  )
}

fy_level_count <- function(mf, level, rows = NULL) {
  y <- try(stats::model.response(mf), silent = TRUE)
  if (inherits(y, "try-error") || is.null(y) || is.matrix(y)) {
    return(NA_integer_)
  }
  if (!is.null(rows)) {
    y <- y[rows]
  }
  as.integer(sum(as.character(y) == level, na.rm = TRUE))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
