# Linear combinations of coefficients, and joint tests of several of them.
#
# These two functions are the only ones in the package that call `car`, and
# they do so through car::linearHypothesis.default() with `coef.`, `vcov.` and
# `error.df` supplied explicitly. That matters:
#
#   * The method a fit dispatches to has its own idea of which coefficients,
#     which covariance and how many degrees of freedom to use, and would quietly
#     use the model's own where a robust or cluster-robust variance was asked
#     for. Calling the default method and handing it all three means the test
#     beside a figure is taken over the same numbers the intervals were.
#   * The hypothesis is always passed as a numeric contrast matrix and never as
#     a character formula such as "x + x:sexMale". A coefficient may be named
#     anything a fitting function likes -- brackets, colons, spaces, an equals
#     sign -- and car's formula parser reads only some of those, whereas the
#     matrix form sidesteps coefficient names entirely.
#
# The estimate and its variance are read from the attributes that car attaches
# to its result, which is how biostat3::lincom() obtains the same quantities.

# Estimates one linear combination per row of L.
#
# L has one column per retained coefficient, in the order of info$coef, and
# returns a data frame with one row per row of L.
fy_lincom <- function(info, L, ci_level = 0.95, exponentiate = info$exponentiate) {
  L <- as.matrix(L)
  stopifnot(ncol(L) == length(info$coef))

  test <- if (is.finite(info$error_df)) "F" else "Chisq"
  out <- lapply(seq_len(nrow(L)), function(i) {
    fy_lincom_row(info, L[i, , drop = FALSE], test)
  })
  out <- do.call(rbind, out)

  # The interval is always formed on the scale the model was fitted on and
  # only then transformed, so that an exponentiated interval stays consistent
  # with its point estimate.
  alpha <- (1 - ci_level) / 2
  crit <- if (is.finite(info$error_df)) {
    stats::qt(1 - alpha, df = info$error_df)
  } else {
    stats::qnorm(1 - alpha)
  }
  out$conf.low <- out$estimate - crit * out$se
  out$conf.high <- out$estimate + crit * out$se

  # A signed Wald statistic -- a z, or a t where the error degrees of freedom
  # are finite -- rather than the squared one car reports. It is the statistic
  # a model summary shows, and it says which way the estimate went, which a
  # squared one does not.
  out$statistic <- out$estimate / out$se

  if (exponentiate) {
    out$estimate <- exp(out$estimate)
    out$conf.low <- exp(out$conf.low)
    out$conf.high <- exp(out$conf.high)
  }
  out$term_label <- rownames(L)
  rownames(out) <- NULL
  out[, c("term_label", "estimate", "se", "conf.low", "conf.high",
          "statistic", "p.value")]
}

fy_lincom_row <- function(info, l, test) {
  lh <- car::linearHypothesis.default(
    info$fit,
    l,
    coef. = info$coef,
    vcov. = info$vcov,
    error.df = info$error_df,
    test = test,
    singular.ok = TRUE
  )
  data.frame(
    estimate = as.vector(attr(lh, "value")),
    se = sqrt(as.vector(attr(lh, "vcov"))),
    p.value = fy_lh_p(lh),
    stringsAsFactors = FALSE
  )
}

# Jointly tests every coefficient named in `columns` against zero. With one
# column this is the ordinary Wald test; with several it is the test that
# answers "is there any interaction at all", which is the p-value usually
# reported beside a subgroup analysis.
fy_joint_test <- function(info, columns) {
  columns <- intersect(columns, names(info$coef))
  if (!length(columns)) {
    return(list(statistic = NA_real_, df = NA_real_, p.value = NA_real_,
                test = NA_character_))
  }
  L <- matrix(0, nrow = length(columns), ncol = length(info$coef),
              dimnames = list(columns, names(info$coef)))
  L[cbind(seq_along(columns), match(columns, names(info$coef)))] <- 1

  test <- if (is.finite(info$error_df)) "F" else "Chisq"
  lh <- car::linearHypothesis.default(
    info$fit,
    L,
    coef. = info$coef,
    vcov. = info$vcov,
    error.df = info$error_df,
    test = test,
    singular.ok = TRUE
  )
  list(
    statistic = fy_lh_statistic(lh),
    df = as.numeric(lh[["Df"]][2L]),
    p.value = fy_lh_p(lh),
    test = if (test == "F") "F" else "Wald chi-square",
    terms = columns
  )
}

# The same hypothesis tested against the likelihood rather than against the
# Wald approximation to it: the model carrying the interaction is compared with
# the same model without it, and twice the difference in log-likelihood is
# referred to a chi-square on the interaction's degrees of freedom.
#
# The two tests answer the same question and usually agree. They part company
# where the Wald approximation is poor -- a small study, a rare outcome, a
# sparse subgroup, an estimate far from the null -- and the likelihood ratio
# test is then the more trustworthy of the two. It has no robust form: it is
# computed from the likelihood, so a sandwich or cluster-robust variance has no
# part in it, and the Wald test is what to report when the standard errors are
# robust.
fy_lrt_test <- function(info, reduced, columns) {
  full <- fy_log_lik(fy_for_likelihood(info$fit),
                     "the model carrying the interaction")
  null <- fy_log_lik(fy_for_likelihood(reduced),
                     "the model without the interaction")

  n_full <- fy_nobs_or_na(info$fit)
  n_null <- fy_nobs_or_na(reduced)
  if (!is.na(n_full) && !is.na(n_null) && n_full != n_null) {
    stop(
      "the model with the interaction was fitted to ", n_full,
      " observations and the model without it to ", n_null,
      ", so the two are not comparable and a likelihood ratio test cannot be ",
      "taken between them. That happens when a variable in the interaction is ",
      "missing on some rows; drop those rows before fitting, or use ",
      "`test = \"wald\"`.",
      call. = FALSE
    )
  }

  df <- full$df - null$df
  if (is.na(df) || df <= 0) {
    df <- length(columns)
  }
  statistic <- 2 * (full$value - null$value)
  if (!is.na(statistic) && statistic < 0) {
    warning(
      "the model without the interaction reports a higher likelihood than the ",
      "model with it, which cannot happen when both have been fitted to the ",
      "same data and one is nested in the other; it usually means one of the ",
      "two did not converge. The likelihood ratio test is not reported",
      call. = FALSE
    )
    return(list(statistic = NA_real_, df = df, p.value = NA_real_,
                test = "Likelihood ratio chi-square", terms = columns))
  }

  list(
    statistic = statistic,
    df = as.numeric(df),
    p.value = stats::pchisq(statistic, df = df, lower.tail = FALSE),
    test = "Likelihood ratio chi-square",
    terms = columns
  )
}

# The fit whose likelihood the test is taken over.
#
# A linear mixed model is fitted by REML by default, and a REML likelihood
# belongs to the fixed-effect structure it was computed under: two models with
# different fixed effects have different REML likelihoods of different things,
# and the difference between them is not a test of anything. lme4's own anova()
# refits with maximum likelihood for exactly this reason, and so does this.
fy_for_likelihood <- function(fit) {
  if (!inherits(fit, "merMod") || !requireNamespace("lme4", quietly = TRUE)) {
    return(fit)
  }
  if (!isTRUE(lme4::isREML(fit))) {
    return(fit)
  }
  refitted <- try(lme4::refitML(fit), silent = TRUE)
  # Handing back the REML fit instead would take the test over a likelihood
  # that is not comparable between the two models, and it would look like an
  # ordinary result. There is no test to be had here, so none is given.
  if (inherits(refitted, "try-error")) {
    stop(
      "this mixed model was fitted by REML and could not be refitted by ",
      "maximum likelihood, so its likelihood is not comparable with that of ",
      "the model without the interaction and a likelihood ratio test cannot ",
      "be taken between them: ",
      conditionMessage(attr(refitted, "condition")),
      "\nUse `test = \"wald\"`.",
      call. = FALSE
    )
  }
  refitted
}

# The log-likelihood of a fit and the parameters it spent, or an error naming
# the class that has none. A quasi-likelihood fit reports NA rather than
# failing, and a GEE has no likelihood at all.
fy_log_lik <- function(fit, what) {
  ll <- try(stats::logLik(fit), silent = TRUE)
  value <- if (inherits(ll, "try-error")) NA_real_ else as.numeric(ll)[1L]
  if (is.na(value)) {
    stop(
      what, ", of class ", paste(class(fit), collapse = "/"),
      ", reports no likelihood, so a likelihood ratio test cannot be taken ",
      "over it. A quasi-likelihood family and a GEE are fitted without one. ",
      "Use `test = \"wald\"`.",
      call. = FALSE
    )
  }
  df <- attr(ll, "df")
  list(value = value, df = if (is.null(df)) NA_real_ else as.numeric(df))
}

fy_nobs_or_na <- function(fit) {
  out <- try(stats::nobs(fit), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out) || length(out) != 1L) {
    return(NA_real_)
  }
  as.numeric(out)
}

# car names the test column after the statistic it chose, which differs between
# the Chisq and F forms and between model classes, so both are looked up rather
# than assumed.
fy_lh_statistic <- function(lh) {
  column <- intersect(c("Chisq", "F"), colnames(lh))
  if (!length(column)) return(NA_real_)
  as.numeric(lh[[column[1L]]][2L])
}

fy_lh_p <- function(lh) {
  column <- grep("^Pr\\(", colnames(lh), value = TRUE)
  if (!length(column)) return(NA_real_)
  as.numeric(lh[[column[1L]]][2L])
}
