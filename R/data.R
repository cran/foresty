#' A simulated birth cohort
#'
#' Four thousand simulated children with an air pollution exposure in infancy,
#' asthma by school age, and a time to first wheeze episode. The data are made
#' up, and are here so that the examples run without a real cohort.
#'
#' The exposure effect was simulated to be about twice as large in boys as in
#' girls, and to be the same whether or not the mother smoked, although
#' maternal smoking raises the risk of asthma on its own. The two modifiers
#' therefore show what a real interaction and an absent one look like when the
#' subgroup estimates are drawn beside the joint test.
#'
#' The outcome comes in four shapes, so that the same interaction can be
#' followed through the model classes that carry it: binary (`asthma`),
#' time to event (`wheeze` with `followup_years`), ordinal
#' (`asthma_severity`) and nominal (`wheeze_phenotype`). The ordinal outcome
#' was drawn from a latent logistic variable cut at three fixed thresholds, so
#' it is a proportional odds model by construction; the nominal one was drawn
#' from two multinomial logits whose exposure effects differ, so its levels
#' cannot be collapsed into an ordering.
#'
#' The four are four separate draws, sharing the covariates and the same
#' exposure effect rather than describing one event four ways: each is there
#' to be the outcome of a model, and a child may be an asthma case in one of
#' them and not in another. So `asthma` is not the `asthma_severity` of the
#' same child collapsed to two levels, and cross-tabulating one outcome
#' against another says nothing about anything. Fit them one at a time.
#'
#' @format A data frame with 4,000 rows and 13 columns:
#' \describe{
#'   \item{asthma}{Asthma by school age, 1 or 0.}
#'   \item{asthma_severity}{Asthma severity by school age, an ordered factor:
#'     `None` < `Mild` < `Moderate` < `Severe`. Drawn on its own rather than
#'     from `asthma`.}
#'   \item{wheeze}{The event indicator for `followup_years`: 1 where a first
#'     wheeze episode was seen, 0 where the child was censored without one.}
#'   \item{wheeze_phenotype}{Wheeze phenotype, an unordered factor: `None`,
#'     `Transient` or `Persistent`.}
#'   \item{followup_years}{Years to the wheeze episode or to censoring.}
#'   \item{no2}{Nitrogen dioxide during infancy, in parts per billion.}
#'   \item{black_carbon}{Black carbon during infancy, in micrograms per cubic
#'     metre.}
#'   \item{sex}{Child's sex, `Female` or `Male`.}
#'   \item{maternal_smoking}{Smoking during pregnancy, `No` or `Yes`.}
#'   \item{maternal_asthma}{Maternal history of asthma, `No` or `Yes`.}
#'   \item{maternal_age}{Maternal age at delivery, in years.}
#'   \item{birth_year}{Year of birth, 2005 to 2014, as a factor.}
#'   \item{urbanicity}{`Rural`, `Suburban` or `Urban`. Exposure rises with it,
#'     so it confounds the comparison.}
#' }
#'
#' @source Simulated by `data-raw/foresty_cohort.R`.
"foresty_cohort"
