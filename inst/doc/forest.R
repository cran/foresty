## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 3,
  dpi = 120,
  out.width = "100%"
)

## -----------------------------------------------------------------------------
library(foresty)

## -----------------------------------------------------------------------------
# cohort <- transform(
#   foresty_cohort,
#   sex = factor(sex, levels = c("Female", "Male")),
#   maternal_smoking = factor(maternal_smoking)
# )

## -----------------------------------------------------------------------------
str(foresty_cohort, max.level = 1)

## -----------------------------------------------------------------------------
# fit <- glm(
#   asthma ~ no2 + sex + maternal_smoking + maternal_age,
#   family = binomial,
#   data = cohort
# )

## -----------------------------------------------------------------------------
# foresty_app(fit)

## -----------------------------------------------------------------------------
# by_sex <- foresty_interaction(
#   fit,
#   exposure = "no2",
#   interaction = "sex",
#   contrast = 10
# )
# 
# by_sex

## -----------------------------------------------------------------------------
# overall <- foresty_main(list(fit), exposure = "no2", contrast = 10)
# figure <- foresty_combine(Overall = overall, Sex = by_sex, layout = "jama")
# 
# figure

## -----------------------------------------------------------------------------
# fit_severity <- MASS::polr(asthma_severity ~ no2 + sex + maternal_smoking,
#                            data = foresty_cohort, Hess = TRUE)
# 
# foresty_main(list(fit_severity), exposure = "no2", contrast = 10)

## -----------------------------------------------------------------------------
# fit_phenotype <- nnet::multinom(
#   wheeze_phenotype ~ no2 + sex + maternal_smoking,
#   data = foresty_cohort, trace = FALSE
# )
# 
# foresty_main(list(fit_phenotype), exposure = "no2", contrast = 10,
#              outcome_reference_row = TRUE)

## -----------------------------------------------------------------------------
# foresty_interaction(fit_phenotype, exposure = "no2", interaction = "sex",
#                     contrast = 10)

## -----------------------------------------------------------------------------
# foresty_report(by_sex, file = "no2_by_sex.html")

