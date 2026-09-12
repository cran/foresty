#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom ggplot2 .data
#' @importFrom grid grid.draw
#' @importFrom stats coef vcov formula nobs predict model.frame
## usethis namespace: end
NULL

utils::globalVariables(c(
  "estimate", "conf.low", "conf.high", "column", "value", "label",
  "modifier_level", "variable_label", "reference", "row", "position", "group"
))
