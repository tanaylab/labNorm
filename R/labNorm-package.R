#' @keywords internal
"_PACKAGE"

#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_point theme_classic geom_area coord_cartesian scale_fill_manual guides geom_line ylab xlab scale_x_continuous scale_y_continuous theme_linedraw theme element_text facet_grid element_blank element_rect geom_hline
#' @importFrom purrr map
#' @importFrom yesno yesno2
#' @importFrom tibble as_tibble tibble
#' @importFrom glue glue
#' @importFrom forcats fct_rev
#' @importFrom scales extended_breaks percent
#' @importFrom utils head globalVariables download.file
#' @importFrom stats approxfun median quantile
## usethis namespace: start
## usethis namespace: end
NULL

globals <- c("LAB_DETAILS", "age", "Age", "median", "Median", "Percentile", "quantile", "strata_value", "transparent", "value", "yintercept")
utils::globalVariables(globals)
