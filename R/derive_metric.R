#' Derive tidycyte metric using arithmetic or function
#'
#' Permits the use of arithmetic or other R functions for each metric. Can accept other metrics in arguments.
#'
#' @param .df A data frame containing tidycyte data.
#' @param .name <[`data-masking`][dplyr::dplyr_data_masking]> The masked name of the metric to be derived (can overwrite existing metrics)
#' @param .formula <[`data-masking`][dplyr::dplyr_data_masking]> A formula containing masked metric names. May use arithmetic or other R functions, and can make use of other metrics.
#' @return An updated data frame containing the new or revised metric.
#' @keywords tidycyte function metric
#' @export
#' @examples
#' df %>% derive_metric(Yellow, sqrt(`h2-3`) / mAzalea^2 - 3) # mathematical operations and offsets
#' df %>% derive_metric(mAzalea, mAzalea - mean(mAzalea)) # can use other functions
#' df %>% derive_metric(mAzalea, mAzalea - as.numeric(quantile(mAzalea,0.05))) # minus bottom 5%ile
#' df %>% derive_metric(mAzalea_scaled, mAzalea) %>% # copy to new name rather than the original name
#'   scale_metric(mAzalea_scaled) # rescale from 0 to 1 by default, returns the same name
derive_metric <- function(.df, .name, .formula) {
  .metrics = unique(c(.df$metric,deparse(substitute(.name))))
  .df %>%
    pivot_wider(names_from = "metric") %>%
    mutate({{.name}} := {{.formula}}) %>%
    pivot_longer(cols = all_of(.metrics), names_to = "metric")
}
