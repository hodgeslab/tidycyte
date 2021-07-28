#' Calculate fractions from a set of tidycyte metrics
#'
#' Accepts as arguments multiple sets of metrics and then calculates the proportion that each metric
#' represents out of the total sum. Effectively assumes that each metric is an object count, and
#' returns the proportion of objects from each set.
#' 
#' Note that in practice, this assumes that each
#' count is mutually exclusive. For example, if you count green, red, and yellow (green and red)
#' objects, then ensure that the yellow objects do not contain green-only or red-only objects.
#' Depending on the type of data, you may (or may not) be able to subtract these off as needed.
#'
#' @param .df A data frame containing tidycyte data.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> The masked names of the metrics to use for calculating each fraction.
#' @param .prefix An optional character string containing a prefix if you wish to assign the fraction to new metric names.
#' @param .suffix An optional character string containing a suffix if you wish to assign the fraction to new metric names.
#' @return A data frame containing updated or new metrics containing fractions.
#' @keywords tidycyte fraction
#' @export
#' @examples
#' df %>% metrics_to_fractions(Confluence,mAzalea,`h2-3`) # updates metrics in place
#' df %>% metrics_to_fractions(Confluence,mAzalea,`h2-3`, .prefix="fraction_", .suffix="_of_all")
#' df %>% metrics_to_fractions(mAzalea,`h2-3`, .prefix="fraction_", .suffix="_of_fluorescent")
metrics_to_fractions <- function(.df, ..., .prefix="", .suffix="") {
  .dots = match.call(expand.dots = FALSE)[["..."]]
  .metrics = unique(c(.df$metric,paste0(.prefix,lapply(.dots, deparse),.suffix))) # existing and new
  .df %>%
    pivot_wider(names_from = "metric") %>%
    mutate(across(c(...), ~ . / rowSums(across(c(...))), .names = "{.prefix}{col}{.suffix}")) %>%
    pivot_longer(cols = all_of(.metrics), names_to = "metric")
}
