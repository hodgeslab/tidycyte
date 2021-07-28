#' Scale tidycyte metric
#' 
#' Convenience function that uses [derive_metric()] and the [scales::rescale()] function.
#' By default, rescales each individual measured value from 0 to 1 using
#' the range from minmum to maximum value.
#' 
#' Note that this does not scale according to the mean of
#' the observations but to the individual measured values themselves. Hence if using [summary_stats()],
#' the maximum mean value would be expected to be less than 1. If normalization is needed after
#' using [summary_stats()], it can be performed manually.
#' 
#' This function overwrites the original metric. If a new metric name is desired, the
#' original metric should first be copied to a new metric name. See example below.
#'
#' @param .df A data frame containing tidycyte data.
#' @param .name <[`data-masking`][dplyr::dplyr_data_masking]> The masked name of the metric to normalize.
#' @param .to Numeric vector of the min and max final rescaled range. Defaults to \code{c(0,1)}.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Additional arguments to pass to rescale().
#' @return A dataframe with rescaled metrics
#' @keywords tidycyte rescale
#' @export
#' @examples
#' # copy to new name rather than the original name
#' df %>% derive_metric(mAzalea_scaled, mAzalea) %>% 
#'   scale_metric(mAzalea_scaled)
#'
#' # rescales mAzalea metric in place, overwriting original values
#' df %>% scale_metric(mAzalea)
scale_metric <- function(.df, .name, .to = c(0,1), ...) {
  derive_metric_value(.df, {{.name}}, scales::rescale({{.name}}, .to), ...)
}
