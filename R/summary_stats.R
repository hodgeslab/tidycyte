#' Calculate summary statistics for tidycyte data
#'
#' Gives n, mean, standard deviation, standard error of the mean, and confidence interval.
#'
#' @param .data A data frame containing tidycyte data.
#' @param measurevar <[`data-masking`][dplyr::dplyr_data_masking]> The masked name of the column containing tidycyte values. Typically this will be named \code{value}.
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> The masked names of the grouping variables for summary statistics. Values sharing each of these parameters will be grouped together for calculating statistics. Typically will be \code{elapsed,treatment,cell,metric}.
#' @param .ci Numeric value specifying the confidence interval range as a fraction. Defaults to 0.95.
#' @param .na.rm A boolean that indicates whether to ignore \code{NA} values. Defaults to FALSE.
#' @keywords tidycyte summary standard error statistics
#' @export
#' @examples
#' df %>% summary_stats(value,elapsed,treatment,cell,metric,.ci = 0.95)
summary_stats <- function(.data, measurevar, ..., .ci = 0.95, .na.rm = FALSE) {
  options(dplyr.summarise.inform = F)
  .data %>%
    group_by(...) %>%
    summarise(n = n(),
              mean = mean({{measurevar}}, na.rm = .na.rm),
              sd = sd({{measurevar}}, na.rm = .na.rm),
              se = sd/sqrt(n),
              ci = se * qt(.ci/2 + 0.5, n-1)) %>%
    ungroup()
}
