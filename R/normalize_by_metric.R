#' Normalize tidycyte metrics by other specified metrics
#'
#' Normalizes all metrics based on the arrangement of the tidycyte metadata table. The metadata
#' table is expected to have a column named "\code{metric}" referring to each metric name, and a
#' column named "\code{metric_norm}" that contains on each row the name of another metric for normalization
#' or, if no normalization is desired for that metric, some other value that does not correspond
#' to a metric name (e.g. \code{NA}, \code{NULL}, or empty). 
#'
#' @param .df A data frame containing tidycyte data.
#' @param .metadata A data frame containing tidycyte metadata
#' @return A data frame with normalized metrics
#' @keywords tidycyte normalize
#' @export
#' @examples
#' df %>% normalize_by_metric(metadata)
normalize_by_metric <- function(df, .metadata) {
  df <- pivot_wider(df, names_from = "metric")
  # metric0, normalization key for each metric
  metric0 <- .metadata %>% pull(metric_norm,metric)
  
  # metric_norm_denom, normalization list named by each channel with their corresponding
  # normalization values. if the value corresponds to a metric, then prepare values to
  # normalize the given metric by the its corresponding normalization metric
  metric_norm_denom <- map2(metric0,names(metric0), function(.x,.y,df) {
    if(is.element(.x,colnames(df)))
      df[[.x]]
    else {
      message("normalize_by_metric(): Metric \"",.y,"\" will not be normalized by another metric.")
      1} }, df)

  df %>%
    mutate(across(all_of(names(metric_norm_denom)), ~ . / metric_norm_denom[[cur_column()]])) %>%
    pivot_longer(cols = .metadata[['metric']], names_to = "metric")
}
