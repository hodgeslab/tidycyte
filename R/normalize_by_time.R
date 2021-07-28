#' Normalize tidycyte metrics by specified time points
#'
#' Normalizes all metrics based on the arrangement of the tidycyte metadata table. The metadata
#' table is expected to have a column named "\code{metric}" referring to each metric name, and a
#' column named "\code{time_norm}" that contains on each row the elapsed time to be used for normalization
#' or, if no normalization is desired for that metric, some other value that does not correspond
#' to an elapsed time value (e.g. \code{NA}, \code{NULL}, or empty).
#'
#' @param .df A data frame containing tidycyte data.
#' @param .metadata A data frame containing tidycyte metadata
#' @return A data frame with normalized metrics
#' @keywords tidycyte normalize
#' @export
#' @examples
#' df %>% normalize_by_time(metadata)
normalize_by_time <- function(df, .metadata) {
  # t0, time normalization key for each metric
  t0 <- .metadata %>% pull(time_norm, metric)
  # print a message if these values are not in elapsed
  map2(t0,names(t0),
       function(.x,.y,df) {
         if(!is.element(.x,df[['elapsed']]))
           message("normalize_by_time(): Metric \"",.y,"\" will not be normalized by a time point.")
       },df)
  df %>%
    group_by(across(-c(date_time,elapsed,id,value))) %>%
    nest() %>%
    mutate(data = map(data, timepoint_norm_group, t0[[metric]])) %>%
    unnest(cols = c(data)) %>%
    ungroup()
}
