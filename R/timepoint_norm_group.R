#' Normalize an individual tidycyte metric data frame by time
#'
#' Normalizes an individual tidycyte data frame based on a specified timem value.
#' This function is used by [normalize_by_time()] to normalize pre-grouped data, and is not
#' anticipated to be used by users.
#'
#' @param df A data frame of an individual tidycyte dataset. At a minimum should contain column names "value" and "elapsed".
#' @param t0 A numeric value specifying an individual elapsed time.
#' @return A dataframe with the value column normalized by the value where elapsed equals t0.
#' @keywords tidycyte normalize timepoint
#' @export
#' @examples
#' df %>%
#'   group_by(across(-c(date_time,elapsed,id,value))) %>%
#'   nest() %>%
#'   mutate(data = map(data, timepoint_norm_group, t0[[metric]])) %>%
#'   unnest(cols = c(data)) %>%
#'   ungroup()
timepoint_norm_group <- function(df,t0) {
  if(is.element(t0, df[['elapsed']])) {
    v0 <- df[['value']][df[['elapsed']] == t0]
    df[['value']] <- df[['value']] / v0
  }
  df
}
