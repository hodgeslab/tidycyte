#' Replace tidycyte metric values above threshold
#' 
#' Replaces all values of a tidycyte metric above a specified threshold with a replacement value.
#'
#' @param .df A data frame containing tidycyte data.
#' @param .max A numeric value specifying the maximum. Values are replaced if they are over this value, but not if they are equal to this value.
#' @param .replace A numeric replacement value
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> The masked names of the metrics to which replacement is applied.
#' @return A dataframe with adjusted metrics
#' @keywords tidycyte max
#' @export
#' @examples
#' df %>% replace_maximum_value_in_metric(50,50,Confluence,mAzalea,`h2-3`)
replace_maximum_value_in_metric <- function(.df, .max, .replace, ...) {
  .metrics <- unique(.df$metric)
  .replace <- ifelse(is.na(.replace),as.double(NA),.replace)
  .df %>%
    pivot_wider(names_from = "metric") %>%
    mutate(across(c(...), ~ if_else(. > .max, .replace, .))) %>%
    pivot_longer(cols = all_of(.metrics), names_to = "metric")
}
