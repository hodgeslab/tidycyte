#' Replace tidycyte metric values below threshold
#' 
#' Replaces all values of a tidycyte metric below a specified threshold with a replacement value.
#'
#' @param .df A data frame containing tidycyte data.
#' @param .min A numeric value specifying the minimum Values are replaced if they are below this value, but not if they are equal to this value.
#' @param .replace A numeric replacement value
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> The masked names of the metrics to which replacement is applied.
#' @return A dataframe with adjusted metrics
#' @keywords tidycyte min
#' @export
#' @examples
#' df %>% replace_minimum_value_in_metric(3,0,Confluence,mAzalea,`h2-3`)
replace_minimum_value_in_metric <- function(.df, .min, .replace, ...) {
  .metrics <- unique(.df$metric)
  .replace <- ifelse(is.na(.replace),as.double(NA),.replace)
  .df %>%
    pivot_wider(names_from = "metric") %>%
    mutate(across(c(...), ~ if_else(. < .min, .replace, .))) %>%
    pivot_longer(cols = all_of(.metrics), names_to = "metric")
}
