#' Replace tidycyte metric values with NA values
#' 
#' Replaces all values of a tidycyte metric containing NA values with a replacement value.
#'
#' @param .df A data frame containing tidycyte data.
#' @param .replace A numeric replacement value
#' @param ... <[`data-masking`][dplyr::dplyr_data_masking]> The masked names of the metrics to which replacement is applied.
#' @return A dataframe with adjusted metrics
#' @keywords tidycyte na
#' @export
#' @examples
#' df %>% replace_na_value_in_metric(0,`h2-3`,mAzalea)
replace_na_value_in_metric <- function(.df, .replace, ...) {
  .metrics <- unique(.df$metric)
  .replace <- ifelse(is.na(.replace),as.double(NA),.replace)
  .df %>%
    pivot_wider(names_from = "metric") %>%
    mutate(across(c(...), ~ if_else(is.na(.), .replace, .))) %>%
    pivot_longer(cols = all_of(.metrics), names_to = "metric")
}
