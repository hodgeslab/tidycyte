#' Average tidycyte metrics across images
#'
#' Averages all metrics in a tidycyte dataframe across images. Groups images together that have the
#' same values of each parameter except for id, image, and value, each of which can be variable. This
#' effectively groups within each combination of elapsed, date_time, well, cell, treatment, and
#' metric.
#'
#' @param .df A data frame containing tidycyte data.
#' @param .na.rm Boolean value indicating whether \code{NA} values should be removed. Defauls to TRUE.
#' @return A data frame containing the metrics averaged across images. The "\code{image}" field is modified to reflect the total number of images used for averaging, and the "\code{id}" field is modified to note that it is an image mean.
#' @keywords tidycyte average image
#' @export
#' @examples
#' df %>% average_across_images()
average_across_images <- function(.df, .na.rm = TRUE) {
  .df %>%
    group_by(across(-c(id,image,value))) %>%
    summarise(id = sub("^(.*,\\sImage)\\s[0-9]+$",
                       paste0("\\1 mean (N=",length(value[!is.na(value)]),")"),id[1]),
              value = mean(value, na.rm = .na.rm),
              image = length(value[!is.na(value)])) %>%
    ungroup()
}
