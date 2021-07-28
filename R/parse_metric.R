#' Parse an individual incucyte metric table and return tidycyte data frame
#'
#' Opens, reads, and parses an individual Incucyte matrix files specified as a row in the metadata table.
#' This function is used by [parse_incucyte_matrices()] and is not anticipated to be used by users.
#'
#' @param .x A character list corresponding to a single row of a tidycyte metadata table. At a minimum should contain names "metric" and "filename".
#' @return A tidycyte dataframe with columns "\code{elapsed}", "\code{date_time}", "\code{id}", "\code{value}", "\code{well}", "\code{image}", "\code{treatment}", "\code{cell}", "\code{passage}", "\code{seeding}", and "\code{metric}".
#' @keywords tidycyte open file
#' @export
#' @examples
#' df <- apply(metadata,1,parse_metric) %>% bind_rows()
parse_metric <- function(.x) {
  read_delim(.x['filename'], delim="\t", skip=1, col_types = cols()) %>%
    rename(date_time = `Date Time`, elapsed = Elapsed) %>%
    relocate(elapsed) %>% # anchor row order on elapsed, which sorts consistently
    pivot_longer(cols=-c(elapsed,date_time), names_to="id", values_to="value") %>%
    mutate(cell = id_to_cell(id),
           passage = id_to_passage(id),
           seeding = id_to_seeding(id),
           treatment = id_to_treatment(id),
           well = id_to_well(id),
           image = id_to_image(id),
           metric = .x['metric'])
}
