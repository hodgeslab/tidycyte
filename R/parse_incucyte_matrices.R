#' Read and parse Incucyte matrices
#'
#' Opens, reads, and parses all Incucyte matrix files specified in the metadata table.
#'
#' @param metadata A data frame containing tidycyte metadata. At a minimum should contain column names "\code{metric}" and "\code{filename}".
#' @return A concatenated tidycyte dataframe with columns \code{elapsed}, \code{date_time}, \code{id}, \code{value}, \code{well}, \code{image}, \code{treatment}, \code{cell}, and \code{metric}.
#' @keywords tidycyte open files
#' @export
#' @examples
#' df <- parse_incucyte_matrices(metadata)
parse_incucyte_matrices <- function(metadata) {
  df <- apply(metadata,1,parse_metric) %>% bind_rows()
}
