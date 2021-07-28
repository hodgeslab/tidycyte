#' Extracts passage number from tidycyte sample ID
#'
#' Utility function that returns the passage number for each id. 
#'
#' @param id A character string or vector of character strings formatted as an Incucyte sample ID.
#' @return A character string or vector of character strings containing the passage number from each sample ID.
#' @keywords tidycyte id passage
#' @export
#' @examples
#' df$passage <- id_to_passage(df$id)
id_to_passage <- function(id) sub("^[^\\(]*\\(([0-9]+)\\).*$","\\1", id)
