#' Extracts seeding conditions from tidycyte sample ID
#'
#' Utility function that returns seeding conditions for each id. 
#'
#' @param id A character string or vector of character strings formatted as an Incucyte sample ID.
#' @return A character string or vector of character strings containing the seeding conditions from each sample ID.
#' @keywords tidycyte id passage
#' @export
#' @examples
#' df$seeding <- id_to_seeding(df$id)
id_to_seeding <- function(id) sub("^[^\\(]*\\([0-9]+\\)\\s(\\S+\\s/\\s\\S+).*$","\\1", id)
