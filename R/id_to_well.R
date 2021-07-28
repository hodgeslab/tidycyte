#' Extracts well from tidycyte sample ID
#'
#' Utility function that returns the well number for each id. 
#'
#' @param id A character string or vector of character strings formatted as an Incucyte sample ID.
#' @return A character string or vector of character strings containing the well from each sample ID.
#' @keywords tidycyte id well
#' @export
#' @examples
#' df$well <- id_to_well(df$id)
id_to_well <- function(id) sub("^.*\\(([^\\)]*)\\),\\sImage\\s[0-9]+$","\\1",id)
