#' Extracts cell name from tidycyte sample ID
#'
#' Utility function that returns the cell name for each id. 
#'
#' @param id A character string or vector of character strings formatted as an Incucyte sample ID.
#' @return A character string or vector of character strings containing the cell name from each sample ID.
#' @keywords tidycyte id cell
#' @export
#' @examples
#' df$cell <- id_to_cell(df$id)
id_to_cell <- function(id) sub("^([^\\(]+)\\s\\(.*$","\\1",id)
