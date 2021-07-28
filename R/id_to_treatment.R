#' Extracts treatment from tidycyte sample ID
#'
#' Utility function that returns the treatment for each id. 
#'
#' @param id A character string or vector of character strings formatted as an Incucyte sample ID.
#' @return A character string or vector of character strings containing the treatment from each sample ID.
#' @keywords tidycyte id treatment
#' @export
#' @examples
#' df$treatment <- id_to_treatment(df$id)
id_to_treatment <- function(id) sub("^.*\\s/\\swell\\s([^\\(]+)\\s\\(.*$","\\1",id)
