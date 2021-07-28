#' Extracts image number from tidycyte sample ID
#'
#' Utility function that returns the image number for each id. 
#'
#' @param id A character string or vector of character strings formatted as an Incucyte sample ID.
#' @return A character string or vector of character strings containing the image number from each sample ID.
#' @keywords tidycyte id image
#' @export
#' @examples
#' df$image <- id_to_image(df$id)
id_to_image <- function(id) sub("^.*\\),\\sImage\\s([0-9]+)$","\\1",id)
