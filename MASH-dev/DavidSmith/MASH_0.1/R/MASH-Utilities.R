#################################################################
#
#   MASH
#   R6-ified
#   Miscellaneous Utilities
#   David Smith, Hector Sanchez, Sean Wu
#   May 21, 2017
#
#################################################################

#' Utility: Convert List to Data Frame
#'
#' Convert a list to a data frame.
#'
#' @param list a list, each element of which will be transformed into a row of a data frame.
#' @return data frame
#' @examples
#' util_List2Df(list = lapply(1:100,MASH:::util_PfSISlice))
#' @export
util_List2Df <- function(list){
  do.call(rbind.data.frame, list)
}
