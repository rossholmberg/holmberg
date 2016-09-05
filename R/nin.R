#' Invert the common function %in%
#'
#'
#' @param x Value or vector of values to test.
#' @param table A vector of values to check against.
#' @keywords notin
#' @export
#' @return A logical value or vector showing whether each value is NOT represented in the test table.
#' 

`%nin%` <- Negate( `%in%` )
