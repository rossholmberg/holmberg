#' Test whether one or more values fall within a specified range.
#'
#'
#' @param x Value or vector of values to test.
#' @param range A vector of length at least 2, from which the range will be computed.
#' @param inclusive Logical value, whether to include the bound of the range.
#' @keywords range within numeric integer
#' @export
#' @return A logical value or vector showing whether each value is within the range.
#' 


withinRange <- function( x, range, inclusive = TRUE ) {
    
    stopifnot(
        !is.na( as.numeric( x ) ),
        length( range ) >= 2L,
        is.logical( inclusive )
    )
    
    if( inclusive ) {
        
        output <- min( range, na.rm = TRUE ) <= x &
            max( range, na.rm = TRUE ) >= x
        
    } else {
        
        output <- min( range, na.rm = TRUE ) < x &
            max( range, na.rm = TRUE ) > x
        
    }
    
    return( output )
    
}
