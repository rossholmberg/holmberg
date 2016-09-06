#' Shift values of a vector by a given amount.
#'
#'
#' @param input A vector to be shifted.
#' @param n An integer value, how far to shift.
#' @param fill A value to fill in empty cells created during the shift operation.
#' @keywords vector, shift, lag, lead
#' @export
#' @return A vector the same length as the input vector, but with values shifted.

shiftLag <- function( input, n = 1L, fill = NA ) {
    output <- c( 
        input[ seq.int( length( input ) - n + 1L, length( input ), 1L ) ],
        input[ seq_len( length( input ) - n ) ]
    )
    output[ seq_len( n ) ] <- fill
    return( output )
}

shiftLead <- function( input, n = 1L, fill = NA ) {
    output <- c(
        input[ seq.int( n + 1L, length( input ), 1L ) ],
        input[ seq_len( n ) ]
    )
    output[ seq.int( length( input ) - n + 1L, length( input ), 1L ) ] <- fill
    return( output )
}
