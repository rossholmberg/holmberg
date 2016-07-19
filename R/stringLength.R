#' Change the length of a character string, placing the input string at
#' one end of the output string (if lengthening), or taking one end of
#' the input string only (when shortening). Useful to match lengths of
#' character strings, where inputs vary in length.
#'
#'
#' @param input A character vector (coerced to character if not already)
#' @param length.out Integer or numerical value for output character length
#' @param which Options "head" (start of string) or "tail" (end of string)
#' @param fill Character vector of any length, used to fill in missing
#' characters where input needs to be lengthened. NA (default) will not
#' lengthen, meaning output strings may be shorter than "length.out".
#' @keywords character string length
#' @export
#' @return A vector of character strings, the same vector length as `input`.


stringLength <- function( input,
                          length.out,
                          which = c( "head", "tail" ),
                          fill = NA ) {
    
    in.length <- nchar( input )
    to.add <- length.out - in.length
    if( !is.na( fill ) ) {
        to.rep <- ceiling( abs( to.add / nchar( fill ) ) )
        tomerge <- strrep( fill, to.rep )
        if( "tail" %in% which ) {
            input <- paste0( tomerge, input )
        } else if( "head" %in% which ) {
            input <- paste0( input, tomerge )
        }
    }
    
    # check which end was requested, and act accordingly
    if( "tail" %in% which ) {
        output <- substr( input,
                          nchar( input ) - length.out + 1L,
                          nchar( input )
        )
    } else if( "head" %in% which ) {
        output <- substr( input, 0L, length.out )
    }
    
    
    
    return( output )
}
