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
    
    # check the character length we're working with
    in.length <- nchar( input )
    
    # work out how many characters we need to add
    to.add <- length.out - in.length
    
    # if the `fill` argument was TRUE
    if( !is.na( fill ) ) {
        # work out how many times we need to repeat the fill characters
        to.rep <- ceiling( abs( to.add / nchar( fill ) ) )
        # create a vector of strings to be merged
        tomerge <- strrep( fill, to.rep )
        
        # and add those strings to the input (in the right order)
        if( "tail" %in% which ) {
            input <- paste0( tomerge, input )
        } else if( "head" %in% which ) {
            input <- paste0( input, tomerge )
        }
    }
    
    # now that we've lengthened where requested (some may be too long)
    # check which end was requested, and shorten accordingly
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
