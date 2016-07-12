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


stringLength <- function( input,
                          length.out,
                          which = c( "head", "tail" ),
                          fill = NA ) {
    
    # make sure values passed from user are correct classes
    if( !is.character( input ) ) {
        input <- as.character( input )
    }
    if( !is.integer( length.out ) ) {
        length.out <- as.integer( length.out )
    }
    
    # if the input needs to be lengthened, do so
    if( nchar( input ) < length.out & !is.na( fill ) ) {
        if( !is.character( fill ) ) {
            fill <- as.character( fill )
        }
        fill.length <- length.out - nchar( input )
        if( fill.length > nchar( fill ) ) {
            fill <- strrep( fill, ceiling( fill.length / nchar( fill ) ) )
        } else if( fill.length < nchar( fill ) ) {
            fill <- substr( fill, 0L, fill.length )
        }
        
        if( "tail" %in% which ) {
            input <- paste0( fill, input )
        } else if( "head" %in% which ) {
            input <- paste0( input, fill )
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
    } else { stop( "The value of which must be either `head` or `tail`." ) }
    
    return( output )
}
