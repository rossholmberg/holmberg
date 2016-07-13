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
    
    to.return <- vector( mode = "character", length = 0L )
    
    for( i in seq_len( length( input ) ) ) {
        to.act.upon <- input[i]
        # make sure values passed from user are correct classes
        if( !is.character( to.act.upon ) ) {
            to.act.upon <- as.character( to.act.upon )
        }
        if( !is.integer( length.out ) ) {
            length.out <- as.integer( length.out )
        }
        
        # if the to.act.upon needs to be lengthened, do so
        if( nchar( to.act.upon ) < length.out & !is.na( fill ) ) {
            if( !is.character( fill ) ) {
                fill <- as.character( fill )
            }
            fill.length <- length.out - nchar( to.act.upon )
            if( fill.length > nchar( fill ) ) {
                fill <- strrep( fill, ceiling( fill.length / nchar( fill ) ) )
            } else if( fill.length < nchar( fill ) ) {
                fill <- substr( fill, 0L, fill.length )
            }
            
            if( "tail" %in% which ) {
                to.act.upon <- paste0( fill, to.act.upon )
            } else if( "head" %in% which ) {
                to.act.upon <- paste0( to.act.upon, fill )
            }
        }
        
        # check which end was requested, and act accordingly
        if( "tail" %in% which ) {
            output <- substr( to.act.upon,
                              nchar( to.act.upon ) - length.out + 1L,
                              nchar( to.act.upon )
            )
        } else if( "head" %in% which ) {
            output <- substr( to.act.upon, 0L, length.out )
        }
        
        to.return <- c( to.return, output )
        
    }

    return( to.return )
}
