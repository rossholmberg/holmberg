#' A function for use when adding a data row to a given data frame
#' 
#' @param x a vector of values
#' @param expand.lims how much to expand numeric value or length 2 vector
#' @param shift optional shift parameter
#' @param na.rm as per base range function
#' 
#' @export
#' 
#'

range_expand <- function( x, expand.lims = 0, shift = 0, na.rm = TRUE ) {
    
    if( !is.numeric( x ) ) {
        stop( "x must be numeric" )
    }
    if( !is.numeric( expand.lims ) && !is.na( expand.lims ) && !is.null( expand.lims ) ) {
        stop( "expand.lims must be numeric" )
    }
    if( !is.numeric( shift ) && !is.na( shift ) && !is.null( shift ) ) {
        stop( "shift must be numeric" )
    }
    
    out <- range( x, na.rm = na.rm )
    
    if( length( expand.lims ) == 2L ) {
        out[1] <- out[1] - expand.lims[1]
        out[2] <- out[2] + expand.lims[2]
    } else if( is.na( expand.lims ) || is.null( expand.lims ) ) {
        
    } else if( length( expand.lims ) == 1L ) {
        out[1] <- out[1] - expand.lims
        out[2] <- out[2] + expand.lims
    } else {
        stop( "expand.lims must be either length 1 or 2" )
    }
    
    if( !is.na( shift ) && !is.null( shift ) && is.numeric( shift ) ) {
        out <- out + shift
    }
    
    
    return( out )
    
}

