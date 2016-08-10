#' calculate and apply an S curve to modify the contrast of an image
#'
#'
#' @param input A file extension to a jpg file, or an array imported from an image file.
#' @param k Value between -1 and 1
#' @import jpeg
#' @keywords image modify curve contrast
#' @export

imageContrast <- function( input, k = 0.3 ) {
    
    # check if `input` is a file needing to be imported
    if( class( input ) == "character" & file.exists( input ) ) {
        
        input <- jpeg::readJPEG( input )
        
    }
    
    # set up the curve function
    sCurve <- function( x, k ) {
        ( k * x - x ) / ( 2 * k * x - k - 1 )
    }
    
    # create an empty array
    output <- array( data = as.numeric( NA ), dim = dim( input ) )
    
    # apply the s curve to each side of the midpoint
    output[ input < 0.5 ] <- sCurve( x = ( 2 * input[ input < 0.5 ] ), k = k ) * 0.5
    output[ input > 0.5 ] <- sCurve( x = 2 * ( input[ input > 0.5 ] - 0.5 ), k = -k ) * 0.5 + 0.5
    
    # fill in any values not covered yet
    output[ input == 0.5 ] <- 0.5
    
    # return the adjusted image
    return( output )
    
}
