#' calculate and apply an S curve to modify the contrast of an image
#'
#'
#' @param input An array, imported from an image file.
#' @param k Value between -1 and 1
#' @param clipWhites Numeric value 0-1. Fraction of image area to clip to white.
#' 
#' @keywords image modify curve contrast
#' @export

imageContrast <- function( input, k = 0.3 ) {
    
    sCurve <- function( x, k ) {
        ( k * x - x ) / ( 2 * k * x - k - 1 )
    }
    
    output <- array( data = as.numeric( NA ), dim = dim( input ) )
    
    output[ input < 0.5 ] <- sCurve( input = ( 2 * input[ input < 0.5 ] ), k = k ) * 0.5
    output[ input > 0.5 ] <- sCurve( input = 2 * ( x[ x > 0.5 ] - 0.5 ), k = -k ) * 0.5 + 0.5
    
    output[ input == 0.5 ] <- 0.5
    
}
