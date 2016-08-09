#' Calculate the area of an image with colour EXCEEDING threshold grey value
#'
#'
#' @param file A jpeg file to analyse
#' @param colour Numeric vector of colour components, c( red, green, blue )
#' @keywords image analysis jpeg
#' @export
#' @return A numeric value, pixel count of matching colour


areaThreshold <- function( file, thresholdGrey = 0.5, which = "high" ) {
    
    # read in the file
    input <- jpeg::readJPEG( file )
    
    input <- mean( input[,,1], input[,,2], input[,,3] )
    
    if( which == "high" ) {
        
        # mark which pixels exceed the threshold grey value
        input <- 1 * ( input[,,1] > thresholdGrey )
    } else if( which == "low" ) {
        
        # or fall below it
        input <- 1 * ( input[,,1] <= thresholdGrey )
    } else {
        stop( "\"which\" argument must be either \"high\" or \"low\"" )
    }
    
    # calculate the area
    return( sum( input ) )
}
