#' Calculate the area of an image matching a specific colour
#'
#'
#' @param file A jpeg file to analyse
#' @param colour Numeric vector of colour components, c( red, green, blue )
#' @keywords image analysis jpeg
#' @export
#' @return A numeric value, pixel count of matching colour


areaColoured <- function( file, colour = c( 0, 0, 0 ) ) {
    
    # read in the file
    input <- jpeg::readJPEG( file )
    
    # mark which pixels match the colour we're searching for
    input <- 1 * ( input[,,1] == colour[1] & 
                       input[,,2] == colour[2] & 
                       input[,,3] == colour[3] )
    
    # calculate the area
    return( sum( input ) )
}
