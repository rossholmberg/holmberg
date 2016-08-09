#' calculate and apply a linear levels function to an image
#'
#'
#' @param image An array, imported from an image file.
#' @param clipBlacks Numeric value 0-1. Fraction of image to clip to black.
#' @param clipWhites Numeric value 0-1. Fraction of image to clip to white.
#' 
#' @keywords image modify levels
#' @export

autoLevels <- function( image, clipBlacks = 0.02, clipWhites = 0.02 ) {
    
    for( layer in dim( image )[3] ) {
        
        # find white and black points
        colour <- sort( image[,,layer], decreasing = FALSE )
        whitePoint <- colour[ as.integer( length( colour ) * ( 1 - clipWhites ) ) ]
        blackPoint <- colour[ as.integer( length( colour ) * clipBlacks ) ]
        rm( colour )
        
        # find slope to apply
        slope <- 1 / ( whitePoint - blackPoint )
        image[,,layer] <- ( image[,,layer] - blackPoint ) * slope
        
        # clip whites and blacks
        image[,,layer][ image[,,layer] > 1 ] <- 1
        image[,,layer][ image[,,layer] < 0 ] <- 0
        
    }
    
    return( image )
    
}


