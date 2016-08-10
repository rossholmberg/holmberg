#' calculate and apply a linear levels function to an image
#'
#'
#' @param image An array, imported from an image file.
#' @param clipBlacks Numeric value 0-1. Fraction of image area to clip to black.
#' @param clipWhites Numeric value 0-1. Fraction of image area to clip to white.
#' 
#' @keywords image modify curve contrast
#' @export

imageContrast <- function( input, k = 0.3 ) {
    
    sCurve <- function( x, k ) {
        ( k * x - x ) / ( 2 * k * x - k - 1 )
    }
    
    output <- array( data = as.numeric( NA ), dim = dim( input ) )
    
    output[ input < midpointX ] <- sCurve( input = ( 2 * input[ input < 0.5 ] ), k = k ) * 0.5
    output[ input > midpointX ] <- sCurve( input = 2 * ( x[ x > 0.5 ] - 0.5 ), k = -k ) * 0.5 + 0.5
    
    output[ input == 0.5 ] <- 0.5
    
}

x <- seq.int( from = 0, to =  1, by = 0.0001 )
y <- 1 / ( 1 + exp( -5 * ( x - 0.5 ) ) )
plot( x, y, type = "l", xlim = c(0,1), ylim = c(0,1) )

k <- 0
midpointX <- 0.5
midpointY <- 0.5

( k * abs( x ) - abs( x ) ) / ( 2 * k * abs( x ) - k - 1 )

Curve <- function( x, k, midpointX = 0.5, midpointY = 0.5 ) {
    x / ( x + exp( 1 - k * x ) )
}

x <- seq.int( from = -6, to = 6, by = 0.0001 )

x <- c( 0, 0.25, 0.5, 0.75, 1 )
y <- c( 0, 0.1, 0.5, 0.9, 1 )
points$t <- seq_len(5)
model <- poly( y ~ x, 3, raw = TRUE )
points$model <- predict( model, points$t )


plot( points$x, points$model, xlim = c(0,1), ylim = c(0,1) )
plot( x, Curve( x, k = 10 ), type = "l", xlim = c(0,1), ylim = c(0,1) )


k <- 0.2
y <- vector( mode = "numeric", length = length( x ) )
y[ x < midpointX ] <- sCurve( x = ( 2 * x[ x < 0.5 ] ), k = k ) * 0.5
y[ x > midpointX ] <- sCurve( x = 2 * ( x[ x > 0.5 ] - 0.5 ), k = -k ) * 0.5 + 0.5
y[ x == 0.5 ] <- 0.5
plot( x, y, type = "l", xlim = c(0,1), ylim = c(0,1) )

plot( x, 0.5 * ( k * 2 * x - 2 * x ) / ( 2 * k * 2 * x - k - 1 ), 
      type = "l", 
      xlim = c(0,1), ylim = c(0,1) )
