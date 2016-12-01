
#' Converting easting and northing values to GPS coordinates
#'
#' @param eastings numeric vector
#' @param northings numeric vector
#' @param input.projection character string specifying projection model of inputs
#' @import sp
#' @import magrittr
#' @export
#' @keywords gps spatial

ENtoGPS <- function( eastings, northings, input.projection = "+init=epsg:28355" ) {
    
    if( length( eastings ) != length( northings ) ) {
        stop( "Eastings and Northings vectors must be of equal length." )
    }
    
    coords <- sp::SpatialPoints( cbind( eastings, northings ), proj4string = sp::CRS( input.projection ) ) %>%
        sp::spTransform( sp::CRS("+proj=longlat") )
    
    coords <- data.frame( lat = coords@coords[,2],
                          lon = coords@coords[,1] )
    
    return( coords )
    
}

## GPStoEN
