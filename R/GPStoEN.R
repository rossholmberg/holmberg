
#' Converting GPS coordinates to Eastings and Northings
#'
#' @param lats numeric vector
#' @param lons numeric vector
#' @param output.projection character string specifying projection model of outputs
#' @import sp
#' @import magrittr
#' @export
#' @keywords gps spatial

GPStoEN <- function( lats, lons, output.projection = "+init=epsg:28355" ) {
    
    if( length( lats ) != length( lons ) ) {
        stop( "lats and lons vectors must be of equal length." )
    }
    
    xy <- data.frame( id = seq_along( lats ),
                      lon = lons,
                      lat = lats )
    coordinates( xy ) <- c( "lon", "lat" )
    proj4string( xy ) <- CRS( "+proj=longlat" )
    
    en <- spTransform( xy, CRS( output.projection ) ) %>%
        as( "SpatialPoints" )
    
    coords <- data.frame( X = en@coords[,1],
                          Y = en@coords[,2] )
    
    return( coords )
    
}
