#' Convert either latitude or longitude distances between angles (degrees) and distances (km)
#'
#'
#' @param degrees Numeric value, degrees for distance
#' @keywords maps gis
#' @export
#' @return numeric value

latDegToKm <- function( degrees ) { 
    return( 
        degrees * 110.574 
    ) 
}



#' Convert either latitude or longitude distances between angles (degrees) and distances (km)
#'
#'
#' @param km Numeric value, distance in km
#' @keywords maps gis
#' @export
#' @return numeric value

latKmToDeg <- function( km ) { 
    return( 
        km / 110.574 
    ) 
}



#' Convert either latitude or longitude distances between angles (degrees) and distances (km)
#'
#'
#' @param degrees Numeric value, degrees for distance
#' @param lat Numeric value, latitude position
#' @keywords maps gis
#' @export
#' @return numeric value

lonDegToKm <- function( degrees, lat = 39.32633 ) { 
    return( 
        degrees * 111.32 * cos( lat / ( 180 / pi ) ) 
    ) 
}



#' Convert either latitude or longitude distances between angles (degrees) and distances (km)
#'
#'
#' @param lat Numeric value, latitude position
#' @param km Numeric value, distance in km
#' @keywords maps gis
#' @export
#' @return numeric value

lonKmToDeg <- function( km, lat = 39.32633 ) { 
    return( 
        km / ( 111.32 * cos( lat / ( 180 / pi ) ) ) 
    ) 
}



#' Take two latlon coordinates, and return the distance between them.
#'
#'
#' @param lat.1 latitude position, point 1
#' @param lon.1 longitude position, point 1
#' @param lat.2 latitude position, point 2
#' @param lon.2 longitude position, point 2
#' @param unit distance units desired for output, default "km"
#' @keywords maps gis
#' @export
#' @return numeric value, km between the two points

mapDistance <- function( lat.1, lon.1, lat.2, lon.2, unit = "km" ) { 
    
    # run a couple of pre-flight checks
    # if( !{ length( lat.1 ) == 1L || length( lat.2 ) == 1L || length( lat.1 ) == length( lat.2 ) } ) {
    #     stop( "Either coordinates 1 and 2 must be the same length, or one of them must be length 1." )
    # }
    # if( length( lat.1 ) == length( lon.1 ) && length( lat.2 ) == length( lon.2 ) ) {
    #     stop( "Lat vectors must match the length of their corresponding lon vectors." )
    # }
    
    # get the latitudinal distances
    lat.dist <- latDegToKm( abs( lat.1 - lat.2 ) )
    
    # get a latitude to use as a reference point
    if( length( lat.1 ) == 1L && length( lat.2 ) == 1L ) {
        lat.ref <- mean( lat.1, lat.2 )
    } else {
        lat.ref <- ( lat.1 + lat.2 ) / 2
    }
    
    # check to see if the values cross between that 180 to -180 transition
    lon.1.neg <- lon.1 < 0
    lon.2.neg <- lon.2 < 0
    deg.diff <- abs( lon.1 - lon.2 )
    
    # mark which ones we need to correct
    to.correct <- { { lon.1.neg & !lon.2.neg } | { !lon.1.neg & lon.2.neg } } & deg.diff > 180
    
    # those needing correction, adjust the negative value by adding 360
    lon.1[ to.correct & lon.1.neg ] <- lon.1[ to.correct & lon.1.neg ] + 360
    lon.2[ to.correct & lon.2.neg ] <- lon.2[ to.correct & lon.2.neg ] + 360
    
    # clean up
    rm( lon.1.neg, lon.2.neg, deg.diff, to.correct )
    
    # also check for crossing the 0 to 360 transition
    lon.1.gt180 <- lon.1 > 180
    lon.2.gt180 <- lon.2 > 180
    deg.diff <- abs( lon.1 - lon.2 )
    
    to.correct <- { { lon.1.gt180 & !lon.2.gt180 } | { !lon.1.gt180 & lon.2.gt180 } } & deg.diff > 180
    
    # those needing correction, add 360 to the smaller value
    lon.1[ to.correct & !lon.1.gt180 ] <- lon.1[ to.correct & !lon.1.gt180 ] + 360
    lon.2[ to.correct & !lon.2.gt180 ] <- lon.2[ to.correct & !lon.2.gt180 ] + 360
    
    # and the longitudinal distance
    lon.dist <- lonDegToKm( abs( lon.1 - lon.2 ), lat = lat.ref )
    
    # calculate a distance in km
    dist <- sqrt( lat.dist ^ 2 + lon.dist ^ 2 )
    
    # convert units if required
    dist <- switch( unit,
                    "km" = dist, "kilometres" = dist,
                    "m" = dist * 1000, "metres" = dist * 1000,
                    "cm" = dist * 1e5, "centimetres" = dist * 1e5,
                    "miles" = dist / 1.609344, "mi" = dist / 1.609344,
                    "ft" = dist * 3280.844, "feet" = dist * 3280.844,
                    "yards" = dist * 1093.61, "yrds" = dist * 1093.61, "yrd" = dist * 1093.61,
                    "inches" = dist * 39370.1, "inch" = dist * 39370.1, "in" = dist * 39370.1
    )
    
    # return the result
    return( dist )
    
}

