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
#' @param lat.1 longitude position, point 1
#' @param lat.1 latitude position, point 2
#' @param lat.1 longitude position, point 2
#' @keywords maps gis
#' @export
#' @return numeric value, km between the two points

mapDistance <- function( lat.1, lon.1, lat.2, lon.2, unit = "km" ) { 
    
    # get the latitudinal distance
    lat.dist <- latDegToKm( abs( lat.1 - lat.2 ) )
    
    # and the longitudinal distance
    lon.dist <- lonDegToKm( abs( lon.1 - lon.2 ), lat = mean( lat.1, lat.2 ) )
    
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

