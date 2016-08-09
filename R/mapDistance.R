#' Convert either latitude or longitude distances between angles (degrees) and distances (km)
#'
#'
#' @param degrees Numeric value, degrees for distance
#' @param lat Numeric value, latitude position
#' @param km Numeric value, distance in km
#' @keywords maps gis
#' @export
#' @return numeric value


latDegToKm <- function( degrees ) { 
    return( 
        degrees * 110.574 
    ) 
}


latKmToDeg <- function( km ) { 
    return( 
        km / 110.574 
    ) 
}


lonDegToKm <- function( degrees, lat = 39.32633 ) { 
    return( 
        degrees * 111.32 * cos( lat / ( 180 / pi ) ) 
    ) 
}


lonKmToDeg <- function( km, lat = 39.32633 ) { 
    return( 
        km / ( 111.32 * cos( lat / ( 180 / pi ) ) ) 
    ) 
}