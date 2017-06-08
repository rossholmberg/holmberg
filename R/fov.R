#'
#' calculate the field of view for a given camera setup
#' 
#' @param focal.length numeric vector of focal length values
#' @param distance.to.subject.m numeric vector of subject distances in meters
#' @param focal.mult numeric vector of focal length multipliers (1.0 is for 35mm format, 1.6 is APS-C, etc.)
#' 
#' @import magrittr
#'
#' @export
#'


fov <- function( focal.length,
                 distance.to.subject.m = 40,
                 focal.mult = 1 ) {
    
    . <- NULL
    
    # figure out how many rows we'll need
    rows <- length( focal.length ) * length( distance.to.subject.m ) * length( focal.mult )
    
    # create the blank data frame to be filled
    output <- matrix( data = NA_real_, nrow = rows, ncol = 12L ) %>%
        data.frame()
    
    # name the columns, ready for filling
    names( output ) <- c( "focal.length",
                          "distance.to.subject.m",
                          "focal.mult",
                          "angular.vertical.deg",
                          "angular.horizontal.deg",
                          "angular.diagonal.deg",
                          "dimensional.vertical.m",
                          "dimensional.horizontal.m",
                          "dimensional.diagonal.m",
                          "angular.vertical.rad",
                          "angular.horizontal.rad",
                          "angular.diagonal.rad" )
    
    
    ## expand the focal length and distance to subject values such that we get
    ## one row per value combination
    
    # expand the focal length by simply repeating the vector
    focal.length %<>% rep( length( distance.to.subject.m ) )
    
    # for the distance vector, we'll need to repeat it, but then reorder it,
    # create a sorting vector to use for that
    to.reorder.dist <- rep_len( seq_along( distance.to.subject.m ), length( focal.length ) ) %>%
        order()
    
    # now expand the distance vector, applying the sorting vector appropriately
    distance.to.subject.m %<>% 
        rep_len( length( to.reorder.dist ) ) %>%
        .[ to.reorder.dist ]
    
    ## now repeat the process to expand by the focal multiplier
    focal.length %<>% rep( length( focal.mult ) )
    distance.to.subject.m %<>% rep( length( focal.mult ) )
    to.reorder.f <- rep_len( seq_along( focal.mult ), length( focal.length ) ) %>%
        order()
    focal.mult %<>%
        rep_len( length( to.reorder.f ) ) %>%
        .[ to.reorder.f ]
    
    
    ## now start filling the output data frame
    output['focal.length'] <- focal.length
    output['distance.to.subject.m'] <- distance.to.subject.m
    output['focal.mult'] <- focal.mult
    
    # calculate angular field of view for all focal.length values
    output['angular.vertical.rad'] <- 
        2 * atan( ( 24 / output['focal.mult'] ) / ( 2 * output['focal.length'] ) )
    output['angular.horizontal.rad'] <- 
        2 * atan( ( 36 / output['focal.mult'] ) / ( 2 * output['focal.length'] ) )
    output['angular.diagonal.rad'] <- 
        2 * atan( ( sqrt( 24 ^ 2 + 36 ^ 2 ) / output['focal.mult'] ) / ( 2 * output['focal.length'] ) )
    
    # convert radians to degrees
    output['angular.vertical.deg'] <- output['angular.vertical.rad'] * ( 180 / pi )
    output['angular.horizontal.deg'] <- output['angular.horizontal.rad'] * ( 180 / pi )
    output['angular.diagonal.deg'] <- output['angular.diagonal.rad'] * ( 180 / pi )
    
    # calculate the dimensional field of view based on angles and distances
    output['dimensional.vertical.m'] <- 
        tan( output['angular.vertical.rad'] / 2 ) * output['distance.to.subject.m'] * 2
    output['dimensional.horizontal.m'] <- 
        tan( output['angular.horizontal.rad'] / 2 ) * output['distance.to.subject.m'] * 2
    output['dimensional.diagonal.m'] <- 
        tan( output['angular.diagonal.rad'] / 2 ) * output['distance.to.subject.m'] * 2
    
    # return the filled data frame to the user
    return( output )
    
}

