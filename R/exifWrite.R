#' Apply new exif data to image files
#'
#'
#' @param files A vector of files to be modified.
#' @param comment A character string to add to the files' exif data as a comment
#' @param latlon A numeric vector of length 2, c( latitude, longitude )
#' @param coresToUse Either "TRUE" (will use some logic to determine the number of cores to utilise),
#' or an integer value specifying the number of cores to multi-thread tasks to.
#' @keywords image exif add comment
#' @import plyr
#' @import doMC
#' @export

exifWrite <- function( files, comment, latlon, coresToUse = TRUE ) {
    
    # create a function to make the relevant system call
    writeToExif <- function( file, 
                                       commentToApply, 
                                       latlonToApply ) {
        
        # format the latlonToApply argument to suit exiftool
        latitude <- as.character( abs( latlonToApply[1] ) )
        longitude <- as.character( abs( latlonToApply[2] ) )
        latitudeRef <- ifelse( latlonToApply[1] >= 0, "N", "S" )
        longitudeRef <- ifelse( latlonToApply[2] >= 0, "E", "W" )
        
        # make the system call
        system( paste0( 'exiftool -GPSLatitude="', latitude,
                        '" -GPSLongitude="', longitude,
                        '" -GPSLongitudeRef="', longitudeRef,
                        '" -GPSLatitudeRef="', latitudeRef,
                        '" -comment="', commentToApply, 
                        '" -preserve ', file ) )
        # exiftool -GPSLatitude="-34.1023" -GPSLongitude="178.1111" -comment="Just tagged" -preserve whp.jpg
    }
    
    # see if we should work in parallel. Either take the number given by the user
    if( is.numeric( coresToUse ) || is.integer( coresToUse ) ) {
        doMC::registerDoMC( cores = coresToUse )
        
        # or where the user only specifies "TRUE", check for cores ourselves
    } else if( coresToUse ) {
        doMC::registerDoMC( cores = holmberg::whichComputer()$coresToUse )
    }
    
    # apply the function to all listed files
    plyr::l_ply( .data = files,
                 .fun = writeToExif,
                 commentToApply = comment,
                 latlon = latlon,
                 .parallel = TRUE
    )
    
    # note no output is sent to the user, just console messages from exiftool
    
}
