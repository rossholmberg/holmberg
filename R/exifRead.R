#' Extract filename, directory, date, and time from image EXIF data.
#'
#'
#' @param files A vector of files to be analysed.
#' @param coresToUse Either "TRUE" (will use some logic to determine the number of cores to utilise),
#' or an integer value specifying the number of cores to multi-thread tasks to.
#' @import chron
#' @import plyr
#' @import doMC
#' @keywords image exif date time name
#' @export

# files <- list.files( path = "~/Desktop/", pattern = ".jpg", full.names = TRUE )

exifRead <- function( files, coresToUse = TRUE ) {
    
    # set up a basic function to retrieve the exif data
    getTheExifData <- function( file ) { 
        return( 
            read.csv( 
                text = system( paste0( "exiftool -csv '", file, "'" ), intern = TRUE ),
                header = TRUE,
                stringsAsFactors = FALSE,
                fill = TRUE,
                strip.white = TRUE
            ) 
        ) 
    }
    
    
    # see if we should work in parallel. Either take the number given by the user
    if( is.numeric( coresToUse ) || is.integer( coresToUse ) ) {
        doMC::registerDoMC( cores = coresToUse )
        
        # or where the user only specifies "TRUE", check for cores ourselves
    } else if( coresToUse ) {
        doMC::registerDoMC( cores = holmberg::whichComputer()$coresToUse )
    }
    
    
    # run the function on all files
    output <- plyr::ldply( .data = files, 
                           .fun = getTheExifData,
                           .parallel = TRUE )
    
    # and send the output to the user
    return( output )
    
}
