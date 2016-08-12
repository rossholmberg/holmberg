#' Extract filename, directory, date, and time from image EXIF data.
#'
#'
#' @param files A vector of files to be analysed.
#' @import chron
#' @import plyr
#' @import doMC
#' @keywords image exif date time name
#' @export

# files <- list.files( path = "~/Desktop/", pattern = ".jpg", full.names = TRUE )

exifRead <- function( files, coresToUse = TRUE ) {
    
    # create a function to appropriately read information from exif data
    getTheExifData <- function( file ) {
        
        # apply the exiftool command line tool to get the data
        exif <- system( paste0( "exiftool ", file ), intern = TRUE )
        
        # separate the data, and put it into a data frame
        exif <- strsplit( exif, split = " : " )
        exif <- data.frame( Reduce( rbind, exif ), 
                            row.names = NULL, 
                            stringsAsFactors = F )
        names( exif ) <- c( "label", "contents" )
        
        # retrieve the date and time
        datetime <- unlist( strsplit( 
            exif[ grep( "File Modification", exif$label ), 2 ], 
            split = " " ) )
        date <- as.Date( datetime[1], format = "%Y:%m:%d" )
        time <- chron::times( substr( datetime[2], 0, 8 ) )
        
        # retrieve the file details
        filename <- exif[ grep( "File Name", exif$label ), 2 ]
        directory <- exif[ grep( "Directory", exif$label ), 2 ]
        
        # add the retrieved data to the main data frame
        return( data.frame( filename = filename, 
                              directory = directory, 
                              date = date, 
                              time = time,
                            stringsAsFactors = FALSE )
        )
    }
    
    # see if we should work in parallel. Either take the number given by the user
    if( is.numeric( coresToUse ) || is.integer( coresToUse ) ) {
        doMC::registerDoMC( cores = coresToUse )
        
    # or where the user only specifies "TRUE", check for cores ourselves
    } else if( coresToUse ) {
        doMC::registerDoMC( cores = holmberg::whichComputer()$coresToUse )
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
    
    return( output )
    
}
