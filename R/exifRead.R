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
    
    # create a function to appropriately read information from exif data
    getTheExifData <- function( file ) {
        
        # apply the exiftool command line tool to get the data
        exif <- system( paste0( "exiftool -s '", file, "'" ), intern = TRUE )
        
        # separate the data, and put it into a data frame
        exif <- strsplit( exif, split = " : " )
        exif <- data.frame( Reduce( rbind, exif ), 
                            row.names = NULL, 
                            stringsAsFactors = F )
        names( exif ) <- c( "label", "contents" )
        
        # strip white space
        exif$label <- holmberg::stripWhiteSpace( exif$label, which = "both" )
        exif$contents <- holmberg::stripWhiteSpace( exif$contents, which = "both" )
        
        # retrieve the date and time
        # first make them NA (in case they can't be found in the exif)
        date <- as.Date( NA )
        time <- chron::times( NA )
        # then see if they're in the exif
        if( "FileModifyDate" %in% exif$label ) {
            # and make them look nice if so (overwriting the NAs we just made)
            datetime <- unlist( strsplit( 
                exif[ grep( "FileModifyDate", exif$label ), 2 ], 
                split = " " ) )
            date <- as.Date( datetime[1], format = "%Y:%m:%d" )
            time <- chron::times( substr( datetime[2], 0, 8 ) )
        } else {
            # send a warning if the datetime couldn't be found
            warning( "No datetime found, outputting as NA" )
        }
        
        
        # retrieve the file details. Starting with NA values
        filename <- directory <- as.character( NA )
        
        if( "FileName" %in% exif&label ) {
            filename <- exif[ grep( "FileName", exif$label ), 2 ]
        } else {
            warning( "No filename found, outputting as NA" )
        }
        
        if( "Directory" %in% exif&label ) {
            directory <- exif[ grep( "Directory", exif$label ), 2 ]
        } else {
            warning( "No directory found, outputting as NA" )
        }
        
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
    
    
    # run the function on all files
    output <- plyr::ldply( .data = files, 
                           .fun = getTheExifData,
                           .parallel = TRUE )
    
    # and send the output to the user
    return( output )
    
}
