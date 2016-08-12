#' Extract filename, directory, date, and time from image EXIF data.
#'
#'
#' @param files A vector of files to be analysed.
#' @import chron
#' @keywords image exif date time name
#' @export

# files <- list.files( path = "~/Desktop/", pattern = ".jpg", full.names = TRUE )

exifRead <- function( files ) {
    
    # create an empty data frame to be filled
    output <- data.frame( filename = vector( mode = "character", length = length( files ) ),
                          directory = vector( mode = "character", length = length( files ) ),
                          date = vector( mode = "character", length = length( files ) ),
                          time = vector( mode = "character", length = length( files ) ),
                          stringsAsFactors = FALSE )
    
    for( i in seq_along( files ) ) {
        
        # apply the exiftool command line tool to get the data
        exif <- system( paste0( "exiftool ", files[i] ), intern = TRUE )
        
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
        date <- datetime[1]
        time <- substr( datetime[2], 0, 8 )
        
        # retrieve the file details
        filename <- exif[ grep( "File Name", exif$label ), 2 ]
        directory <- exif[ grep( "Directory", exif$label ), 2 ]
        
        # add the retrieved data to the main data frame
        output[i,] <- c( filename, directory, date, time )
        
    }
    
    # convert the dates and times to nicer formats
    output$date <- as.Date( output$date, format = "%Y:%m:%d" )
    output$time <- chron::times( output$time )
    
    return( output )
    
}
