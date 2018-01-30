#' Extract filename, directory, date, and time from image EXIF data.
#'
#'
#' @param paths A vector of file or folder paths to be analysed
#' @param chunk.size How many files to process at a time
#' @keywords image exif date time name
#' @export
#' @import data.table

exifRead <- function( paths, chunk.size = 500 ) {
    
    group <- path <- NULL
    
    files <- data.table( path = file.path( paths ) )
    
    # break up the list into chunks if necessary
    if( length( files ) > chunk.size ) {
        files[ , group := seq_len( .N ) %% chunk.size ]
    } else {
        files[ , group := 1L ]
    }
    
    output <- files[ , read.csv( 
        text = system( paste0( "exiftool -csv '", 
                               paste( path, collapse = "' '" ),
                               "'" ),
                       intern = TRUE ),
        header = TRUE,
        stringsAsFactors = FALSE,
        fill = TRUE,
        strip.white = TRUE
    ), by = group ]
    
    # and send the output to the user
    return( output )
    
}
