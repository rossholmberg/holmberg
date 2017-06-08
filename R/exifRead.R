#' Extract filename, directory, date, and time from image EXIF data.
#'
#'
#' @param paths A vector of file or folder paths to be analysed.
#' @keywords image exif date time name
#' @export

exifRead <- function( paths ) {
    
    files <- paste( paths, collapse = "' '" )
    
    output <- read.csv( 
                text = system( paste0( "exiftool -csv '", files, "'" ), intern = TRUE ),
                header = TRUE,
                stringsAsFactors = FALSE,
                fill = TRUE,
                strip.white = TRUE
            )
    
    # and send the output to the user
    return( output )
    
}
