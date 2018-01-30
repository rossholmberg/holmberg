#' Read all exif data from a directory of image files recursively.
#' 
#' @param dir a directory path within which to find the images
#' @param progress logical whether to print progress to the console during processing
#'
#' @importFrom data.table fread
#' @export
#' 


exifRead_folder <- function( dir, progress = TRUE ) {
    
    # tidy up the directory string
    dir <- file.path( dirname( dir ), basename( dir ) )
    dir <- gsub( "\\ ", "\\\\ ", dir )
    
    # create the command call
    if( progress ) {
        command <- paste0( "exiftool -csv -progress ", dir, "/*" )
    } else {
        command <- paste0( "exiftool -csv ", dir, "/*" )
    }
    
    # run exiftool
    exif.data <- system( command, intern = TRUE )
    
    # convert the data to a data table
    fread( paste( exif.data, collapse = "\n" ) )
    
}