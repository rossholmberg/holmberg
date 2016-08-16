#' Apply a new exif tag to image files
#'
#'
#' @param files A vector of files to be modified.
#' @param comment A character string to add to the files' exif data as a comment
#' @keywords image exif add comment
#' @export

# files <- list.files( path = "~/Desktop/", pattern = ".jpg", full.names = TRUE )

exifWrite <- function( files, comment = NA ) {
    
    system(
        paste0( 'exiftool -comment="', tag, '" -preserve whp.jpg' )
    )
    
}
