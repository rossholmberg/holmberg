#' Source all files in a particular folder.
#'
#'
#' @param folder Character string directory link to folder.
#' @param except Character vector, files to avoid sourcing
#' @param use.regex When eliminating `except` files, should we use regular expression search?
#' @param quietly Logical, should we output running status along the way?
#' @keywords function, load, source, script
#' @export
#' @return invisible character vector of files sourced

sourceFolder <- function( folder, except = NULL, use.regex = FALSE, quietly = FALSE ) {
    
    files <- list.files( path = folder, pattern = ".R$", full.names = TRUE )
    
    if( !is.null( except ) && !is.na( except ) ) {
        
        if( use.regex ) {
            except <- paste0( except, collapse = "|" )
            files <- files[ !grepl( except, files ) ]
        } else {
            filenames <- sapply( strsplit( files, split = "/" ), tail, n = 1L )
            files <- files[ !( filenames %in% except ) ]
        }
        
    }
    
    for( file in files ) {
        
        source( file )
        
        if( !quietly ) {
            print( paste( "Sourced",
                          tail( strsplit( file, split = "/" )[[1]], n = 1L ) )
            )
        }
        
    }
    
    return( invisible( files ) )
    
}
