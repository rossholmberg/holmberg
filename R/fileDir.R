#'
#' Find the complete directory structure of the folder containing
#' a specified file.
#' 
#' @param file character string
#' @export
#' 


fileDir <- function( file ) {
    
    # remove the file itself from the end of the string,
    # and navigate to that directory
    setwd( sub( "/[^/]*$", "", file ) )
    
    # return the full directory link. This will,
    # for example, replace "~" with a specific link
    getwd()
    
}
