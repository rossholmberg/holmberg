#' Strip the white space from around a character string
#'
#'
#' @param input A character vector from which white space is to be stripped.
#' @param which One of "both", "leading", or "trailing". Specifying from which
#' end (or both) to strip white space.
#' @keywords string character whitespace leading trailing
#' @export

stripWhiteSpace <- function( input, which = "both" ) {
    
    # strip leading AND trailing white space
    if( which == "both" ) {
        output <- gsub( "^\\s+|\\s+$", "", input ) 
        
    # or just the leading white space
    } else if( which == "leading" ) {
        output <- gsub( "^\\s+", "", input )
        
    # or just the trailing white space
    } else if( which == "trailing" ) {
        output <- gsub( "\\s+$", "", input )
    }
        
    return( output )
}
