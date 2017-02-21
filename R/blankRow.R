#' A function for use when adding a data row to a given data frame
#' 
#' @param x a data.frame or data.table to have a row added
#'
#' @importFrom data.table setDT
#' @importFrom data.table is.data.table
#' @importFrom methods as
#' @importFrom chron times
#' @export
#' 

blankRow <- function( x, n = 1L ) {
    
    df <- list()
    for( col in seq_len( ncol( x ) ) ) {
        
        inclass <- class( x[[col]] )[1]
        
        df[[col]] <- switch( inclass,
                             "Date" = as.Date( rep( NA, n ) ),
                             "POSIXct" = as.POSIXct( rep( NA, n ) ),
                             "times" = chron::times( rep( NA, n ) ),
                             as( rep( NA, n ), class( x[[col]] ) ) )
    }
    
    df <- data.frame( df, stringsAsFactors = FALSE )
    names( df ) <- names( x )
    
    if( is.data.table( x ) ) {
        setDT( df )
    }
    
    return( df )
    
}