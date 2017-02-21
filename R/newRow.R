#' A function for adding a data row to a given data frame
#' 
#' @param x a data.frame or data.table to have a row added
#'
#' @importFrom data.table setDT
#' @importFrom data.table is.data.table
#' @importFrom methods as
#' @importFrom chron times
#' @export
#' 

newRow <- function( x ) {
    
    # extract column classes of the existing data frame
    col.classes <- lapply( x, class )
    col.classes <- sapply( col.classes, "[", 1L )
    
    # make sure there aren't any list columns
    if( "list" %in% col.classes ) {
        stop( "Sorry, list columns are not supported yet." )
    }
    
    # start an empty list to be filled
    newrow <- list()
    
    # go through columns one-by-one
    for( i in seq_along( x ) ) {
        
        # ask the user for the value to add at each column
        input <- readline( prompt = paste( names( x )[i],
                                           "... (class:",
                                           class( x[[i]] ),
                                           ") :  " )
        )
        
        # use NA instead of blank
        if( input == "" ) {
            input <- NA_character_
        }
        
        # convert the class of the input value as necessary
        input <- switch( col.classes[i],
                         "Date" = as.Date( input ),
                         "POSIXct" = as.POSIXct( input ),
                         "times" = chron::times( input ),
                         as( input, col.classes[i] )
        )
        
        # add this column value to the list
        newrow[[i]] <- input
    }
    
    # convert the list to a data frame (with one row)
    newrow <- data.frame( newrow, stringsAsFactors = FALSE )
    
    # convert to data.table if necessary
    if( is.data.table( x ) ) {
        setDT( newrow )
    }
    
    # rename columns to match the input data frame
    names( newrow ) <- names( x )
    
    # display the completed new line to the user
    print( newrow )
    
    # bind the input data frame with the new row, and output to the user
    return( rbind( x, newrow ) )
    
}
