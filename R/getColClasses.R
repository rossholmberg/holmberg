#' Retrieve the column classes from a data frame.
#'
#'
#' @param df
#' @keywords dataframe, datatable, columns, classes
#' @export


getColClasses <- function( df ) {
    
    col.classes <- vector( mode = "character", length = 0L )
    
    for( col.num in seq_len( dim( df )[2] ) ) {
        
        col.classes <- c( col.classes, class( df[[col.num]] )[1] )
        
    }
    
    return( col.classes )
    
}

