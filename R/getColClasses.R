#' Retrieve the column classes from a data frame.
#'
#'
#' @param student A data frame from which to retrieve classes.
#' @keywords dataframe, datatable, columns, classes
#' @export
#' @return A vector of classes, represented as character strings.


getColClasses <- function( student ) {
    
    col.classes <- vector( mode = "character", length = 0L )
    
    for( col.num in seq_len( dim( student )[2] ) ) {
        
        col.classes <- c( col.classes, class( student[[col.num]] )[1] )
        
    }
    
    return( col.classes )
    
}

