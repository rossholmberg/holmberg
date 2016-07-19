#' Apply column classes to a data frame. Useful before calling `rbind`.
#'
#'
#' @param df
#' @param col.classes A vector of classes to be applied. Probably an output from `getColClasses`.
#' @keywords dataframe, datatable, columns, classes
#' @export
#' @return A data frame, with column classes adjusted as necessary.


applyColClasses <- function( df, col.classes ) {
    
    for( col.num in seq_len( dim( df )[2] ) ) {
        
        if( class( df[[col.num]] )[1] == col.classes[col.num] ) {
            
            next
            
        }
        
        if( col.classes[col.num] == "times" ) {
            
            df[[col.num]] <- chron::times( df[[col.num]] )
            
        } else if( col.classes[col.num] == "POSIXct" ) {
            
            df[[col.num]] <- as.POSIXct( df[[col.num]] )
            
        } else {
            
            class( df[[col.num]] ) <- col.classes[col.num]
            
        }
        
    }
    
    return( df )
    
}

