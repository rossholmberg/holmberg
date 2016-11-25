#' Apply column classes to a data frame. Useful before calling `rbind`.
#'
#'
#' @param student A data frame to be adjusted.
#' @param master.classes A vector of classes to be applied. Probably an output from `getColClasses`.
#' @keywords dataframe, datatable, columns, classes
#' @export
#' @return A data frame, with column classes adjusted as necessary.


applyColClasses <- function( student, master.classes ) {
    
    # for every column in the student frame...
    for( col.num in seq_len( dim( student )[2] ) ) {
        
        # check if it's already what we want
        if( class( student[[col.num]] )[1] == master.classes[col.num] ) {
            
            # if so, move on
            next
        }
        
        # if it's a factor class column...
        if( class( student[[col.num]] )[1] == "factor" ) {
            
            # convert it to character class before going any further
            student[[col.num]] <- as.character( student[[col.num]] )
        }
        
        if( master.classes[col.num] == "times" ) {
            
            student[[col.num]] <- chron::times( student[[col.num]] )
            
        } else if( master.classes[col.num] == "POSIXct" ) {
            
            student[[col.num]] <- as.POSIXct( student[[col.num]] )
            
        } else if( master.classes[col.num] == "Date" ) {
            
            student[[col.num]] <- as.Date( student[[col.num]] )
            
        } else {
            
            class( student[[col.num]] ) <- master.classes[col.num]
            
        }
        
    }
    
    # give the adjusted frame back to the user
    return( student )
    
}

