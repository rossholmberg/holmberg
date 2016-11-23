#' Match the column names between 2 data frames. Useful to use before doing calling `rbind`.
#'
#'
#' @param master A data frame or data table acting as MASTER.
#' @param student A data frame or data table, will "learn" from the master.
#' @param remove.unwanted logical. Whether or not to remove columns not needed to match "master"
#' @export
#' @keywords dataframe, datatable, columns, matching
#' @import data.table
#' @return A data frame, with column names adjusted as necessary.

matchColNames <- function( master, student, remove.unwanted = TRUE ) {
    
    # flag whether or not the input started as a data.table or data.frame
    student.was.DT <- data.table::is.data.table( student )
    
    # make everything data.table class
    data.table::setDT( master )
    data.table::setDT( student )
    
    # get the column names of both datatables
    master.colnames <- colnames( master )
    student.colnames <- colnames( student )
    
    # check if they already match
    if( identical( master.colnames, student.colnames ) ) {
        print( "No change to column names needed." )
        
        # change the output back to a dataframe if that's how it was brought in
        if( !student.was.DT ) {
            data.table::setDF( student )
        }
        
        # send the fixed up datatable or dataframe back as output
        return( student )
        
        break
        
        
    }
    
    # remove unwanted columns if requested
    if( sum( !student.colnames %in% master.colnames ) > 0 ) {
        cols.to.remove <- which( !student.colnames %in% master.colnames )
        if( remove.unwanted ) {
            
            print( paste( "Columns",
                          paste( student.colnames[ cols.to.remove ], collapse = ", " ),
                          "are not needed, removing them." ) )
            student[ , student.colnames[ cols.to.remove ] := NULL ]
            student.colnames <- colnames( student )
            
        } else {
            stop( print( paste( "Columns",
                                paste( cols.to.remove, collapse = ", " ),
                                "are not needed, try running with `remove.unwanted = TRUE` to remove them." ) )
            )
        }
    }
    
    # check if there are columns missing, and add them if so
    if( sum( !master.colnames %in% student.colnames ) > 0 ) {
        
        cols.missing <- which( !master.colnames %in% student.colnames )
        
        print( paste( "Columns", 
                      paste( master.colnames[ cols.missing ], collapse = ", " ), 
                      "are missing, adding them now" ) 
        )
        
        # add those new columns
        student[ , master.colnames[ cols.missing ] := NA ]
        
        student.colnames <- colnames( student )
        
        
    }
    
    # check if we at least have the same number of columns
    if( length( master.colnames ) == length( student.colnames ) ) {
        
        # check to see if the column names are the same, just out of order
        if( sum( is.na( match( student.colnames, master.colnames ) ) ) == 0L ) {
            
            print( "Columns are out of order. Reordering..." )
            data.table::setcolorder( student, master.colnames )
            
        } else {
            
            stop(
                paste( sum( is.na( match( master.colnames, student.colnames ) ) ),
                       "column names cannot be matched automatically. Make corrections and try again." )
            )
            
        }
        
        # different number of columns... let's see what we can do about that    
        # there might just be missing columns, so add them...
    } else if( sum( is.na( match( student.colnames, master.colnames ) ) ) == 0L ) {
        
        print( paste( "It looks like",
                      sum( is.na( match( master.colnames, student.colnames ) ) ),
                      "columns are missing. Adding them now."
        ) )
        
        # get a list of new columns that are needed
        cols.to.add <- master.colnames[ !grepl( student.colnames, master.colnames ) ]
        
        # add those new columns
        student[ , as.vector( cols.to.add ) := NA ]
        
        # and get the column ordering right
        print( "And reordering..." )
        data.table::setcolorder( student, master.colnames )
        
    }
    
    # change the output back to a dataframe if that's how it was brought in
    if( !student.was.DT ) {
        data.table::setDF( student )
    }
    
    # send the fixed up datatable or dataframe back as output
    return( student )
    
}