#' Match the column names between 2 data frames. Useful to use before doing calling `rbind`.
#'
#'
#' @param 
#' df1 A dataframe or datatable, to be changed to match columns of df2.
#' df2 A "master" dataframe or datatable, df1 will be changed to match this.
#' @export
#' @keywords dataframe, datatable, columns, matching
#' @import data.table
#' @return A data frame, with column names adjusted as necessary.

matchColNames <- function( df1, df2 ) {
    
    # flag whether or not the input started as a data.table or data.frame
    df1.was.DT <- data.table::is.data.table( df1 )
    
    # make everything data.table class
    data.table::setDT( df1 )
    data.table::setDT( df2 )
    
    # get the column names of both datatables
    df1.colnames <- colnames( df1 )
    df2.colnames <- colnames( df2 )
    
    # check if they already match
    if( identical( df1.colnames, df2.colnames ) ) {
        print( "No change to column names needed." )
        
        # check if we at least have the same number of columns    
    } else if( length( df1.colnames ) == length( df2.colnames ) ) {
        
        # check to see if the column names are the same, just out of order
        if( sum( is.na( match( df2.colnames, df1.colnames ) ) ) == 0L ) {
            
            print( "Columns seem to just be out of order. Reordering..." )
            data.table::setcolorder( df1, df2.colnames )
            
        } else if( sum( is.na( match( df2.colnames, df1.colnames ) ) ) == 1L ) {
            print( "Only 1 column name isn't matched. Renaming it to match." )
            data.table::setnames( df1, 
                                  df1.colnames[ is.na( match( df2.colnames, df1.colnames ) ) ],
                                  df2.colnames[ is.na( match( df2.colnames, df1.colnames ) ) ]
            )
            
            print( "And reordering..." )
            data.table::setcolorder( df1, df2.colnames )
            
        } else {
            
            stop(
                paste( sum( is.na( match( df2.colnames, df1.colnames ) ) ),
                       "column names cannot be matched automatically. Make corrections and try again." )
            )
            
        }
        
        # different number of columns... let's see what we can do about that    
        # there might just be missing columns, so add them...
    } else if( sum( is.na( match( df1.colnames, df2.colnames ) ) ) == 0L ) {
        
        print( paste( "It looks like",
                      sum( is.na( match( df2.colnames, df1.colnames ) ) ),
                      "columns are missing. Adding them now."
        ) )
        
        # get a list of new columns that are needed
        cols.to.add <- df2.colnames[ !grepl( df1.colnames, df2.colnames ) ]
        
        # add those new columns
        df1[ , as.vector( cols.to.add ) := NA ]
        
        # and get the column ordering right
        print( "And reordering..." )
        data.table::setcolorder( df1, df2.colnames )
        
    }
    
    # change the output back to a dataframe if that's how it was brought in
    if( !df1.was.DT ) {
        data.table::setDF( df1 )
    }
    
    # send the fixed up datatable or dataframe back as output
    return( df1 )
    
}