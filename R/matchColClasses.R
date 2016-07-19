# matchColClasses
#' Match the column classes of one data frame to another. Useful before calling `rbind`.
#' Note: this is just an easy way to use `getColClasses` and `applyColClasses` in a single step.
#'
#' @param df1 A data frame or data table to be changed.
#' @param df2 A data frame or data table to be matched. This one will be unchanged
#' @keywords dataframe, datatable, columns, classes
#' @export
#' @import data.table
#' @name matchColClasses

matchColClasses <- function( df1, df2 ) {
    
    if( identical( colnames( df1 ), colnames( df2 ) ) ) {
        
        return( holmberg::applyColClasses( df1, holmberg::getColClasses( df2 ) ) )
        
    } else {
        
        stop( "Column names don't match. Repair, perhaps with matchColNames() first." )
        
    }
    
}



