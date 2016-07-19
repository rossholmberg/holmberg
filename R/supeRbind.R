# supeRbind
#' Match the column classes of one data frame to another. Useful before calling `rbind`.
#' Note: this is just an easy way to use `getColClasses` and `applyColClasses` in a single step.
#'
#' @param df1 A data frame or data table acting as MASTER.
#' @param df2 A data frame or data table, will be adjusted to match df1.
#' @keywords dataframe, datatable, columns, classes
#' @export
#' @import data.table
#' @name matchColClasses
#' @return A data frame, with column classes adjusted as necessary.


supeRbind <- function( df1, df2 ) {
    rbind( 
        df1,
        matchColClasses( 
            matchColNames( df1, df2 ), 
            df2 
        )
    )
}