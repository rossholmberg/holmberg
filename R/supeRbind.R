#' supeRbind
#' Match the column classes of one data frame to another. Useful before calling `rbind`.
#' Note: this is just an easy way to use `getColClasses` and `applyColClasses` in a single step.
#'
#' @param master A data frame or data table acting as MASTER.
#' @param student A data frame or data table, will "learn" from the master.
#' @keywords dataframe, datatable, columns, classes
#' @export
#' @import data.table
#' @import magrittr
#' @return A data frame, with column classes adjusted as necessary.


supeRbind <- function( master, student ) {
    
    output <- student %>%
        matchColNames( master = master ) %>%
        matchColClasses( master = master )
        
    output <- rbind( master, output )
    
    return( output )
    
}