#' Retrieve the column classes from a data frame.
#'
#'
#' @param student A data frame from which to retrieve classes.
#' @keywords dataframe, datatable, columns, classes
#' @export
#' @return A vector of classes, represented as character strings.


getColClasses <- function( student ) {

    return( 
        sapply( student, class ) 
            )
    
}

