#' testDF, a function for creating a data frame for mucking around with.
#'
#' @param rows how big should the data frame be
#' @param DT should the output be a data.table? (data.frame is output if FALSE)
#' @importFrom data.table setDT
#' @importFrom stats rnorm
#' @export
#' 

testDF <- function( rows, DT = FALSE ) {
    
    df <- data.frame(
        strings = sapply( seq_len( rows ),
                          function(x) {
                              paste( sample( letters, 4, replace = TRUE ), collapse = "" )
                          } ),
        ints = sample( seq.int( from = 1000L, to = 9999L, by = 1L ), rows ),
        rands = rnorm( rows ),
        stringsAsFactors = FALSE
    )
    
    if( DT ) {
        setDT( df )
    }
    
    return( df )
    
}
