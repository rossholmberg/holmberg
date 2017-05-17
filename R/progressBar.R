
#' 
#' Add a progress bar to a loop call, eg `for`, `while`, or `*apply`
#' 
#' @param i iterator, feeding in from the loop call, should be different for each iteration
#' @param n the total number of iterations in the call
#' @param length the character length of the progress bar. 25 works quite well
#' 
#' @export
#' 

progressBar <- function( i, n, length = 25 ) {

    l <- as.integer( length )
    i <- as.integer( i )
    
    if( i == 1L ) {
        cat( "Processing...\n" )
    }
    
    frac <- i / n
    frac.int <- as.integer( frac * l )
    
    cat( "\r",
         paste0( "|",
                 paste( rep( "=", frac.int ), collapse = "" ),
                 paste( rep( " ", l - frac.int ), collapse = "" ),
                 "| ", as.integer( frac * 100 ), " %"
         )
    )
    
}

