#' Return a sequential vector along x row wise.
#'
#'
#' @param x data.frame (incl. data.table) or matrix object
#' @keywords data.frame data.table matrix
#' @export
#' @return vector sequence

seq_down <- function( x ) {
    
    if( !is.data.frame(x) && !is.matrix(x) ) {
        stop( "Input `x` must be a data.frame (incl. data.table), or matrix" )
    }
    
    return(
        seq_along( nrow( x ) )
    )
}


#' Return a sequential vector along x column-wise.
#'
#'
#' @param x data.frame (incl. data.table) or matrix object
#' @keywords data.frame data.table matrix
#' @export
#' @return vector sequence

seq_across <- function( x ) {
    
    if( !is.data.frame(x) && !is.matrix(x) ) {
        stop( "Input `x` must be a data.frame (incl. data.table), or matrix" )
    }
    
    return(
        seq_along( ncol( x ) )
    )
}


#' Return a sequential vector along x in the dimension specified.
#'
#'
#' @param x data.frame (incl. data.table) or array (incl. matrix) object
#' @param mar integer for margin along which to create the sequence
#' @keywords data.frame data.table array matrix
#' @export
#' @return vector sequence

seq_dim <- function( x, mar ) {
    
    if( !is.data.frame(x) && !is.array(x) ) {
        stop( "Input `x` must be a data.frame (incl. data.table), or array (incl. matrix)" )
    }
    
    return(
        seq_len( dim( x )[mar] )
    )
}
