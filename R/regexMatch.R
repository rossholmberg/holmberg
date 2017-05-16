#' Perform a regular expression match, returning the matched string.
#'
#'
#' @param text input string, can be a vector of strings
#' @param pattern regular expression pattern
#' @param ignore.case to pass to regexpr
#' @param perl to pass to regexpr
#' 
#' @keywords regex
#' @export
#' @return character vector, same length as `text` vector

regexMatch <- function( text, pattern, ignore.case = FALSE, perl = FALSE ) {
    
    regexpr.return <- regexpr( pattern = pattern,
                               text = text,
                               ignore.case = ignore.case )
    
    char.start <- regexpr.return[1]
    char.stop <- regexpr.return[1] + attr( regexpr.return, "match.length" ) - 1L
    
    substr(
        x = text,
        start = char.start,
        stop = char.stop
    )
    
}
