#' Smooth a numeric vector using moving window averaging.
#'
#'
#'
#' @param input A numeric vector of data to be smoothed.
#' @param window Numeric value for the size of each "group" to be averaged.
#' @param overlap Logical. Should the windows overlap (by half the window size)?
#' @param fill.NAs Logical. If TRUE, only the centre point of each window is filled.
#' Goes to TRUE if `overlap` is TRUE.
#' @param na.Rm Logical, to be passed to `mean` function as `na.rm`.
#' @keywords mean smooth rolling window
#' @export
#' @import data.table


runMean <- function( input,
                     window,
                     overlap = FALSE,
                     fill.NAs = TRUE,
                     na.Rm = TRUE ) {
    
    # put the data into a table
    row.means.calc <- data.table( input.data = input )
    
    # calculate means based on data
    row.means.calc[ , means.group := ceiling( seq_len( .N ) / window )
                    ][ , group.mean := mean( input.data, na.rm = na.Rm ),
                       by = means.group
                       ]
    
    # fill with NAs if requested
    if( overlap | fill.NAs ) {
        row.means.calc[ , group.row := seq_len( .N ),
                        by = means.group
                        ][ , group.row.means := floor( mean( group.row ) ),
                           by = means.group
                           ][ group.row != group.row.means, group.mean := NA
                              ]
    }
    
    
    # if the user specified overlapping windows, get the second set of means,
    # and merge them into the means data column
    if( overlap ) {
        
        # find the new groups
        row.means.calc[ !is.na( group.mean ),
                        group.mean.2 := 1L
                        ][ is.na( group.mean.2 ),
                           group.mean.2 := 0L ]
        
        # number the groups, and get each mean value
        row.means.calc[ , group.mean.2 := cumsum( group.mean.2 )
                        ][ group.mean.2 > 0.5,
                           group.means.2 := mean( input.data ),
                           by = group.mean.2 ]
        
        # taking only the cells we want, merge the two means columns
        row.means.calc[ , group.row.2 := seq_len( .N ),
                        by = group.mean.2
                        ][ , group.row.means.2 := ceiling( mean( group.row.2 ) ),
                           by = group.mean.2
                           ][ group.row.2 == group.row.means.2,
                              group.mean := group.means.2
                              ]
    }
    
    
    # output the new vector
    return( row.means.calc$group.mean )
}
