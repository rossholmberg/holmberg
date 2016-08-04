#' Search Google Maps for GPS coordinates, and return as latitude and longitude.
#'
#'
#' @param location A string to pass to Google maps API for searching.
#' @keywords GoogleMaps
#' @export
#' @return dataframe

getLatLong <- function( location ) {
    
    # Create an appropriate URL to search via the Google Maps API
    urlForGoogleAPI <- utils::URLencode(
        paste0( "http://maps.googleapis.com/maps/api/geocode/xml?address=", location )
    )
    
    # attempt to retrieve data from the Google Maps API
    input <- try( 
        utils::read.table( urlForGoogleAPI, 
                           sep = ":",
                           col.names = c( "split1", "split2" ),
                           colClasses = "character",
                           flush = TRUE,
                           fill = TRUE,
                           strip.white = TRUE,
                           stringsAsFactors = FALSE 
        ), 
        silent = TRUE 
    )
    
    output <- data.frame( location = location,
                          address = as.character( NA ),
                          lat = as.numeric( NA),
                          long = as.numeric( NA )
    )
    
    # first check that the data was returned
    if( class( input ) != "try-error" ) {
        
        # see if there's a straight "location" returned from the API
        locationRow <- match( "location", input$split1 )
        
        # if so, use it (or at least the rows just below it)
        if( !is.na( locationRow ) & 
            grepl( "lat", input1$split1[ locationRow + 1 ] ) ) {
            output$lat <- as.numeric( sub( ",", "", input$split2[ locationRow + 1 ] 
            ) )
        } else {
            # otherwise, we'll need to use the "boundary" values, using them to get a
            # best guess of the location's latitude and longitude
            output$lat <- mean( as.numeric( sub( ",", "", 
                                                 input$split2[ grepl( "lat", input$split1 ) ] 
            ) ),
            na.rm = TRUE )
        }
        
        # if so, use it (or at least the rows just below it)
        if( !is.na( locationRow ) & 
            grepl( "lng", input$split1[ locationRow + 2 ] ) == TRUE ) {
            output$long <- as.numeric( sub( ",", "", input$split2[ locationRow + 2 ] 
            ) )
        } else {
            # otherwise, we'll need to use the "boundary" values, using them to get a
            # best guess of the location's latitude and longitude
            output$long <- mean( as.numeric( sub( ",", "", 
                                                  input$split2[ grepl( "lng", input$split1 ) ] 
            ) ),
            na.rm = TRUE )
        }
        
        
        # see if there's a formatted address value. If so, use it.
        if( !is.na( match( "formatted_address", input$split1 ) ) ) {
            
            output$address <- input$split2[ match( "formatted_address", input$split1 ) ]
            
        }
        
    }
    
    return( output )
    
}

