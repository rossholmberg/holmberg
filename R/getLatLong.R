#' Search Google Maps for GPS coordinates, and return as latitude and longitude.
#'
#'
#' @param location A string to pass to Google maps API for searching.
#' @keywords GoogleMaps
#' @export
#' @return 

getLatLong <- function( location ) {
    
    # Create an appropriate URL to search via the Google Maps API
    urlForGoogleAPI <- utils::URLencode(
        paste0( "http://maps.googleapis.com/maps/api/geocode/json?address=", location )
    )
    
    # attempt to retrieve data from the Google Maps API
    input <- try( 
        utils::read.csv( urlForGoogleAPI, stringsAsFactors = FALSE ), 
        silent = TRUE 
    )
    
    address <- as.character( NA )
    lat <- as.character( NA )
    long <- as.character( NA )
    
    # first check that the data was returned
    if( class( input ) != "try-error" ) {
        
        # then get to work on the data
        names( input ) <- "incoming"
        
        # break up the data a little
        breakUp <- function(x, i) { strsplit( x, " : " )[[1]][i] }
        input$split1 <- sapply( X = input$incoming, FUN = breakUp, i = 1 )
        input$split1 <- gsub( " ", "", input$split1 )
        input$split2 <- sapply( X = input$incoming, FUN = breakUp, i = 2 )
        
        # see if there's a straight "location" returned from the API
        locationRow <- match( "location", input$split1 )
        
        # if so, use it (or at least the rows just below it)
        if( !is.na( locationRow ) ) {
            lat <- gsub( " ", "", input$split2[ locationRow + 1 ] )
            long <- gsub( " ", "", input$split2[ locationRow + 2 ] )
            
        } else {
            
            # otherwise, we'll need to use the "boundary" values, using them to get a
            # best guess of the location's latitude and longitude
            
            lat <- mean( as.numeric( input$split2[ grepl( "lat", input$split1 ) ] ),
                         na.rm = TRUE
            )
            long <- mean( as.numeric( input$split2[ grepl( "lng", input$split1 ) ] ),
                          na.rm = TRUE
            )
            
        }
        
        
        # see if there's a formatted address value
        formattedAddressRow <- match( "formatted_address", input$split1 )
        if( !is.na( formattedAddressRow ) ) {
            address <- input$split2[ formattedAddressRow ]
        }
        
    }
    
    output <- data.frame( location = location, address = address,
                          lat = lat, long = long )
    
    return( output )
    
}

