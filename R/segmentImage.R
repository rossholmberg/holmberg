

#' Functions for segmenting an image into a grid of smaller images
#' 
#' @param input.image file path to input file
#' @param grid.coord.upperleft.x pixel coordinate value
#' @param grid.coord.upperleft.y pixel coordinate value
#' @param image.width pixels
#' @param image.height pixels
#' @param output.filename file path to output file
#' @export

# create a function for creating the output images
cropAndOutput <- function( input.image,
                           grid.coord.upperleft.x,
                           grid.coord.upperleft.y,
                           image.width,
                           image.height,
                           output.filename ) {
    
    # create a subfolder alongside the input image
    if( !dir.exists( gsub( "\\.jpg|\\.JPG|\\.jpeg|\\.JPEG", "", input.image ) ) ) {
        dir.create( gsub( "\\.jpg|\\.JPG|\\.jpeg|\\.JPEG", "", input.image ) )
    }
    
    
    sys.command <- sprintf( "jpegtran -crop %dx%d+%d+%d -copy none %s > %s",
                            image.width, 
                            image.height,
                            grid.coord.upperleft.x,
                            grid.coord.upperleft.y,
                            input.image,
                            output.filename )
    
    system( sys.command )
    
    feedback <- if( file.exists( output.filename ) ) {
        cat( paste( "Successfully written", output.filename, "\n" ) )
    } else {
        cat( paste( "FAILED to write", output.filename, "\n" ) )
    }
    
    return( invisible( feedback ) )
    
}




#' Break up a .jpg image into segments, before outputting to individual files
#' 
#' @title segmentImage
#' @name segmentImage
#' 
#' @param input.image file name of input jpeg
#' @param output.grid integer vector of format "c( x, y )"
#' @import magrittr
#' @import data.table
#' @export
    

segmentImage <- function( input.image, output.grid = NULL ) {
    
    input.format <- strsplit( input.image, split = "\\." ) %>%
                            sapply( tail, n = 1L )
    
    if( !input.format %in% c( "jpg", "jpeg", "JPG", "JPEG" ) ) {
        stop( "`input.image` must be jpg format" )
    }
    
    # prepare some dummy variables to keep Roxygen happy
    ImageWidth <- ImageHeight <- grid.ref.x <- grid.ref.y <- NULL
    grid.coord.upperleft.x <- grid.coord.upperleft.y <- NULL
    image.width <- image.height <- output.filename <- NULL
    
    input.dim <- holmberg::exifRead( input.image, coresToUse = 1 ) %>%
        setDT() %>%
        .[ , .( ImageWidth, ImageHeight ) ]
    
    if( !is.null( output.grid ) ) {
        outputGrid.x <- output.grid[1]
        outputGrid.y <- output.grid[2]
    } else {
        stop( "Missing parameter `output.grid`." )
    }
    
    outputDim.x <- round( input.dim$ImageWidth / outputGrid.x )
    outputDim.y <- round( input.dim$ImageHeight / outputGrid.y )
    
    # set up a data frame with details for each output image
    crop.setup <- expand.grid( seq_len( outputGrid.x ), seq_len( outputGrid.y ) ) %>%
        as.data.table() %>%
        setnames( c( "grid.ref.x", "grid.ref.y" ) ) %>%
        .[ , grid.coord.upperleft.x := ( grid.ref.x - 1L ) * outputDim.x ] %>%
        .[ , grid.coord.upperleft.y := ( grid.ref.y - 1L ) * outputDim.y ] %>%
        .[ , image.width := outputDim.x ] %>%
        .[ grid.ref.x == outputGrid.x, image.width := input.dim$ImageWidth - ( outputGrid.x - 1L ) * outputDim.x ] %>%
        .[ , image.height := outputDim.y ] %>%
        .[ grid.ref.y == outputGrid.y, image.height := input.dim$ImageHeight - ( outputGrid.y - 1L ) * outputDim.y ] %>%
        .[ , output.filename := gsub( "\\.jpg|\\.JPG|\\.jpeg|\\.JPEG", "", input.image ) %>% 
               paste0( "/crop", holmberg::stringLength( seq_len( .N ), 3, "tail", fill = "0" ) ) %>%
               paste0( "_x", grid.ref.x, "_y", grid.ref.y, ".jpg" ) ]
    
    # apply the setup parameters to output the images as desired
    crop.setup[ , cropAndOutput( input.image,
                                 grid.coord.upperleft.x,
                                 grid.coord.upperleft.y,
                                 image.width,
                                 image.height,
                                 output.filename ), 
                by = .( grid.ref.x, grid.ref.y ) ]
    
    return( invisible() )
    
}
