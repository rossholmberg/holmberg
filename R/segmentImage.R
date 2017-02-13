

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
#' @param output.grid integer vector specifying how to divide the input image. format "c( x, y )"
#' @param output.px integer vector specifying nominal dimensions of output images. format "c( x, y )"
#' @param exact if TRUE, output.px will be followed exactly, even if the image is not divisible exactly.
#' if FALSE, output.px will be automatically maintained as closely as possible while dividing the image exactly.
#' @import magrittr
#' @import data.table
#' @export
    

segmentImage <- function( input.image, output.grid = NULL, output.px = NULL, exact = TRUE ) {
    
    # check that one of the appropriate input parameters were used
    if( ( is.null( output.grid ) && is.null( output.px ) ) ||
        ( !is.null( output.grid ) && !is.null( output.px ) ) ) {
        stop( "You must specify either `output.grid` or `output.px`, but not both." )
    }
    
    # note down which input parameter we'll be using here
    if( !is.null( output.grid ) ) {
        input.parameter.touse <- "grid"
    } else if( !is.null( output.px ) ) {
        input.parameter.touse <- "px"
    }
    
    # check that whichever parameter was used is of length 2
    if( input.parameter.touse == "grid" && length( output.grid ) != 2L ) {
        stop( "The parameter `output.grid` must be a vector of length 2." )
    }
    if( input.parameter.touse == "px" && length( output.px ) != 2L ) {
            stop( "The parameter `output.px` must be a vector of length 2." )
    }
    
    # check that whichever parameter was used is numeric
    if( !is.numeric( output.grid ) && !is.numeric( output.px ) ) {
        stop( "The parameter `output.grid` or `output.px` must be an integer or numeric vector.")
    }
    
    # check that the `exact` input parameter is either TRUE or FALSE
    if( is.na( exact ) ||is.null( exact ) || ( !isTRUE( exact ) && exact != FALSE ) ) {
        stop( "The parameter `exact` must be either TRUE or FALSE." )
    }
    
    # find the format of the input file (to make sure it's a jpeg image)
    input.format <- strsplit( input.image, split = "\\." ) %>%
                            sapply( tail, n = 1L )
    if( !input.format %in% c( "jpg", "jpeg", "JPG", "JPEG" ) ) {
        stop( "`input.image` must be jpg format" )
    }
    
    # prepare some dummy variables to keep Roxygen happy
    ImageWidth <- ImageHeight <- grid.ref.x <- grid.ref.y <- NULL
    grid.coord.upperleft.x <- grid.coord.upperleft.y <- NULL
    image.width <- image.height <- output.filename <- NULL
    
    # find the dimensions of the input file
    input.dim <- holmberg::exifRead( input.image, coresToUse = 1 ) %>%
        setDT() %>%
        .[ , .( ImageWidth, ImageHeight ) ]
    
    if( input.parameter.touse == "px" ) {
        outputGrid.x <- ceiling( input.dim$ImageWidth )
        outputGrid.y <- ceiling( input.dim$ImageHeight )
    } else {
        outputGrid.x <- output.grid[1]
        outputGrid.y <- output.grid[2]
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
