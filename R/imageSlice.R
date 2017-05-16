
#' Take an input image, and slice it into equal parts
#' 
#' @param image file path to input image
#' @param x.px.out nominal output image dimension, actual dimensions will be calculated
#' @param y.px.out nominal output image dimension, actual dimensions will be calculated
#' @param overwrite logical whether or not to overwrite old outputs
#' 
#' @importFrom reticulate import
#' @export
#' 

imageSlice <- function( image,
                        x.px.out = 1000,
                        y.px.out = 1000,
                        overwrite = FALSE ) {
    
    # make sure we've got a full path, for python's sake
    image <- normalizePath( image )
    
    # separate the directory
    image.dir <- regexMatch( image, ".*\\/" )
    
    # separate the image name
    image.name <- gsub( ".*/", "", image )
    
    # import the image using python
    scipy.misc <- reticulate::import( "scipy.misc" )
    img <- scipy.misc$imread( image )
    im.dim <- dim( img )[1:2]
    
    
    # find the dimensions to use for output images
    if( im.dim[2] %% x.px.out > x.px.out / 2 ) {
        x.dim.out <- floor( im.dim[2] / ( 1 + im.dim[2] %/% x.px.out ) )
    } else {
        x.dim.out <- floor( im.dim[2] / ( im.dim[2] %/% x.px.out ) )
    }
    
    if( im.dim[1] %% y.px.out > y.px.out / 2 ) {
        y.dim.out <- floor( im.dim[1] / ( 1 + im.dim[1] %/% y.px.out ) )
    } else {
        y.dim.out <- floor( im.dim[1] / ( im.dim[1] %/% y.px.out ) )
    }
    
    
    # make a folder for the output images
    output.folder <- sub( "\\.[a-z|A-Z]*", "", image )
    output.folder <- paste0( output.folder, "-sliced_x", x.dim.out, "_y", y.dim.out )
    
    # Increment with a suffix if necessary
    if( !dir.exists( output.folder ) ) {
        dir.create( output.folder )
    } else if( length( list.files( output.folder ) ) == 0L ) {
        cat( "The output directory exists, but is empty. Using it now.\n" )
    } else if( overwrite ) {
        cat( "Found old outputs, deleting them." )
        old.files <- list.files( path = output.folder, full.names = TRUE )
        cat( "Removing", length( old.files ), "files.\n" )
        file.remove( old.files )
    } else {
        cat( "The output directory already exists. Creating a new one.\n" )
        i <- 1L
        output.folder.try <- paste0( output.folder, "_", i )
        while( dir.exists( output.folder.try ) ) {
            i <- i + 1L
            output.folder.try <- paste0( output.folder, "_", i )
        }
        output.folder <- output.folder.try
        cat( "Using output folder", output.folder, "\n" )
    }
    
    
    
    # create a grid data frame, one row for each output image
    image.grid.x <- as.integer( im.dim[2] / x.dim.out )
    image.grid.y <- as.integer( im.dim[1] / y.dim.out )
    
    outputs <- data.frame(
        x.grid = rep( seq_len( image.grid.x ), image.grid.y ),
        y.grid = sort( rep( seq_len( image.grid.y ), image.grid.x ) ),
        stringsAsFactors = FALSE
    )
    
    
    
    # mark exactly how to subset the input image, for each of the output images
    outputs$x.start <- x.dim.out * ( outputs$x.grid - 1L ) + 1L
    outputs$x.stop <- x.dim.out * ( outputs$x.grid - 1L ) + x.dim.out
    
    outputs$y.start <- y.dim.out * ( outputs$y.grid - 1L ) + 1L
    outputs$y.stop <- y.dim.out * ( outputs$y.grid - 1L ) + y.dim.out
    
    
    # create a unique name for each output file
    outputs$filename <- file.path( output.folder,
                                   paste0(
                                       sub( "\\.[a-z|A-Z]*", "", image.name ),
                                       "_x", outputs$x.start, "-", outputs$x.stop,
                                       "_y", outputs$y.start, "-", outputs$y.stop,
                                       ".jpg"
                                   )
    )
    
    
    # go through the list of output images, subsetting to create each one
    lapply( X = seq_len( nrow( outputs ) ),
            FUN = function(x) {
                output.filename <- outputs$filename[x]
                output.array <- img[ outputs$y.start[x] : outputs$y.stop[x],
                                     outputs$x.start[x] : outputs$x.stop[x], ]
                scipy.misc$imsave( output.filename, output.array )
            } )
    
    
    return( invisible( TRUE ) )
    
}
