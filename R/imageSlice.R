
#' Take an input image, and slice it into equal parts
#'
#' @param image file path to input image
#' @param x.px.out nominal output image dimension, actual dimensions will be calculated
#' @param y.px.out nominal output image dimension, actual dimensions will be calculated
#' @param overwrite logical whether or not to overwrite old outputs
#'
#' @import reticulate
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
    pil.image <- reticulate::import( "PIL.Image", convert = FALSE )
    img <- pil.image$open( image )
    im.dim <- reticulate::py_get_attr( img, "size" )

    im.dim <- reticulate::py_to_r( im.dim )
    im.height <- unlist( im.dim[2] )
    im.width <- unlist( im.dim[1] )


    # find the dimensions to use for output images
    if( im.width %% x.px.out > x.px.out / 2 ) {
        x.dim.out <- floor( im.width / ( 1 + im.width %/% x.px.out ) )
    } else {
        x.dim.out <- floor( im.width / ( im.width %/% x.px.out ) )
    }

    if( im.height %% y.px.out > y.px.out / 2 ) {
        y.dim.out <- floor( im.height / ( 1 + im.height %/% y.px.out ) )
    } else {
        y.dim.out <- floor( im.height / ( im.height %/% y.px.out ) )
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
    image.grid.x <- as.integer( im.width / x.dim.out )
    image.grid.y <- as.integer( im.height / y.dim.out )

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

                output.filename <- normalizePath( outputs$filename[x] )

                crop.grid <- c( outputs$x.start[x],
                                outputs$y.start[x],
                                outputs$x.stop[x],
                                outputs$y.stop[x] )
                crop.grid <- as.integer( crop.grid )
                crop.grid <- reticulate::r_to_py( crop.grid )

                output.array <- img$crop( box = crop.grid )
                output.array$save( output.filename )

            } )


    return( invisible( TRUE ) )

}
