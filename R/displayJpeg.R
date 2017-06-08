
#' Shortcut to display a jpeg image
#' 
#' @param file path to the jpeg file
#' 
#' @importFrom jpeg readJPEG
#' @importFrom grid rasterGrob
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 annotation_custom
#' @export

displayJpeg <- function( file ) {
    
    # read in the image
    img <- jpeg::readJPEG( file, native = TRUE )
    
    grob <- grid::rasterGrob( img )
    
    ggplot2::ggplot() +
        ggplot2::annotation_custom(
            grob = grob,
            xmin = -Inf,
            xmax = Inf,
            ymin = -Inf,
            ymax = Inf
        )

}
