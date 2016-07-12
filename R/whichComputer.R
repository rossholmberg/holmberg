#' A function to find the computer currently in use
#'
#'
#' @keywords computer OperatingSystem UserName
#' @export

whichComputer <- function() {
    
    computer <- data.frame( name = as.character( NA ),
                            OS = as.character( NA ),
                            user = as.character( NA ),
                            r.version = as.character( NA ),
                            home.folder = as.character( NA ),
                            drive.folder = as.character( NA ),
                            folderRMRW = as.character( NA ),
                            stringsAsFactors = FALSE
    )
    
    computer$OS <- Sys.info()[['sysname']]
    if( computer$OS == "Darwin" ) { computer$OS <- "MacOSX" }
    
    computer$user <- Sys.info()[['user']]
    
    computer$r.version <- paste0( R.Version()[['major']],
                                  ".",
                                  R.Version()[['minor']]
    )
    
    computer$home.folder <- switch( EXPR = computer$OS,
                                    Windows = { "C:/Users/" },
                                    Linux  = { "/home/" },
                                    MacOSX = { "/Users/" },
                                    as.character( NA )
    )
    
    maybe.home.folder <- paste0( computer$home.folder, computer$user, "/" )
    if( file.exists( maybe.home.folder ) ) {
        computer$home.folder <- maybe.home.folder
    } else {
        goback <- getwd()
        setwd( "~" )
        computer$home.folder <- paste0( getwd(), "/" )
        setwd( goback )
    }
    
    
    if( file.exists( paste0( computer$home.folder, "Google Drive" ) ) ) {
        computer$drive.folder <- paste0( computer$home.folder, "Google Drive/" )
        if( file.exists( paste0( computer$drive.folder,
                                 "Pinguino Project/Ross - Monash Research work/" )
        ) ) {
            computer$folderRMRW <- paste0( computer$drive.folder,
                                           "Pinguino Project/Ross - Monash Research work/" )
        } else if( file.exists( paste0( computer$drive.folder,
                                        "Ross - Monash Research work/" ) ) ) {
            computer$folderRMRW <- paste0( computer$drive.folder,
                                           "Ross - Monash Research work/" )
        }
    } else if( file.exists( "~/APMS" ) ) {
        computer$folderRMRW <- paste0( computer$home.folder, "APMS/" )
    }
    
    computer$name <- switch(
        EXPR = computer$folderRMRW,
        "/Users/ross/Google Drive/Ross - Monash Research work/" = "rossMBPr",
        as.character( NA )
    )
    
    return( computer )
}
