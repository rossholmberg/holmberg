#' A function to find the computer currently in use
#'
#'
#' @param applyCores If TRUE, uses the obtained `coresToUse` values via doMC's `registerDoMC`.
#' @keywords computer OperatingSystem UserName
#' @export
#' @return A data frame, with one observation. Collected parameters are returned column-wise.
#' @import parallel
#' @import doMC

whichComputer <- function( applyCores = TRUE ) {
    
    computer <- data.frame( name = as.character( NA ),
                            OS = as.character( NA ),
                            user = as.character( NA ),
                            r.version = as.character( NA ),
                            home.folder = as.character( NA ),
                            drive.folder = as.character( NA ),
                            folderRMRW = as.character( NA ),
                            coresToUse = as.integer( NA ),
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
    
    logicalCores <- parallel::detectCores( logical = FALSE )
    virtualCores <- parallel::detectCores( logical = TRUE )
    if( virtualCores > logicalCores ) {
        computer$coresToUse <- logicalCores
    } else if( logicalCores > 2 ) {
        computer$coresToUse <- logicalCores - 1L
    } else {
        computer$coresToUse <- 1L
    }
    
    if( applyCores ) {
        doMC::registerDoMC( cores = computer$coresToUse )
    }
    
    return( computer )
}
