#' A function to find the computer currently in use
#'
#'
#' @keywords computer OperatingSystem UserName
#' @export
#' @return A data frame, with one observation. Collected parameters are returned column-wise.
#' @import parallel

whichComputer <- function() {
    
    computer <- data.frame( name = NA_character_,
                            OS = NA_character_,
                            user = NA_character_,
                            r.version = NA_character_,
                            home.folder = NA_character_,
                            drive.folder = NA_character_,
                            folderRMRW = NA_character_,
                            coresToUse = NA_integer_,
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
                                    NA_character_
    )
    
    maybe.home.folder <- paste0( computer$home.folder, computer$user, "/" )
    if( file.exists( maybe.home.folder ) ) {
        computer$home.folder <- maybe.home.folder
    } else if( file.exists( "~" ) ){
        goback <- getwd()
        setwd( "~" )
        computer$home.folder <- paste0( getwd(), "/" )
        setwd( goback )
    } else {
        computer$home.folder <- NA_character_
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
    } else if( file.exists( "/run/user/1000/gvfs/smb-share:server=pinpfp,share=rrdata/Research/APMS/" ) ) {
        computer$folderRMRW <- "/run/user/1000/gvfs/smb-share:server=pinpfp,share=rrdata/Research/APMS/"
    } else if( file.exists( "~/APMS" ) ) {
        computer$folderRMRW <- paste0( computer$home.folder, "APMS/" )
    }
    
    computer$name <- switch(
        EXPR = computer$home.folder,
        "/Users/ross/" = "rossMBPr",
        "/home/pinp/" = "rossWorkUbuntu",
        NA_character_
    )
    
    computer$coresToUse <- switch(
        EXPR = computer$name,
        "rossMBPr" = 4L,
        "rossWorkUbuntu" = 6L,
        NA_integer_
    )
    
    if( is.na( computer$coresToUse ) ) {
        logicalCores <- parallel::detectCores( logical = FALSE )
        virtualCores <- parallel::detectCores( logical = TRUE )
        if( anyNA( c( logicalCores, virtualCores ) ) ) {
            computer$coresToUse <- 1L
        } else if( virtualCores > logicalCores ) {
            computer$coresToUse <- logicalCores
        } else if( logicalCores > 2 ) {
            computer$coresToUse <- logicalCores - 1L
        } else {
            computer$coresToUse <- 1L
        }
    }
    
    
    # if( applyCores ) {
    #     doMC::registerDoMC( cores = computer$coresToUse )
    # }
    
    return( computer )
}
