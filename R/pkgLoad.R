#' activate the "install.packages" function, only if the package is not already installed,
#' then load that package
#'
#'
#' @param packages A vector of packages to be checked or installed.
#' Defaults to a list of my favourite packages, all worth having.
#' @param repos A character string, linking to the repository to be utilised if packages need installing.
#' @param quietly TRUE or FALSE, should we suppress helper outputs?
#'
#' @keywords packages
#' @export

pkgLoad <- function( packages = "favourites", repos = "http://cran.ms.unimelb.edu.au/", quietly = FALSE ) {
    
    if( "favourites" %in% tolower( packages ) ) {
        packages <- c( packages[ !grepl( "favourites", packages, ignore.case = TRUE ) ],
                       "data.table", "chron", "plyr", "ggplot2", "tibble",
                       "tidyr", "readr", "purrr", "dplyr", "shiny", "shinyjs",
                       "parallel", "foreach", "ggmap", "ggvis", "doMC",
                       "utils", "stats", "microbenchmark", "readxl", "feather",
                       "googlesheets", "DT", "knitr", "rmarkdown", "Rcpp",
                       "RcppArmadillo", "magrittr"
        )
        packages <- unique( packages )
    }
    
    packagecheck <- match( packages, utils::installed.packages()[,1] )
    
    packagestoinstall <- packages[ is.na( packagecheck ) ]
    
    if( length( packagestoinstall ) > 0L ) {
        
        # construct a feedback sentence, if requested
        if( !quietly ) {
            cat( "Installing packages " )
            for( i in seq_along( packagestoinstall ) ) {
                if( i != 1L ) {
                    if( i == length( packagestoinstall ) ) {
                        cat( " & " )
                    } else {
                        cat( ", " )
                    }
                }
                cat( packagestoinstall[ i ] )
            }
            cat( paste0( " from ", repos, ".\n" ) )
        }
        
        utils::install.packages( packagestoinstall,
                                 repos = repos
        )
    } else if( !quietly ) {
        cat( "All requested packages already installed.\n" )
    }
    
    # Also make sure they're all loaded
    packages.to.load <- packages[ !( paste( "package", packages, sep = ":" ) %in% search() ) ]
    
    if( length( packages.to.load ) > 0L ) {
        if( !quietly ) {
            if( length( packages.to.load ) > 1L ) {
                cat( "Loading packages " )
            } else {
                cat( "Loading package " )
            }
        }
        
        for( i in seq_along( packages.to.load ) ) {
            if( !quietly ) {
                if( i != 1L ) {
                    if( i == length( packages.to.load ) ) {
                        cat( ", & " )
                    } else {
                        cat( ", " )
                    }
                }
                cat( packages.to.load[ i ] )
            }
            suppressPackageStartupMessages(
                library( packages.to.load[ i ], character.only = TRUE, quietly = TRUE, logical.return = TRUE )
            )
        }
        
        if( !quietly ) { cat( ".\n" ) }
        
    }
    
}
