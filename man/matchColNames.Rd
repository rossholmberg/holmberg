% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchColNames.R
\name{matchColNames}
\alias{matchColNames}
\title{Match the column names between 2 data frames. Useful to use before doing calling `rbind`.}
\usage{
matchColNames(master, student, remove.unwanted = TRUE)
}
\arguments{
\item{master}{A data frame or data table acting as MASTER.}

\item{student}{A data frame or data table, will "learn" from the master.}

\item{remove.unwanted}{logical. Whether or not to remove columns not needed to match "master"}
}
\value{
A data frame, with column names adjusted as necessary.
}
\description{
Match the column names between 2 data frames. Useful to use before doing calling `rbind`.
}
\keyword{columns,}
\keyword{dataframe,}
\keyword{datatable,}
\keyword{matching}
