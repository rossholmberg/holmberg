% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regexMatch.R
\name{regexMatch}
\alias{regexMatch}
\title{Perform a regular expression match, returning the matched string.}
\usage{
regexMatch(text, pattern, ignore.case = FALSE, perl = FALSE)
}
\arguments{
\item{text}{input string, can be a vector of strings}

\item{pattern}{regular expression pattern}

\item{ignore.case}{to pass to regexpr}

\item{perl}{to pass to regexpr}
}
\value{
character vector, same length as `text` vector
}
\description{
Perform a regular expression match, returning the matched string.
}
\keyword{regex}
