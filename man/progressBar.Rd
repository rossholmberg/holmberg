% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progressBar.R
\name{progressBar}
\alias{progressBar}
\title{Add a progress bar to a loop call, eg `for`, `while`, or `*apply`}
\usage{
progressBar(i, n, length = 25)
}
\arguments{
\item{i}{iterator, feeding in from the loop call, should be different for each iteration}

\item{n}{the total number of iterations in the call}

\item{length}{the character length of the progress bar. 25 works quite well}
}
\description{
Add a progress bar to a loop call, eg `for`, `while`, or `*apply`
}
