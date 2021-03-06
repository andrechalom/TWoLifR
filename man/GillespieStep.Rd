% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utils.R
\name{GillespieStep}
\alias{GillespieStep}
\title{Gillespie algorithm}
\usage{
GillespieStep(landscape)
}
\arguments{
\item{landscape}{a landscape object}
}
\description{
GillespieStep implements a Gillespie algorithm for advancing time in the simulation.
It changes the objects in the Landscape as side-effect, and returns the total elapsed time.
A common usage for it is: \code{while(L$clock < maxtime) print(NGillespieStep(L))}
}
\details{
This function uses some optimization based on how exponential random variables work.
}

