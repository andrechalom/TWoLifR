#' Creates a new individual for the simulation
#' 
#' @param landscape in which \code{\link{Landscape}} this individual lives
#' @param species from which \code{\link{Species}} this individual is
#' @param x,y initial position
#' @export
Individual <- function(landscape, species, x = 0, y = 0) {
  if (class(landscape) != "landscape")
    stop ("landscape must be of class landscape!")
  if (class(species) != "species")
    stop ("species must be of class species!")
  ind <- list(lanscape=landscape, species=species, x=x, y=y)
  class(ind) <- "individual"
  return(ind)
}

plot.individual <- function(x, ...) {
  points(x$x, x$y, col=2, ...)
}
