#' Creates a new individual for the simulation
#' 
#' @param landscape in which \code{\link{Landscape}} this individual lives
#' @param species from which \code{\link{Species}} this individual is
#' @param x,y initial position
#' @section WARNING
#' Every time the function Individual is called, a new individual is created
#' and pushed to the landscape population.
#' @export
Individual <- function(landscape, species, x = 0, y = 0) {
  if (class(landscape) != "landscape")
    stop ("landscape must be of class landscape!")
  if (class(species) != "species")
    stop ("species must be of class species!")
  landscape$maxid = landscape$maxid + 1
  ind <- new.env()
  ind$landscape <- landscape;
  ind$species <- species;
  ind$x=x; ind$y=y; ind$id = l$maxid
  class(ind) <- "individual"
  landscape$population <- c(landscape$population, ind)
  return(ind)
}

plot.individual <- function(x, ...) {
  points(x$x, x$y, col=2, ...)
}

print.individual <- function(x, ...) {
  cat(paste0("Individual number ",x$id," in ", round(x$x,2), ", ", round(x$y,2), "\n"))
}
