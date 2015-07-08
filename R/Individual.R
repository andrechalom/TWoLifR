#' Creates a new individual for the simulation
#' 
#' @param species from which \code{\link{Species}} this individual is
#' @param x,y initial position
#' @section WARNING:
#'
#' Every time the function Individual is called, a new individual is created
#' and pushed to the species population.
#' @export
Individual <- function(species, x = 0, y = 0) {
  if (class(species) != "species")
    stop ("species must be of class species!")
  species$maxid = species$maxid + 1
  ind <- new.env()
  ind$species <- species;
  ind$x=x; ind$y=y; ind$id = species$maxid
  ind$orientation = runif(1, 0, 2*base::pi)
  class(ind) <- "individual"
  species$population <- c(species$population, ind)
  return(ind)
}

#' @export
plot.individual <- function(x, ...) {
  points(x$x, x$y, ...)
}

#' @export
print.individual <- function(x, ...) {
  cat(paste0("Individual number ",x$id," in ", round(x$x,2), ", ", round(x$y,2), "\n"))
}

#' @export
move <- function(x) {
  angle <- x$species$visual.angle
  x$orientation = x$orientation + runif(1, -angle/2, angle/2)
  x$x = x$x + cos(x$orientation) * x$species$step;
  x$y = x$y + sin(x$orientation) * x$species$step;
}
