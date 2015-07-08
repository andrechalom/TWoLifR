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
  ind$sumrates = species$move.rate + species$birth.rate + species$death.rate ###### TODO!!!!!!
  ind$real.birth = species$birth.rate; ind$real.death = species$death.rate
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
  # steps
  angle <- x$species$visual.angle
  x$orientation = x$orientation + runif(1, -angle/2, angle/2)
  x$x = x$x + cos(x$orientation) * x$species$step;
  x$y = x$y + sin(x$orientation) * x$species$step;
  # apply boundary condition 
  # WARNING: may kill individual!
  bc <- x$species$landscape$bound.condition
  size <- x$species$landscape$numb.cells/2
  if (bc == "absorptive") {
    if (abs(x$x) > size | abs(x$y) > size) {
      die(x); return()
    }
    # TODO: other boundaries
  }
  # update neighborhood (self and others!!) and vital rates
  # updates habitat information?
}

# finds the index of x in the list l
whoami <- function(x, l) {
  f <- function(i) identical(i, x)
  which(sapply(l, f))
}

#' @export
die <- function(x) {
  # removes x from neighborhoods ### TODO
  # removes x from population
  x$species$population[[ whoami(x, x$species$population) ]] <- NULL
}

#' @export
clone <- function(x) { # non-sexual reproduction
  new <- Individual(x$species, x$x, x$y)
  # adds new to neighborhoods
}

#' @export
act <- function(x) {
  event <- runif(1,0,x$sumrates)
  if (event < x$real.death) return(die(x))
  if (event < x$real.death + x$real.birth) return(clone(x))
  move(x)
}
