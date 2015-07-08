#' Creates a new individual for the simulation
#' 
#' @param species from which \code{\link{Species}} this individual is
#' @param x,y initial position
#' @param \dots further arguments for plotting
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
  ind <- apply.bc(ind)# applying the boundary condition MIGHT kill the individual, so it returns NULL
  if (!is.null(ind)) setrates(ind)
  return(ind) 
}

# internal: sets the vital rates
setrates <- function (x) {
  if (whereami(x) == 0) { # on the matrix
    x$real.birth = 0
    x$real.death = x$species$matrix.death * x$species$death.rate
  } else { # on habitat
    x$real.birth = x$species$birth.rate
    x$real.death = x$species$death.rate
  }
  x$sumrates = x$species$move.rate + x$real.birth + x$real.death
}

#' @export
#' @param addradius logical. set to TRUE to draw the individual perception radius
#' @rdname Individual
plot.individual <- function(x, addradius = FALSE, ...) {
  points(x$x, x$y, ...)
  if (addradius) plotrix::draw.circle(x$x, x$y, x$species$radius, border="darkorange", lty=2)
}

#' @export
print.individual <- function(x, ...) {
  cat(paste0("Individual number ",x$id," in ", round(x$x,2), ", ", round(x$y,2), "\n"))
}

# internal: applies boundary condition to the individual position
apply.bc <- function(x) {
  bc <- x$species$landscape$bound.condition
  size <- x$species$landscape$numb.cells/2
  if (bc == "absorptive") {
    if (abs(x$x) > size | abs(x$y) > size) {
      die(x); return(NULL)
    }
    # TODO: other boundaries
  }
  return(x)
}

# moves
move <- function(x) {
  # steps
  angle <- x$species$visual.angle
  x$orientation = x$orientation + runif(1, -angle/2, angle/2)
  x$x = x$x + cos(x$orientation) * x$species$step;
  x$y = x$y + sin(x$orientation) * x$species$step;
  # apply boundary condition 
  # WARNING: may kill individual!
  apply.bc(x)
  # update neighborhood (self and others!!)
  # updates vital rates
  setrates(x)
}

# finds the index of x in the list l
whoami <- function(x, l) {
  f <- function(i) identical(i, x)
  which(sapply(l, f))
}

# deletes the individual from the population
die <- function(x) {
  # removes x from neighborhoods ### TODO
  # removes x from population
  x$species$population[[ whoami(x, x$species$population) ]] <- NULL
}

# non-sexual reproduction
clone <- function(x) { 
  new <- Individual(x$species, x$x, x$y)
  # adds new to neighborhoods
}

# internal: acts in one of three ways (die, clone or move) according to vital rates
act <- function(x) {
  event <- runif(1,0,x$sumrates)
  if (event < x$real.death) return(die(x))
  if (event < x$real.death + x$real.birth) return(clone(x))
  move(x)
}

# returns the habitat information for the individual
whereami <- function(x) {
  corn <- x$species$landscape$numb.cells/2 + 0.5
  x$species$landscape$scape[ round(x$x + corn), round(x$y + corn)]
}
