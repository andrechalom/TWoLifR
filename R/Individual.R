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
  ind <- new.env()
  # position and movement direction
  ind$x=x; ind$y=y; 
  ind$orientation = runif(1, 0, 2*base::pi)
  # unique identifier
  ind$species <- species;
  species$maxid = species$maxid + 1
  ind$id = species$maxid
  ind$neighbors <- linkedList() # list will be constructed by add.neighbors below
  # "caching" as these are used frequently
  ind$landscape = species$landscape
  # "corn" is a correction added to the x,y positions to translate them into
  # indexes for the landscape matrix
  ind$corn <- ind$landscape$numb.cells/2 + 0.5 
  # squared radius of neighbourhood
  ind$rad2 <- species$radius^2
  # rates
  ind$matrix.death = species$matrix.death;
  ind$death.rate = species$death.rate; ind$incl.death = species$incl.death
  ind$birth.rate = species$birth.rate; ind$incl.birth = species$incl.birth
  ind$move.rate = species$move.rate
  # boudary condition
  ind$bc = ind$landscape$bound.condition

  class(ind) <- "individual"
  species$population <- .push(species$population, ind)
  ind <- apply.bc(ind)# applying the boundary condition MIGHT kill the individual, so it returns NULL
  if (!is.null(ind)) {add.neighbors(ind); setrates(ind)}
  return(ind) 
}

# adds itself to other neighborhoods, and adds others into x neighborhood
add.neighbors <- function(x) {
  rad2 = x$rad2
  x$neighbors <- linkedList() # starts afresh
  if (rad2 == 0) return(); # no neighbors can be detected
  .map(x$species$population, function(other) {
    if (! identical(x, other) && sqDist(x, other) < rad2) {
      x$neighbors <- .push(x$neighbors, other)
      other$neighbors <- .push(other$neighbors, x)
      setrates(other)
    }
  })
}

# drops itself from other neighborhoods (before dying or moving)
drop.neighbors <- function(x) {
  if (length(x$neighbors)) #is this check necessary??
    .map(x$neighbors, function(other) {
      other$neighbors <- .drop(other$neighbors, x)
      setrates(other)
    })
  x$neighbors <- linkedList() # empties own neigh list
}

# internal: sets the vital rates
setrates <- function (x) {
  if(x$species$radius)
    density <- length(x$neighbors) / (base::pi * x$species$radius^2)
  else
    density <- 0
  if (whereami(x) == 0) { # on the matrix
    x$real.birth = 0
    x$real.death = x$matrix.death * x$death.rate + x$incl.death * density
  } else { # on habitat
    x$real.birth = x$birth.rate - x$incl.birth * density
    x$real.death = x$death.rate + x$incl.death * density
    if(x$real.birth < 0) x$real.birth = 0
  }
  x$sumrates = x$move.rate + x$real.birth + x$real.death
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
  cat(paste0("Individual number ",x$id," in ", round(x$x,2), ", ", round(x$y,2), 
             " neighbors: ", length(x$neighbors), "\n"))
}

# internal: applies boundary condition to the individual position
apply.bc <- function(x) {
  size <- x$landscape$numb.cells/2
  if (x$bc == "absorptive") {
    if (abs(x$x) > size | abs(x$y) > size) {
      die(x); return(NULL)
    }
    # TODO: other boundaries
  }
  return(x)
}

# moves
move <- function(x) {
  # drops itself from other neighborhoods 
  drop.neighbors(x)
  # steps
  angle <- x$species$visual.angle
  x$orientation = x$orientation + runif(1, -angle/2, angle/2)
  x$x = x$x + cos(x$orientation) * x$species$step;
  x$y = x$y + sin(x$orientation) * x$species$step;
  # apply boundary condition 
  # WARNING: may kill individual!
  if(!is.null(apply.bc(x))) {
    # adds itself to neighborhoods:
    add.neighbors(x)
    # updates vital rates
    setrates(x)
  }
}

# deletes the individual from the population
die <- function(x) {
  # removes x from neighborhoods
  drop.neighbors(x)
  # removes x from population
  x$species$population <- .drop(x$species$population, x)
}

# non-sexual reproduction
clone <- function(x) { 
  # the 'Individual' function will set the vital rates and adds 'new' to neighborhoods
  Individual(x$species, x$x, x$y)
}

# internal: acts in one of three ways (die, clone or move) according to vital rates
act <- function(x) {
  event <- runif(1,0,x$sumrates)
  if (event < x$real.death) return(die(x))
  if (event < x$real.death + x$real.birth) return(clone(x))
  move(x)
}

# returns the habitat information for the individual (0 = matrix, 1 = habitat)
whereami <- function(x) {
  j = round(x$y + x$corn); i = round(x$x + x$corn)
  x$landscape$scape[i,j]
}
