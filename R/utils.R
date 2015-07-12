#' Distance calculation
#'
#' dist calculates the distance between two individuals, and should be used by the end user for
#' exploring simulation results.
#'
#' sqDist calculates the squared distance between two individuals. 
#' For most application (such as determining neighbors) knowing the square distance is enough, 
#' and square roots are computationally expensive. Also, this function does not check for obvious errors
#' (such as the parameters not being individuals, or being individuals from different landscapes),
#' as it is designed to be as efficient as possible. 
#' @param a1,a2 individuals
#' @examples
#' L <- Landscape(bound.condition="p", numb.cells = 50) # x ranges from -25 to 25
#' S <- Species(L)
#' a1 <- Individual(S, 25, 25)
#' a2 <- Individual(S, -20, -20)
#' sqDist(a1, a2)
#' @export
dist <- function(a1, a2) {
  if(class(a1) != "individual" | class(a2) != "individual")
    stop("a1 and a2 must be individuals")
  if(! identical(a1$species$landscape, a2$species$landscape))
    stop("Trying to compare individuals from different landscapes!")
  sqrt(sqDist(a1, a2))
}

#' @export
#' @rdname dist
sqDist <- function(a1, a2) {
  bc <- a1$bc
  if (bc == "periodical") {
    x1=a1$x; x2=a2$x; y1=a1$y; y2=a2$y; l = a1$landscape$numb.cells
    if (x1 > x2) dx = x1-x2 else dx = x2-x1;
    if (y1 > y2) dy = y1-y2 else dy = y2-y1;
    if (dx > l - dx) dx = l - dx; 
    if (dy > l - dy) dy = l - dy;
    return (dx*dx + dy*dy)
  } else {
    x1=a1$x; x2=a2$x; y1=a1$y; y2=a2$y;
    return ( (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) )
  }
}

#' Gillespie algorithm
#' 
#' GillespieStep implements a Gillespie algorithm for advancing time in the simulation. 
#' It changes the objects in the Landscape as side-effect, and returns the total elapsed time.
#' A common usage for it is: \code{while(L$clock < maxtime) print(NGillespieStep(L))}
#'
#' This function uses some optimization based on how exponential random variables work.
#' @export
#' @param landscape a landscape object
GillespieStep <- function(landscape) {
  if (class(landscape) != "landscape")
    stop ("landscape must be of class landscape!")
  if (length(landscape$specieslist) > 1) 
    stop ("This algorithm is currently implemented for one species only")
  rates <- .apply(landscape$specieslist[[1]]$population, function(i) i$sumrates)
  ind <- sample(1:length(rates), 1, prob=rates)
  time <- rexp(1, sum(rates))
  # increments the world clock
  landscape$clock = landscape$clock + time
  # makes the chosen individual act
  cat("DEBUGS: ", landscape$specieslist[[1]]$population[[ind]]$id, "\n")
  act(landscape$specieslist[[1]]$population[[ind]])
  return(landscape$clock)
}
#' Simulation routines
#' 
#' Set up a new \code{\link{Landscape}} with preset Species and Individiduals, and run a Gillespie
#' algorithm until a final time is reached. \code{runSSim} runs a single-species simulation and passes
#' all parameters as named to the Landscape or Species constructor.
#' 
#' Exponential and Logistic are just wrappers for runSSim that enforce that the model is a simple 
#' exponential / logistic growth.
#' @param maxtime Maximum simulation running time
#' @param N initial population size
#' @param \dots further arguments that will be passed to Landscape or Species. All options specified
#' on \code{\dots} and not used for these functions will be passed to \code{\link{populate}}
#' @export
runSSim <- function(maxtime, N, ...) {
  dots <- list(...)
  # Create a Landscape
  lopts <- names(dots) %in% names(formals(Landscape))
  L <- do.call(Landscape, dots[lopts])
  sopts <- names(dots) %in% names(formals(Species))
  S <- do.call(Species, c(list(landscape=L), dots[sopts]))
  do.call(populate, c(list(species=S, N=N), dots[!lopts && !sopts]))
  pop.over.time <- total.N(L)

  while(total.N(L) & L$clock < maxtime) {
    oldtime = L$clock
    GillespieStep(L)
    if (round(oldtime) != round(L$clock))
      pop.over.time <- c(pop.over.time, total.N(L))
  }

  pop.over.time <- c(pop.over.time, total.N(L))
  return(list(Landscape=L, pop.over.time=pop.over.time))
}

#' @export
#' @rdname runSSim
Exponential <- function(maxtime, N, ...) {
  dots <- modifyList(list(...), list(numb.cells=1, cover=1, radius=0, incl.birth=0, incl.death=0, move.rate=0))
  do.call(runSSim, c(list(maxtime=maxtime, N=N), dots))
}

#' @export 
#' @rdname runSSim
Logistic <- function(maxtime, N, ...) {
  dots <- modifyList(list(...), list(numb.cells=1, cover=1, move.rate=0))
  do.call(runSSim, c(list(maxtime=maxtime, N=N), dots))
}
