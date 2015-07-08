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
  bc <- a1$species$landscape$bound.condition
  if (bc == "periodical") {
    x1=a1$x; x2=a2$x; y1=a1$y; y2=a2$y; l = a1$species$landscape$numb.cells
    if (x1 > x2) dx = x1-x2 else dx = x2-x1;
    if (y1 > y2) dy = y1-y2 else dy = y2-y1;
    if (dx > l - dx) dx = l - dx; 
    if (dy > l - dy) dy = l - dy;
    return (dx*dx + dy*dy)
  } else {
    return ( (a1$x-a2$x)*(a1$x-a2$x) + (a1$y-a2$y)*(a1$y-a2$y) )
  }
}

#' Gillespie algorithm
#' 
#' NGillespieStep implements a naive Gillespie algorithm for advancing time in the simulation. 
#' It changes the objects in the Landscape as side-effect, and returns the total elapsed time.
#' A common usage for it is: \code{while(L$clock < maxtime) print(NGillespieStep(L))}
#' @export
NGillespieStep <- function(landscape) {
  if (class(landscape) != "landscape")
    stop ("landscape must be of class landscape!")
  sp <- 0; ind <- 0; time <- Inf
  for (i in 1:length(landscape$specieslist)) {
    pop <- landscape$specieslist[[i]]$population
    if(length(pop) > 0 ) for (j in 1:length(pop)) {
      draw = rexp(1, rate=pop[[j]]$sumrates)
      if (draw < time) {
        sp = i; ind = j; time = draw
      }
    }
  }
  # increments the world clock
  landscape$clock = landscape$clock + time
  # makes the chosen individual act
  act(landscape$specieslist[[sp]]$population[[ind]])
  return(landscape$clock)
}
