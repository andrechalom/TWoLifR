#' Creates a new species
#' 
#' Currently, every simulation contains a single species that must be created using this function.
#' @param landscape the \code{\link{Landscape}} in which this species lives
#' @param birth.rate the intrinsic birth rate of the species
#' @param death.rate the intrinsic death rate of the species
#' @param radius the perception radius of the species.
#' @param incl.birth the slope in which the birth rate decreases with increasing density
#' @param incl.death the slope in which the death rate increases with increasing density
#' @param matrix.death a constant multiplying the death rate in non-habitat patches (Currently unimplemented!)
#' @param move.rate the intrinsic movement rate of the species
#' @param step the length of the step for each movement
#' @param visual.angle currently, controls how much the movement orientation changes between steps
#' @section WARNING:
#'
#' Every time the function Species is called, a new species is created
#' and pushed to the landscape's species list.
#' @export
Species <- function(landscape, birth.rate = 1, death.rate = 0.1,  
                    incl.birth = 500, incl.death = 0,
                    radius = 1, matrix.death = 10, move.rate = 1, step = 1, visual.angle = base::pi/4) {
  if (class(landscape) != "landscape")
    stop ("landscape must be of class landscape!")
  sp <- new.env()
  sp$birth.rate = birth.rate; sp$death.rate = death.rate; 
  sp$incl.birth = incl.birth; sp$incl.death = incl.death; sp$radius = radius; sp$matrix.death = matrix.death
  sp$move.rate = move.rate; sp$step = step; sp$visual.angle = visual.angle
  if (incl.birth != 0 | incl.death != 0) {
   sp$max.density = (birth.rate - death.rate)/(incl.birth + incl.death)
   sp$birth.death.eq = death.rate + incl.death*((birth.rate - death.rate) / (incl.birth + incl.death))
  }
  # prepares the population list
  sp$maxid <- 0; sp$population <- list()
  # adds itself to the landscape list
  landscape$maxsp = landscape$maxsp + 1
  sp$id = landscape$maxsp
  sp$landscape = landscape
  class(sp) <- "species"
  landscape$specieslist <- c(landscape$specieslist, sp)
  return(sp)
}

#' @export
print.species <- function(x, ...) {
  cat(paste0("Species ", x$id, "; population ", length(x$population), "\n"))
}

#' @export
#' @import graphics
plot.species <- function(x, ...) {
  lapply(x$population, plot, col=x$id, ...)
}
