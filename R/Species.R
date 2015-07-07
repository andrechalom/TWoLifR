#' Creates a new species
#' 
#' Currently, every simulation contains a single species that must be created using this function.
#' @param birth.rate the intrinsic birth rate of the species
#' @param death.rate the intrinsic death rate of the species
#' @param max.density the support capacity for this species
#' @param radius the perception radius of the species.
#' @param incl.birth the slope in which the birth rate decreases with increasing density
#' @param incl.death the slope in which the death rate increases with increasing density
#' @param matrix.death a constant multiplying the death rate in non-habitat patches
#' @param move.rate the intrinsic movement rate of the species
#' @param step the length of the step for each movement
#' @param visual.angle currently, controls how much the movement orientation changes between steps
#' @export
Species <- function(birth.rate = 1, death.rate = 0.1, max.density = 10, incl.birth = 500, incl.death = 0,
                    radius = 1, matrix.death = 10, move.rate = 1, step = 1, visual.angle = 45) {
  sp <- new.env()
  sp$birth.rate = birth.rate; sp$death.rate = death.rate; sp$max.density = max.density
  sp$incl.birth = incl.birth; sp$incl.death = incl.death; sp$radius = radius; sp$matrix.death = matrix.death
  sp$move.rate = move.rate; sp$step = step; sp$visual.angle = visual.angle
  class(sp) <- "species"
  return(sp)
}
