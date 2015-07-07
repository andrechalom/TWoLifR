#' Accessors methods for class Landscape
#' 
#' Use these functions to access the status of a landscape or its enclosed population
#' @param scape The landscape
#' @rdname Landscape-accessors
#' @export
population <- function(scape) {
  return (scape$population)
}

#' @rdname Landscape-accessors
#' @export
popSize <- function(scape) {
  return (length(scape$population))
}
