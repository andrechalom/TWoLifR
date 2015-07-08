#' Squared distance
#'
#' Calculates the squared distance between two individuals. If you need the actual distance,
#' call \code{sqrt(sqDist(a,b))}, but for most application (such as determining neighbors)
#' knowing the square distance is enough, and square roots are computationally expensive.
#' @param a,b individuals
#' @export
sqDist <- function(a1, a2) {
  bc <- a1$species$landscape$bound.condition
  if (bc == "periodical") {
  x1=a1$x; x2=a2$x; y1=a1$y; y2=a2$y; l = a$species$landscape$numb.cells
  if (x1 > x2) dx = x1-x2 else dx = x2-x1;
  if (y1 > y2) dy = y1-y2 else dy = y2-y1;
  if (dx > l - dx) dx = l - dx; 
  if (dy > l - dy) dy = l - dy;
  return (dx*dx + dy*dy)
  } else {
    return ( (a1$x-a2$x)*(a1$x-a2$x) + (a1$y-a2$y)*(a1$y-a2$y) )
  }
}
