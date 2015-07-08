#' Landscape generators
#'
#' This function generates a landscape object, which represent a fragmented environment in which
#' the simulations will run. 
#' @param numb.cells integer. Represents both the lenght AND width of the landscape, so numb.cells=100 creates a 100x100 landscape
#' @param type character. Currently, three types of habitat fragmentation routines are implemented: random fragmentation (noise), contiguous blob removal (random walk removal) and Fahrig fragmentation. Also, you can specify "disc" to create a landscape that is close to a circular disc; in this case, the disc is fully made of habitat and the parameter cover is ignored.
#' @param bound.condition character. Control what is the boundary condition. This parameter have no effect on the actual landscape generated, but will be passed to the simulation.
#' @param cover numerical, between 0 and 1. What fraction of the landscape should consist on habitat?
#' @param frag fragmentation parameter for the Fahrig method. Is ignored for other types. Higher values 
#' are close to random noise, while lower values result in more realistic landshapes.
#' @param \dots further arguments for plotting functions
#' @examples
#' plot(Landscape(cover=0.7, type="f", frag=0.01))
#' @export
#' @import stats
Landscape <- function (numb.cells = 100, type=c("random","blob","fahrig","disc"), 
                       bound.condition=c("absorptive", "periodical", "reflexive"), cover=1,frag=NULL) {
	type=match.arg(type)
  bound.condition=match.arg(bound.condition)
	if(cover < 0 || cover > 1) 
		stop("Error creating landscape. Cover must be between 0 and 1")
  if (type=="fahrig" && is.null(frag))
    stop("frag parameter must be specified for fahrig fragmentation!")
	# scape represents the actual landscape
  if(type=="disc") {scape <- discLandscape(numb.cells); cover = base::pi/4}
  else if(cover == 1) scape <- matrix(1, numb.cells,numb.cells)
  else {
		if(type=="random") scape <- randomLandscape(numb.cells, cover)
		if(type=="blob") scape <- blobLandscape(numb.cells, cover)
    if(type=="fahrig") scape <- fahrigLandscape(numb.cells, cover, frag)
  }
  land <- new.env()
  land$numb.cells <- numb.cells; land$cover <- cover; land$clock <- 0;
  land$type <- type; land$bound.condition <- bound.condition; land$scape <- scape
  land$maxsp <- 0; land$specieslist <- list()
	class(land) <- "landscape"
	return(land)
}

#' @export
print.landscape <- function(x, ...) {
  cat(paste0("Landscape object.\n  Type ", x$type, ", boundary ", x$bound.condition,
             ", height/width ", x$numb.cells, " habitat cover ", round(100*x$cover),"%\n"))
  lapply(x$specieslist, print) -> ignoreResult
}

#' @param x a landscape object
#' @param col1 habitat color
#' @param col2 non-habitat color
#' @export
#' @import graphics
#' @rdname Landscape
plot.landscape <- function(x, col1="darkgreen", col2="grey70", ...) {
 	n = x$numb.cells
 	s <- seq(-(n-1)/2, (n-1)/2, length=n) # creates the x- and y- sequences for image
 	if (sum(x$scape) == n*n) { 
 		color = col1
 	} else {
 		color = c(col2, col1)
 	}
 	image(s, s, x$scape, col=color, ...)
  lapply(x$specieslist, plot) -> ignoreResult
}

#' @export
#' @rdname Landscape
#' @param landscape a landscape object
total.N <- function(landscape) {
  sum(sapply(landscape$specieslist, function(x) length(x$population)))
}

### Landscape generating functions
### Internal methods

randomLandscape <- function(numb.cells, cover) {
	scape <- matrix(1, numb.cells,numb.cells)
  NtoRemove=round((1-cover)*numb.cells*numb.cells);
  while(NtoRemove>0)
  {
    i=round(runif(1,1,numb.cells));
    j=round(runif(1,1,numb.cells));
    # tests to see if this point has already been removed
    if(scape[i,j] == 1) {
      NtoRemove = NtoRemove - 1
      scape[i,j] = 0
    }
  }
  return (scape)
}

blobLandscape <- function(numb.cells, cover) {
	scape <- matrix(1, numb.cells,numb.cells)
  NtoRemove=round((1-cover)*numb.cells*numb.cells);
  i=round(runif(1,1,numb.cells));
  j=round(runif(1,1,numb.cells));
  while(NtoRemove>0)
  {
    # tests to see if this point has already been removed
    if(scape[i,j] == 1) {
      NtoRemove = NtoRemove - 1
      scape[i,j] = 0
    }
    # Draft a new point to be removed (random walk!)
    if(sample(1:2,1) == 1) {
      i = i + (-1)**sample(1:2,1)
    } else {
      j = j + (-1)**sample(1:2,1)
    }
    if(i == 0) { i=numb.cells}
    if(i == numb.cells+1) { i=1}
    if(j == 0) { j=numb.cells}
    if(j == numb.cells+1) { j=1}
  }
  return(scape)
}

fahrigLandscape <- function(numb.cells, cover, frag) {
  scape=matrix(rep(0, numb.cells*numb.cells),numb.cells,numb.cells)
  while(sum(scape)<round(numb.cells*numb.cells*cover))
  {
    chosen.cell=sample(1:numb.cells,2,replace=T)
    neigh.cells=matrix(c(chosen.cell[1],chosen.cell[2]+1,chosen.cell[1]-1,chosen.cell[2],chosen.cell[1],
                         chosen.cell[2]-1,chosen.cell[1]+1,chosen.cell[2]),4,2,byrow=T)
    out.land=which(neigh.cells==numb.cells+1 | neigh.cells==0,arr.ind=T)[,1]
    n.neigh=1:4
    if(scape[chosen.cell[1],chosen.cell[2]]==0) 
    {
      rnd=runif(1,0,1)
      is.habitat=0
      if(length(out.land)>0)
      {
        for(i in n.neigh[-out.land])
        {
          is.habitat=is.habitat+scape[neigh.cells[i,1],neigh.cells[i,2]]
        }
      }
      else
      {
        for(i in n.neigh)
        {
          is.habitat=is.habitat+scape[neigh.cells[i,1],neigh.cells[i,2]]
        }
      }
      if(rnd<frag | is.habitat>0) 
      {
        scape[chosen.cell[1],chosen.cell[2]]=1
      }
    }  
  }
  return(scape)
}

discLandscape <- function(numb.cells) {
	scape <- matrix(1, numb.cells,numb.cells)
  for (i in 1:numb.cells)
    for (j in 1:numb.cells) 
      if(
         (i-.5-numb.cells/2)*(i-.5-numb.cells/2)+(j-.5-numb.cells/2)*(j-.5-numb.cells/2) >
         numb.cells*numb.cells/4
         )
        scape[i,j] = 0
  return(scape)
}
