#' Landscape generators
#'
#' This function generates a landscape object, which represent a fragmented environment in which
#' the simulations will run. 
#' @param numb.cells integer. Represents both the lenght AND width of the landscape, so numb.cells=100 creates a 100x100 landscape
#' @param type character. Currently, three types of habitat fragmentation routines are implemented: random fragmentation (noise), contiguous blob removal (random walk removal) and Fahrig fragmentation.
#' @param land.shape,bound.condition character. Control whether the shape of the land should be treated as a square (default) or a circle inscribed in this square; and what is the boundary condition. These parameters have no effect on the actual landscape generated, but will be used passed to the simulation.
#' @param cover numerical, between 0 and 1. What fraction of the landscape should consist on habitat?
#' @param frag recuperation parameter for the Fahrig method. Is ignored for other types. Higher values 
#' are close to random noise, while lower values result in more realistic landshapes.
#' @examples
#' plot(Landscape(cover=0.7, type="f", frag=0.01))
#' @export
#' @import stats
Landscape <- function (numb.cells = 100, land.shape = c("square","circle"),
                       type=c("random","blob","fahrig"), 
                       bound.condition=c("absorptive", "periodical", "reflexive"), cover=1,frag=NULL) {
	type=match.arg(type)
  land.shape=match.arg(land.shape)
  bound.condition=match.arg(bound.condition)
	if(cover < 0 || cover > 1) 
		stop("Error creating landscape. Cover must be between 0 and 1")
  if (type=="fahrig" && is.null(frag))
    stop("frag parameter must be specified for fahrig fragmentation!")
	# scape represents the actual landscape
	scape <- rep(1, numb.cells*numb.cells)
	if(cover < 1) {
		NtoRemove=round((1-cover)*numb.cells*numb.cells);
		if(type=="random") {
			while(NtoRemove>0)
			{
				i=round(runif(1,0,numb.cells-1));
				j=round(runif(1,0,numb.cells-1));
				# tests to see if this point has already been removed
				if(scape[1+numb.cells*j+i] == 1) {
					NtoRemove = NtoRemove - 1
					scape[1+numb.cells*j+i] = 0
				}
			}
		}
		if(type=="blob") {
			i=round(runif(1,0,numb.cells-1));
			j=round(runif(1,0,numb.cells-1));
			while(NtoRemove>0)
			{
				# tests to see if this point has already been removed
				if(scape[1+numb.cells*j+i] == 1) {
					NtoRemove = NtoRemove - 1
					scape[1+numb.cells*j+i] = 0
				}
				# Draft a new point to be removed (random walk!)
				if(sample(1:2,1) == 1) {
					i = i + (-1)**sample(1:2,1)
				} else {
					j = j + (-1)**sample(1:2,1)
				}
				if(i == -1) { i=numb.cells-1}
				if(i == numb.cells) { i=1}
				if(j == -1) { j=numb.cells-1}
				if(j == numb.cells) { j=1}
			}
		}
    if(type=="fahrig")
    {
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
      scape=as.numeric(scape)
    }
	}
  land <- new.env()
  land$numb.cells <- numb.cells;  land$land.shape <- land.shape;
  land$cover <- cover;
  land$type <- type; land$bound.condition <- bound.condition; land$scape <- scape
  land$maxid <- 0; land$population <- list()
	class(land) <- "landscape"
	return(land)
}

print.landscape <- function(x, ...) {
  cat(paste0("Landscape object.\n  Shape ", x$land.shape, ", type ", x$type, ", boundary ", x$bound.condition,
             ", height/width ", x$numb.cells, " habitat cover ", round(100*x$cover),"%", 
             "\n  Population: ", length(x$population), " individuals\n"))
}

#' @param col1 habitat color
#' @param col2 non-habitat color
#' @export
#' @import graphics
#' @rdname Landscape
plot.landscape <- function(x, col1="darkgreen", col2="grey70", ...) {
 	n = x$numb.cells
 	s <- seq(-n/2, n/2, length=n) # creates the x- and y- sequences for image
 	if (sum(x$scape) == n*n) { 
 		color = col1
 	} else {
 		color = c(col2, col1)
 	}
 	image(s, s, matrix(x$scape,ncol=n), col=color, ...)
}
