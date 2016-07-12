# TWoLifR - The Walk of Life, on R

**NOTICE:** Development on TWoLifR has been discontinued, as the performance of this package does not allow its use in large scale applications. See the [TWoLife](https://github.com/piLaboratory/TWoLife) project for a better alternative.

An R library For Agent-Based Simulations of Demography in Heterogeneus Landscapes

The TWoLife project aims to provide an implementation of the Gillespie algorithm for
stochastic population dynamics in continuous time. To install, open R and run
```R
library(devtools)
install_github("andrechalom/TWoLifR")
```

## Basic usage

The first object that needs to be created is a `Landscape`. It defines the environment
in which the individuals will live, and consists essentially on a matrix of 1s and 0s
indicating where the habitat is favorable, and where it is not (matrix). See `?Landscape` 
for details on how the habitat is generated.
```R
L <- Landscape(type='fahrig', cover=0.4, numb.cells=50, frag=0.01)
print(L)
plot(L)
```

Notice that the x and y scales range from `-numb.cells/2` to `numb.cells/2`.

Next, you must create one or more species that will live in this environment. The `Species`
function creates a new species with specified vital rates, such as `birth.rate`, `death.rate`,
`move.rate` and movement `step` size:
```R
S <- Species(L, birth.rate = 1, death.rate = 0.1, move.rate = 1, matrix.death=9)
```

After that, you can create individuals on this species by using the `Individual` function,
or use the `populate` function to quickly create a series of individuals. This function
is able to place each individual in a position determined by a specified function, for example,
it may place 100 individuals uniformly distributed across the landscape:
```R
populate(S, 100, FUN=runif, min=-25, max=25)
```

Notice that the `Landscape` object keeps track of every species and individual that is created:
```R 
print(L)
plot(L)
```

Now, to simulate one step of the the population dynamics, call the function `GillespieStep`, as many times
as wanted. To check the landscape internal clock, access `L$clock`:
```R
while(L$clock < 10) GillespieStep(L)
print(L)
plot(L)
```

## Running full simulations
TWoLifR currently comes with some functions to automate running simulations, such as `runSSim`, that accept
all the arguments that can be passed to `Landscape`, `Species` and `populate`, and run a full simulation.
To run a simulation similar to the one we constructed above, run
```R
newsim <- runSSim(maxtime=10, N=50, type='fahrig', cover=0.4, 
    numb.cells=50, frag=0.01, birth.rate=1, death.rate=0.1, move.rate=1, 
    matrix.death=9, FUN=runif, min=-25, max=25)
```

The object generated contains the final state of the landscape, as well as a vector showing total
population over time:
```R
plot(newsim$L)
plot(newsim$pop.over.time)
```

## TWoLifR implementation details

This R implementation of an individual based model depends heavily on the use of R
environments, as they are among the only R objects that allow for a pass-by-reference 
syntax. It is strongly advised to read Hadley Wickham's chapter on [Environments] 
(http://adv-r.had.co.nz/Environments.html) before working with this package. The
classes `Landscape`, `Species`, `Individual` and linked lists are all implemented
as R environments.

The basic data structure used for keeping track of the individuals in a population is a 
[linked list] (https://en.wikipedia.org/wiki/Linked_list),
as it is much more efficient to insert and delete values in a specified position of a linked 
list than it is using vectors. A very basic (fully R) implementation of linked lists is provided
in this package; see `?linkedList` for details.

## To-do lists and issues

- Currently, the TWoLifR implementation is unable to run simulations with more than one species.
- Plot function could indicate the last individual that acted?
- Boundary conditions are not fully implemented (currently implemented: absorptive)
- Corner-cases and inhomogeneous habitat are not handled by the density calculations
- Analytics functions are not implemented
