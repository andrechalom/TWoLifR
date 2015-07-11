# TWoLifR - The Walk of Life, on R
An R library For Agent-Based Simulations of Demography in Heterogeneus Landscapes

The TWoLife project aims to provide an implementation of the Gillespie algorithm for
stochastic population dynamics in continuous time. To install, open R and run
```R
library(devtools)
install_github("andrechalom/TWoLifR")
```

## TWoLifR implementation details

This R implementation of an individual based model depends heavily on the use of R
environments, as they are among the only R objects that allow for a pass-by-reference 
syntax. It is strongly advised to read Hadley Wickham's chapter on [Environments] 
(http://adv-r.had.co.nz/Environments.html) before working with this package. The
classes `Landscape`, `Species`, `Individual` and linked lists are all implemented
as R environments.
