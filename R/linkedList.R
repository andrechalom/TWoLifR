#' Linked List manipulation functions
#'
#' \code{linkedList()} creates an empty linked list object.
#'
#' @section Details:
#'
#' A linked list is a data structure that's specially suitable for insertion/deletion
#' of objects in its middle. Deleting an element in the middle of a vector causes the
#' rest of the vector to be shifted, which can be a performance issue if this operation
#' is carried over many times. In a linked list, on the other hand, deleting an element
#' in the middle is a constant-time operation. The main disadvantage of using linked lists
#' comes from the fact that vectors are optimized for random access - in other words,
#' x[25] is a very fast operation for a vector, but may take a long time in a linked list.
#' 
#' The current implementation is VERY minimalistic, as it strives for being a reasonably
#' efficient replacement for the base "list" type in the specific context of a population
#' that frequently loses members. Also note that environment R objects are large, so these
#' functions may lead to heavier memory usage in comparison with standard vectors or lists
#' (in the order of kilobytes per list item).
#'
#' @export
linkedList <- function () {
  # creates a new EMPTY node to be this list head
  a <- new.env(); class(a) = "linkedList"
  a$head = .node(NULL)
  a$length = 0
  return(a)
}

#' @param list an object of class linkedList
#' @rdname linkedList
#' @export
print.linkedList <- function (list) {
  cat (paste0("Linked list object; length ", list$length, "; head contains: \n"))
  print(list$head$content)
}

#' @rdname linkedList
#' @export
length.linkedList <- function(list) {
  list$length
}

#' \code{.node()} creates a node with specified content
#' @param content the content for the node that's being generated
#' @param points.to next node in the list (if 'pushing', the old list head)
#' @rdname linkedList
#' @export
.node <- function(content, points.to = NULL) {
  a <- new.env()
  class(a) = "node"
  a$content = content
  a$points.to = points.to
  return(a)
}

#' \code{.push()} adds a new element to a list, and returns the larger list
#' WARNING: this method modifies the first argument as a side effect
#' @rdname linkedList
#' @export
.push <- function(list, content) {
  # creates a new node for storing the content
  new <- .node(content, list$head)
  list$head = new
  list$length = list$length+1
  return(new)
}

#' \code{.drop()} removes the node passed as the second argument
#' WARNING: this method modifies the first argument as a side effect
#' @rdname linkedList
#' @export
.drop <- function(list, node) {
  node$content = node$points.to$content
  node$points.to = node$points.to$points.to
  list$length = list$length - 1
}

#' Both \code{.map()} and \code{.apply()} apply a function over all the elements of a list.
#' Their difference is that .apply returns a numerical vector with the same length as the linkedList,
#' while .map simply applies a function and does not collect any result.
#' @param FUN function to be applied
#' @param \dots additional arguments to be passed to FUN
#' @rdname linkedList
#' @export
.map <- function(list, FUN, ...) {
  n <- list$head
  dots <- list(...)
  while (!is.null(n$content)) {
    do.call(FUN, c(list(n$content), dots))
    n <- n$points.to
  }
}

#' @rdname linkedList
#' @export
.apply <- function(list, FUN, ...) {
  result <- vector(mode="numeric", length=list$length) # prealocates the resulting list
  n <- list$head; i = 1;
  dots <- list(...)
  while (!is.null(n$content)) {
    result[i] <- do.call(FUN, c(list(n$content), dots))
    n <- n$points.to; i = i+1
  }
  result
}

#' \code{as.list()} creates a list with the content of each node, and
#' \code{as.vector()} creates a similar vector, with mode inferred from the first element.
#' Notice that converting to vector might incur type errors, so use with caution.
#' @rdname linkedList
#' @export
as.list.linkedList <- function(list) {
  result <- vector(mode="list", length=list$length) # prealocates the resulting list
  n <- list$head
  for (i in 1:list$length) {# some smarter "apply" here would be helpful!
    result[[i]] <- n$content
    n <- n$points.to
  }
  return(result)
}

#' @param mode optional character string specifying the mode of the vector created by
#' \code{as.vector()}. If left blank, mode will be inferred from the first element.
#' @rdname linkedList
#' @export
as.vector.linkedList <- function(list, mode) {
  # prealocates the resulting vector
  if(mode == "any") mode <- mode(list$head$content)
  result <- vector(mode, length=list$length) 
  n <- list$head
  for (i in 1:list$length) {# some smarter "apply" here would be helpful!
    result[i] <- n$content
    n <- n$points.to
  }
  return(result)
}

#' The [[ ]] operator is provided for accessing the content of the i-th element 
#' in a linked list, but notice that this is a time consuming operation!
#' @param i position in the list that should be returned. Note that the last element pushed is the element 1!
#' @rdname linkedList
#' @export
'[[.linkedList' <- function(list, i) {
  if (i > list$length) {warning("Subscript out of bounds!"); return(NULL)}
  .get(list$head, i)
}

.get <- function(node, i) {
  if(i==1) return (node$content)
  .get(node$points.to, i-1)
}
