% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/linkedList.R
\name{linkedList}
\alias{.apply}
\alias{.drop}
\alias{.map}
\alias{.node}
\alias{.push}
\alias{[[.linkedList}
\alias{as.list.linkedList}
\alias{as.vector.linkedList}
\alias{length.linkedList}
\alias{linkedList}
\alias{print.linkedList}
\title{Linked List manipulation functions}
\usage{
linkedList()

\method{print}{linkedList}(list)

\method{length}{linkedList}(list)

.node(content, points.to = NULL)

.push(list, content)

.drop(list, content)

.map(list, FUN, ...)

.apply(list, FUN, ...)

\method{as.list}{linkedList}(list)

as.vector.linkedList(list, mode)

\method{[[}{linkedList}(list, i)
}
\arguments{
\item{list}{an object of class linkedList}

\item{content}{the content for the node that's being generated}

\item{points.to}{next node in the list (if 'pushing', the old list head)}

\item{FUN}{function to be applied}

\item{mode}{optional character string specifying the mode of the vector created by
\code{as.vector()}. If left blank, mode will be inferred from the first element.}

\item{i}{position in the list that should be returned. Note that the last element pushed is the element 1!}

\item{\dots}{additional arguments to be passed to FUN}
}
\description{
\code{linkedList()} creates an empty linked list object.

\code{.node()} creates a node with specified content

\code{.push()} adds a new element to a list, and returns the larger list

\code{.drop()} removes the first occurrence of 'content' in a list and returns the resulting list.
\code{.drop()} compares elements using the function \code{identical}.

Both \code{.map()} and \code{.apply()} apply a function over all the elements of a list.
Their difference is that .apply returns a numerical vector with the same length as the linkedList,
while .map simply applies a function and does not collect any result.

\code{as.list()} creates a list with the content of each node, and
\code{as.vector()} creates a similar vector, with mode inferred from the first element.
Notice that converting to vector might incur type errors, so use with caution.

The [[ ]] operator is provided for accessing the content of the i-th element
in a linked list, but notice that this is a time consuming operation!
}
\section{Details}{


A linked list is a data structure that's specially suitable for insertion/deletion
of objects in its middle. Deleting an element in the middle of a vector causes the
rest of the vector to be shifted, which can be a performance issue if this operation
is carried over many times. In a linked list, on the other hand, deleting an element
in the middle is a constant-time operation. The main disadvantage of using linked lists
comes from the fact that vectors are optimized for random access - in other words,
x[25] is a very fast operation for a vector, but may take a long time in a linked list.

The current implementation is VERY minimalistic, as it strives for being a reasonably
efficient replacement for the base "list" type in the specific context of a population
that frequently loses members. Also note that environment R objects are large, so these
functions may lead to heavier memory usage in comparison with standard vectors or lists
(in the order of kilobytes per list item).
}

