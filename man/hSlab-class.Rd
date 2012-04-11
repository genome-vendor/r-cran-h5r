\name{hSlab-class}
\Rdversion{1.1}
\docType{class}
\alias{hSlab-class}
\alias{[,H5Dataset,hSlab,missing,missing-method}
\alias{hSlab}
\alias{[,H5Dataset,hSlab,missing,missing-method}
\alias{start,hSlab-method}
\alias{width,hSlab-method}
\alias{length,hSlab-method}

\title{Class \code{"hSlab"}}
\description{
  A class used to represent hyper slabs. This class is often not as
  useful as directly using integers due to the performance cost of S4
  class construction.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("hSlab", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{s}:}{Object of class \code{"integer"} representing the
      start of the extents. There will be exactly n points where n
      represents the number of dimensions of the selection.}
    \item{\code{w}:}{Object of class \code{"integer"} representing the
      extent of the selection. This will be the same length as 's'}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "H5Dataset", i = "hSlab", j = "missing", drop = "missing")}: ... }
    \item{length}{\code{signature(x = "hSlab")}: ... }
  }
}
\examples{
  showClass("hSlab")
}
\keyword{classes}
