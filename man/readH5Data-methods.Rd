\name{readH5Data-methods}
\alias{readH5Data}
\alias{readH5Data-methods}
\alias{readH5Data,H5Attribute-method}
\alias{readH5Data,H5Dataset-method}
\title{Read H5 Data}
\description{
  Low-level function for reading data from an H5 File.
}
\usage{
  \S4method{readH5Data}{H5Attribute}(h5Obj)
  \S4method{readH5Data}{H5Dataset}(h5Obj)
}
\arguments{
  \item{h5Obj}{
    An H5Object which stores data.
  }
}
\section{Methods}{
  \describe{
    \item{\code{signature(h5Obj = "H5Attribute")}}{
    }
    \item{\code{signature(h5Obj = "H5Dataset")}}{
    }
}}
\examples{
  require(h5r)
  h5 <- H5File("test.H5",'w')
  m <- createH5Dataset(h5, "mm", cbind(rnorm(1000), rnorm(1000)))
  readH5Data(m)
}
\keyword{methods}
