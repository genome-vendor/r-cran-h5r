\name{h5ObjectExists}
\alias{h5ObjectExists}
\title{
  Check whether an Object Exists
}
\description{
  'h5ObjectExists' is a function that searches foran object of the
  specified name. The function returns a TRUE or a FALSE statement.
}
\usage{
  h5ObjectExists(h5Obj, name)
}
\arguments{
  \item{h5Obj}{
    H5 object in which the attribute is located.
}
  \item{name}{
    Name of group.
}
}
\value{
  The object returned is of type logical.
}
\examples{
  require(h5r)
  h5 <- H5File("test.h5", 'w')
  d1 <- createH5Dataset(h5,"new", c(rnorm(100),rnorm(90)))
  o.d <- h5ObjectExists(h5,"new")
  o.d
  g1 <- createH5Group(h5, "grp101")
  o.g <- h5ObjectExists(h5, "grp101")  
  o.g
}
\keyword{datasets}

