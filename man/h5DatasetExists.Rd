\name{h5DatasetExists}
\alias{h5DatasetExists}
\title{
  Check whether a Dataset Exists
}
\description{
  'h5DatasetExists' is a function that searches for a dataset of the
  specified name. The function returns a TRUE or FALSE statement.
}
\usage{
  h5DatasetExists(h5Obj, name)
}
\arguments{
  \item{h5Obj}{
    H5 object to which the dataset is attached.
}
  \item{name}{
     Name of attribute.
}
}
\value{
  The object returned is of type logical.
}
\examples{
  require(h5r)
  h5 <- H5File("test.h5", 'w')
  a <- c(rnorm(100), rnorm(102)) 
  d1 <- createH5Dataset(h5, "new", a)
  d.e <- h5DatasetExists(h5, "new")
  d.e
}
\keyword{datasets}
