\name{readPoints}
\alias{readPoints}
\title{
  Prints Specified Data Points from the Selected H5 Dataset
}
\description{
  'readPoints' is a function that prints user-specified points from an
  H5 dataset. These points may be selected either by directly specifying
  coordinate indexes, or by a sampling function.

  lksdflksjf

}
\usage{
readPoints(h5Dataset, idxs)
}
\arguments{
  \item{h5Dataset}{
  Specify H5 dataset.
}
  \item{idxs}{
  Specify coordinate indexes to be printed.
}
}
\value{
  The values returned depend on H5 dataset data. Values returned can be
  of type integer, double or character.
}
\seealso{
  \link{read1DSlabs}
}
\examples{
  require(h5r)
  h5 <- H5File("test.h5", 'w')
  d1 <- createH5Dataset(h5, "dataset", runif(100000))
  p <- readPoints(d1, ss <- sample(1:length(d1), size = 1000, replace = TRUE))
  head(p)
  all(p == d1[ss])
}
\keyword{datasets}

