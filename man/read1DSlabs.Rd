\name{read1DSlabs}
\alias{read1DSlabs}
\title{
 A Function that Selects Specified Portions or Strings of Data from an
 H5 Dataset.
}
\description{
  'read1DSlabs' is a funtion that pulls out strings (or portions) of
  data, where the starting point and dimension of strings/portions can be
  specified by the user.  
}
\usage{
read1DSlabs(h5Dataset, offsets, dims)
}
\arguments{
  \item{h5Dataset}{
  Specify H5 dataset.
}
  \item{offsets}{
  Specify offsets i.e. the points at which the data strings (or data
  portions) begin. 
}
  \item{dims}{
  Specify dimensions of dataset string or portion.
}
}
\value{
  The values returned depend on H5 dataset data type.
}
\seealso{
  \link{readSlab}
}
\examples{
  file <- system.file("h5_files", "ex_1.h5", package = 'h5r')
  f <- H5File(file)
  g <- getH5Group(f, "group_1")
  ds6 <- getH5Dataset(g, "ds_6", inMemory = FALSE)
  r1 <- read1DSlabs(ds6,1:10,rep(5,10))
  r1
}
\keyword{datasets}

