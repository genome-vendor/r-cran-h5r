\name{H5File}
\alias{H5File}
\title{
  Create an H5File.
}
\description{
  Constructs a read-only H5File.
}
\usage{
  H5File(fileName, mode = "r")
}
\arguments{
  \item{fileName}{
    A path to an H5File. Typically, the path cannot contain special
    characters, such as "~". 
  }
  \item{mode}{
    Whether to open the file up with write capabilities.
  }
}
\value{
  An object of class H5File. 
}
\examples{
 f <- system.file("h5_files", "ex_1.h5", package = "h5r")
 h5 <- H5File(f)
 d <- getH5Dataset(getH5Group(h5, "group_1"), "ds_1")
 dim(d)
 d[1:10,]
}
\keyword{file}
