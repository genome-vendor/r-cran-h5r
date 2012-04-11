\name{h5GroupExists}
\alias{h5GroupExists}
\title{
  Check whether a Group Exists
}
\description{
  'h5GroupExists' is a function that searches for a group of the
  specified name. The function returns a TRUE or FALSE
  statement.
}
\usage{
  h5GroupExists(h5Obj, name)
}
\arguments{
  \item{h5Obj}{
    The object to which the group is attached.
}
  \item{name}{
    The name of the group.
}
}
\value{
  The object returned is of type logical.
}
\examples{
  library("h5r")
  h5 <- H5File("test.H5",'w')
  new <- createH5Group(h5, "grp101")
  check <- h5GroupExists(h5,"grp101")
  check
}
\keyword{datasets}

