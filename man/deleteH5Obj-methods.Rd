\name{deleteH5Obj-methods}
\alias{deleteH5Obj-methods}
\alias{deleteH5Obj}
\alias{deleteH5Obj,H5Container-method}
\title{Delete an H5 Object}
\description{
  Deletes an H5 Object.
}
\usage{
  \S4method{deleteH5Obj}{H5Container}(h5Obj, h5ObjName)
}
\arguments{
  \item{h5Obj}{
    An object of class H5Container holding something with name h5ObjName.
  }
  \item{h5ObjName}{
    Name of H5Object.
  }
}
\section{Methods}{
  \describe{

    \item{\code{signature(h5Obj = "H5Container")}}{
    }
}}
\examples{
   require(h5r)
   h5 <- H5File("test.h5", 'w')
   g1 <- createH5Group(h5,"grp1")
   dl <- deleteH5Obj(h5,"grp1")
   dl
}
\keyword{methods}
