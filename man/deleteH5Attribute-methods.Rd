\name{deleteH5Attribute-methods}
\alias{deleteH5Attribute}
\alias{deleteH5Attribute-methods}
\alias{deleteH5Attribute,H5Obj-method}
\title{Delete an H5Attribute}
\description{
  Deletes an H5 Attribute which is attached to an object.
}
\usage{
 \S4method{deleteH5Attribute}{H5Obj}(h5Obj, attrName)
}
\arguments{
  \item{h5Obj}{
    Object to which the attribute is attached.
  }
  \item{attrName}{
    Name of attribute.
  }
}
\section{Methods}{
  \describe{
    \item{\code{signature(h5Obj = "H5Obj")}}{
    }
}}
\examples{
  require(h5r)
  h5 <- H5File("test.h5", 'w')
  atr <- createH5Attribute(h5, "atr1", 10:1)
  atr
  dl <- deleteH5Attribute(h5, "atr1")
  dl
}
\keyword{methods}
