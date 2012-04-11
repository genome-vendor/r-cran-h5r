\name{h5AttributeExists}
\alias{h5AttributeExists}
\title{
  Check whether an Attribute Exists 
}
\description{
  'h5AttributeExists' is a function that searches for an attribute of the
  specified name. The function returns a TRUE or FALSE
  statement. 
}
\usage{
  h5AttributeExists(h5Obj, name)
}
\arguments{
  \item{h5Obj}{
    H5 object in which the attribute is located.
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
  H5A <-createH5Attribute(h5,"a1",10:1, overwrite = TRUE)
  if (h5AttributeExists(h5, "a1")) {
    print("Exists!")
  }
  f = function(attrName) h5AttributeExists(h5, attrName)
  d = listH5Attributes(h5)
  f(d[[1]])
  sapply(d, f)
}
\keyword{attribute}

