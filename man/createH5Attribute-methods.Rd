\name{createH5Attribute-methods}
\alias{createH5Attribute}
\alias{createH5Attribute-methods}
\alias{createH5Attribute,H5Obj-method}
\title{Method for Creating Attributes on H5 Objects}
\description{
  Creates a new attribute on an H5 Object.
}
\usage{
  \S4method{createH5Attribute}{H5Obj}(h5Obj, attrName, attrValue, overwrite = TRUE)
}
\arguments{
  \item{h5Obj}{
    Oject to which the attribute is attached.
  }
  \item{attrName}{
    Name of attribute.
  }
  \item{attrValue}{
    The value portion of the attribute which contains data element(s)
    of the same data type. 
  }
  \item{overwrite}{
    Flag specifying whether to err if attribute name already exists.
  }
}
\value{
  An object of type H5Attribute.  
}
\section{Methods}{
  \describe{
    \item{\code{signature(h5Obj = "H5Obj")}}{
    }
}}
\examples{
  require(h5r)
  h5 <- H5File("test.h5", 'w')
  a <-  createH5Attribute(h5, "attri", c("a","b","c","d"))
  b <-  createH5Attribute(h5, "bute", as.integer(c(rnorm(1),rpois(1,5))))
  list(a,b)
}
\keyword{methods}
