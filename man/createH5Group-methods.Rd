\name{createH5Group-methods}
\alias{createH5Group}
\alias{createH5Group-methods}
\alias{createH5Group,H5Container,character-method}
\title{Create an H5Group in an H5Container}
\description{
  Creates a new H5Group under h5Obj.
}
\usage{
  \S4method{createH5Group}{H5Container,character}(h5Obj, groupName, overwrite = TRUE)
}
\arguments{
  \item{h5Obj}{
    H5 object in which the group(s) or dataset(s) are located.
  }
  \item{groupName}{
    Name of group to create.
  }
  \item{overwrite}{
    Flag specifying whether to err if attribute name already exists.
  }
}
\section{Methods}{
  \describe{
    \item{\code{signature(h5Obj = "H5Container", groupName = "character")}}{
    }
}}
\keyword{methods}
