\name{H5Container-class}
\Rdversion{1.1}
\docType{class}
\alias{H5Container-class}
\title{Class \code{"H5Container"}}
\description{
  This class is the parent class for H5Files and H5Groups. 
}
\section{Objects from the Class}{
  These objects are often not directly created by created when
  instanteating an H5Group or H5File.
}
\section{Slots}{
  \describe{
    \item{\code{ePtr}:}{Object of class \code{"externalptr"}}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{H5Obj}"}, directly.
}
\section{Methods}{
  \describe{
    \item{createH5Dataset}{\code{signature(h5Obj = "H5Container", datasetName = "character")}: ... }
    \item{createH5Group}{\code{signature(h5Obj = "H5Container", groupName = "character")}: ... }
    \item{deleteH5Obj}{\code{signature(h5Obj = "H5Container")}: ... }
    \item{getH5Dataset}{\code{signature(h5Obj = "H5Container", datasetName = "character")}: ... }
    \item{getH5Group}{\code{signature(h5Obj = "H5Container", groupName = "character")}: ... }
  }
}
\examples{
  showClass("H5Container")
}
\keyword{classes}
