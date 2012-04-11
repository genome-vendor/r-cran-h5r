\name{H5DataFrame-class}
\Rdversion{1.1}
\docType{class}
\alias{H5DataFrame-class}
\alias{H5DataFrame}
\alias{writeH5DataFrame}
\alias{[,H5DataFrame,ANY,ANY,ANY-method}
\alias{[[,H5DataFrame-method}
\alias{$,H5DataFrame-method}
\alias{as.data.frame,H5DataFrame-method}
\alias{colnames,H5DataFrame-method}
\alias{dim,H5DataFrame-method}
\alias{ncol,H5DataFrame-method}
\alias{nrow,H5DataFrame-method}

\title{Class \code{"H5DataFrame"}}
\description{
  Presents a data.frame style interface to an HDF5 file. The data for
  each column is stored in a separate H5Dataset.
}
\usage{
  H5DataFrame(fileName, nms = NA)
  writeH5DataFrame(df, fileName, overwrite = T)
}
\arguments{
  \item{nms}{
    File names.
  }
  \item{df}{
    The input data.frame from which to construct an H5DataFrame
  }
  \item{fileName}{
    The fileName to either store or load the H5DataFrame from.
  }
  \item{overwrite}{
    Flag as to whether to overwrite the file.
  }
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{H5DataFrame()}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}}
    \item{\code{h5File}:}{Object of class \code{"H5File"}}
    \item{\code{h5Datasets}:}{Object of class \code{"list"}}
    \item{\code{names}:}{Object of class \code{"character"}}
    \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"}}
    \item{\code{.S3Class}:}{Object of class \code{"character"}}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{data.frame}"}, directly.
  Class \code{"\linkS4class{list}"}, by class "data.frame", distance 2.
  Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
  Class \code{"\linkS4class{vector}"}, by class "data.frame", distance 3.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "H5DataFrame", i = "ANY", j = "ANY", drop = "ANY")}: ... }
    \item{[[}{\code{signature(x = "H5DataFrame")}: ... }
    \item{$}{\code{signature(x = "H5DataFrame")}: ... }
    \item{as.data.frame}{\code{signature(x = "H5DataFrame")}: ... }
    \item{colnames}{\code{signature(x = "H5DataFrame")}: ... }
    \item{dim}{\code{signature(x = "H5DataFrame")}: ... }
    \item{ncol}{\code{signature(x = "H5DataFrame")}: ... }
    \item{nrow}{\code{signature(x = "H5DataFrame")}: ... }
  }
}
\examples{
   require(h5r)
   showClass("H5DataFrame")
}
\keyword{classes}
