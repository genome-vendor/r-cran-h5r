\name{H5Obj-class}
\Rdversion{1.1}
\docType{class}
\alias{H5Obj-class}
\alias{H5File-class}
\alias{H5Group-class}
\alias{H5Dataset-class}
\alias{H5DataContainer-class}
\alias{H5Attribute-class}
\alias{getH5Group,H5Container,character-method}
\alias{getH5Group}
\alias{getH5Attribute,H5Obj,character-method}
\alias{getH5Attribute}
\alias{getH5Dataset,H5Container,character-method}
\alias{getH5Dataset}
\alias{getH5Dim,H5DataContainer-method}
\alias{getH5Dim}
\alias{getH5Type,H5DataContainer-method}
\alias{getH5Type}
\alias{nrow,H5DataContainer-method}
\alias{ncol,H5DataContainer-method}
\alias{[,H5DataContainer-method}
\alias{[,H5Dataset-method}
\alias{dim,H5DataContainer-method}
\alias{length,H5DataContainer-method}
\alias{readSlab}
\alias{listH5Contents}
\alias{print.H5ContentList}
\alias{h5Exists}
\alias{start}
\alias{width}
\alias{[,H5DataContainer,ANY,ANY,ANY-method}
\alias{[,H5Dataset,ANY,ANY,ANY-method}
\alias{show,H5DataContainer-method}
\alias{show,H5File-method}
\alias{show,H5Group-method}
\alias{show,H5Obj-method}
\alias{ls,H5Obj-method}

\title{Class "H5Obj"}
\description{Classes for objects originating from HDF5 files.}
\section{Objects from the Class}{
  Objects of this class should not be directly instanteated, but rather
  obtained from calls to methods. The H5File provides the entry point
  into HDF5 Files and can be obtained via the function \code{"H5File"}
}
\section{Slots}{
  \describe{
    \item{\code{ePtr}:}{Object of class \code{"externalptr"} }
  }
}
\section{Methods}{
  \describe{
    \item{getH5Attribute}{\code{signature(h5Obj = "H5Obj", attrName =
	"character")}: Get an attribute from an H5Obj. }
    \item{getH5Dataset}{\code{signature(h5Obj = "H5Obj", datasetName =
	"character")}: Get a dataset from an H5Obj, this needs to be
      either a H5File or H5Group. }
    \item{getH5Dim}{\code{signature(h5Obj = "H5DataContainer", datasetName =
	"character")}: 
      This gets the dimension of a dataset given a dataset name and an
      H5Obj. This function is used internally, however the more typical
      case is to retrieve a dataset and then ask its dimension. 
    }
    \item{getH5Group}{\code{signature(h5Obj = "H5Obj", groupName =
	"character")}:  Get an H5Group from another H5Group or H5File
      object. 
    }
    \item{getH5Type}{\code{signature(h5Obj = "H5DataContainer", datasetName =
	"character")}: This gets the type of a dataset given its parent
      and its name. Similar to getH5Dim this function is useful
      internally.
    }
  }
}
\examples{
 f <- system.file("h5_files", "ex_1.h5", package = "h5r")
 h5 <- H5File(f)
 d <- getH5Dataset(getH5Group(h5, "group_1"), "ds_1")
 dim(d)
 d[1:10,]

}
\keyword{classes}
