\name{createH5Dataset-methods}
\alias{createH5Dataset-methods}
\alias{createH5Dataset}
\alias{createH5Dataset,H5Container,character-method}
\title{Create Datasets in HDF5 Files}
\description{
  This method creates a new dataset in an HDF5 file.
}
\usage{
  \S4method{createH5Dataset}{H5Container,character}(h5Obj, datasetName, data, dims, dType, chunkSizes, overwrite)
}
\arguments{
  \item{h5Obj}{
    H5 object in which the dataset is located, e.g., H5File or H5Group.
  }
  \item{datasetName}{
    Specify name of dataset.
  }
  \item{data}{
    The actual data to store, e.g., a matrix or vector of the same data type.
  }
  \item{dims}{
    Specify dimensions of dataset.
  }
  \item{dType}{
    Specify data type; integer, double or character.
  }
  \item{chunkSizes}{
    Specify chunk sizes of dataset.
  }
  \item{overwrite}{
    Overwrite dataset.
  }
}
\section{Methods}{
  \describe{
    \item{\code{signature(h5Obj = "H5Container", datasetName = "character")}}{
      Given an H5Container object, e.g., H5Group and a name create a
      dataset under that container in the file.
    }
}}
\examples{
  require(h5r)
  h5 <- H5File("test.h5",'w')
  m <- createH5Dataset(h5, "mm", cbind(rnorm(1000), rnorm(1000)))
  m[1:10, 2]

  m <- createH5Dataset(h5, "nn", dims = c(2,2),dType = "character")
  m[]
  writeH5Data(m, c("agct", "tcga"), c(1,1), c(1,2)) 
  writeH5Data(m, c("jack", "jill"), c(2,1), c(1,2)) 
  m[]
  ls(h5)
  m <- createH5Dataset(h5, "vect", dims = c(10000,1), dType = "integer")
  m[]
  for(i in 1:10) {
      writeH5Data(m, as.integer(rpois(1000,lambda = 10)), c((i-1)*1000+1,1),c( 1000,1))
  }
}
\keyword{methods}
