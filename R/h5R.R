## ##############################################################################
##
## h5R.R : Main interface file to the hdf5 C libraries.
##
## ##############################################################################

## These are defined in H5Tpublic.h
.h5Types <- c("integer", "double", "time", "character", "logical")

## ##############################################################################
##
## Class Definitions
##
## ##############################################################################
setClass("H5Obj", representation(ePtr = "externalptr"))
setClass("H5Container", contains = "H5Obj")
setClass("H5File", contains = "H5Container",
         representation(fileName = "character"))
setClass("H5Group", contains = "H5Container",
         representation(name = "character"))

setClassUnion("envOrNULL", c("environment", "NULL"))
setClass("H5DataContainer", contains = "H5Obj",
         representation(name = "character", dims = "integer",
                        h5Type = "integer", .data = "envOrNULL"))
setClass("H5Dataset", contains = "H5DataContainer")
setClass("H5Attribute", contains = "H5DataContainer")
setClass("hSlab", representation = representation(s = "integer", w = "integer"))
setClass("H5DataFrame", contains = "data.frame", representation(h5File = "H5File", h5Datasets = "list"))

## ##############################################################################
##
## Generics Definitions
##
## ##############################################################################
setGeneric("getH5Group", function(h5Obj, groupName, ...) standardGeneric("getH5Group"))
setGeneric("getH5Dim", function(h5Obj, ...) standardGeneric("getH5Dim"))
setGeneric("getH5Type", function(h5Obj, ...) standardGeneric("getH5Type"))
setGeneric("getH5Dataset", function(h5Obj, datasetName, ...) standardGeneric("getH5Dataset"))
setGeneric("getH5Attribute", function(h5Obj, attrName, ...) standardGeneric("getH5Attribute"))
setGeneric("createH5Group", function(h5Obj, groupName, ...) standardGeneric("createH5Group"))
setGeneric("createH5Dataset", function(h5Obj, datasetName, ...) standardGeneric("createH5Dataset"))
setGeneric("createH5Attribute", function(h5Obj, attrName, attrValue, ...) standardGeneric("createH5Attribute"))
setGeneric("writeH5Data", function(h5Obj, ...) standardGeneric("writeH5Data"))
setGeneric("readH5Data", function(h5Obj, ...) standardGeneric("readH5Data"))
setGeneric("deleteH5Obj", function(h5Obj, h5ObjName, ...) standardGeneric("deleteH5Obj"))
setGeneric("deleteH5Attribute", function(h5Obj, attrName, ...) standardGeneric("deleteH5Attribute"))

## ##############################################################################
##
## C-helper
##
## ##############################################################################
.myCall <- function(nm, ...) {
  .Call(nm, ..., PACKAGE = 'h5r')
}

H5File <- function(fileName, mode = 'r') {
  new("H5File", fileName, mode)
}

.ePtr <- function(obj) obj@ePtr

.H5Obj <- function(ep) {
  o <- new("H5Obj")
  o@ePtr <- ep
  return(o)
}

.H5Group <- function(ep, name) {
  o <- new("H5Group")
  o@ePtr <- ep
  o@name <- name
  return(o)
}

.initH5DataContainer <- function(o, name, inMemory) {
  o@name <- name
  o@h5Type <- getH5Type(o)
  o@dims <- getH5Dim(o)
  
  if (! inMemory) {
    o@.data <- NULL
  } else {
    o@.data <- new.env(parent = emptyenv(), hash = TRUE)
  }
  return(o)
}

.H5Attribute <- function(ep, name, inMemory = TRUE) {
  o <- new("H5Attribute")
  o@ePtr <- ep
  return(.initH5DataContainer(o, name, inMemory = inMemory))
}

.H5Dataset <- function(ep, name, inMemory = FALSE) {
  o <- new("H5Dataset")
  o@ePtr <- ep
  return(.initH5DataContainer(o, name, inMemory = inMemory))
}

.hasData <- function(h5DataContainer) {
  return(exists(".data", h5DataContainer@.data))
}

.putData <- function(h5DataContainer, dta) {
  assign(".data", dta, h5DataContainer@.data)
}

.getData <- function(h5DataContainer) {
  get(".data", h5DataContainer@.data)
}

.inMemory <- function(h5Dataset) {
  return(! is.null(h5Dataset@.data))
}

setMethod("getH5Group", c("H5Container", "character"), function(h5Obj, groupName) {
  if (is.null(x <- .myCall("h5R_get_group", .ePtr(h5Obj), groupName)))
    stop(paste("Group:", groupName, "cannot be opened."))
  .H5Group(x, groupName)
})

setMethod("getH5Dim", "H5DataContainer", function(h5Obj) {
  .myCall('h5R_get_dims', .ePtr(h5Obj))
})

setMethod("getH5Type", "H5DataContainer", function(h5Obj) {
  .myCall("h5R_get_type", .ePtr(h5Obj))
})

setMethod("initialize", c("H5File"), function(.Object, fileName, mode = c('r', 'w')) {
  ## This is obscene. I have to do this because somehow Subclasses
  ## call this at *class* instantiation time. 
  if (missing(fileName))
    return(.Object)

  mode <- match.arg(mode)
  if (! file.exists(fileName) && mode == 'w') {
    .myCall("h5R_create", fileName)
  }
  if (! file.exists(fileName)) {
    stop(paste("Unable to open file:", fileName, "does not exist."))
  }

  ## convert the fileName - essentially for ~. 
  fileName <- tools:::file_path_as_absolute(normalizePath(fileName))
  x <- .myCall("h5R_open", fileName, if (mode == 'r') as.integer(0) else as.integer(1))
  if (is.null(x)) {
    stop(paste("Problem opening file:", fileName))
  }
  .Object@ePtr <- x
  .Object@fileName <- fileName
  
  return(.Object)
})

setMethod("getH5Dataset", c("H5Container", "character"), function(h5Obj, datasetName, inMemory = FALSE) {
  if (is.null(x <- .myCall("h5R_get_dataset", .ePtr(h5Obj), datasetName))) {
    stop(paste("Dataset:", datasetName, "cannot be opened."))
  }
  return(.H5Dataset(x, datasetName, inMemory = inMemory))
})

setMethod("getH5Attribute", c("H5Obj", "character"), function(h5Obj, attrName) {
  if (is.null(x <- .myCall("h5R_get_attr", .ePtr(h5Obj), attrName))) {
    stop(paste("Attribute:", attrName, "cannot be opened."))
  }
  return(.H5Attribute(x, attrName, inMemory = TRUE))
})

##
## Writing.
##
.flush <- function(h5Obj) {
  .myCall("h5R_flush", .ePtr(h5Obj))
}

setMethod("writeH5Data", c("H5Dataset"), function(h5Obj, data, offsets, extents) {
  storage.mode(data) <- h5r:::.h5Types[getH5Type(h5Obj) + 1]
  .myCall("h5R_write_slab", .ePtr(h5Obj), as.integer(offsets - 1), as.integer(extents), data)
})

setMethod("createH5Dataset", c("H5Container", "character"), function(h5Obj, datasetName, data,
                                                                     dims, dType = c("integer", "double", "character"),
                                                                     chunkSizes = NA, overwrite = TRUE) {
  if (h5DatasetExists(h5Obj, datasetName)) {
    if (! overwrite)
      stop(paste("Dataset:", datasetName, "already exists."))
    else
      deleteH5Obj(h5Obj, datasetName)
  }

  mData <- missing(data)
  mDims <- missing(dims)
  mType <- missing(dType)
  mChnk <- missing(chunkSizes)

  if (mData && (mDims || mType)) {
    stop("Must specify either data or dimensions and type.")
  }

  if (mData && mType) {
    storage.mode(data) <- dType
  }
  
  if (! mData) { 
    if (is.null(dim(data))) {
      dims <- length(data)
    } else {
      ## note order.
      dims <- dim(data)
      data <- aperm(data)
    }
    dType <- storage.mode(data)
  } else {
    dType <- match.arg(dType)
  }
  iType <- as.integer(match(dType, .h5Types) - 1)

  if (is.na(iType)) {
    stop("Type is not resolveable.")
  }
  dims  <- as.integer(dims)
  if (mChnk)
    chunkSizes <- rep(4096, length(dims))

  h5Dataset <- .H5Dataset(.myCall("h5R_create_dataset", .ePtr(h5Obj), datasetName, iType, dims, as.integer(chunkSizes)),
                          datasetName)
  
  if (! mData) {
    if(! writeH5Data(h5Dataset, data,
                     as.integer(rep(1L, length(dims))),
                     as.integer(dims))) stop("Unable to write data.")
  }
  .flush(h5Obj)
  
  return(h5Dataset)
})

setMethod("createH5Group", c("H5Container", "character"), function(h5Obj, groupName,
                                                                   overwrite = TRUE) {
  if (h5GroupExists(h5Obj, groupName)) {
    if (! overwrite) {
      stop(paste("Group:", groupName, "exists."))
    } else {
      deleteH5Obj(h5Obj, groupName)
    }
  }
  h5Group <- .H5Group(.myCall("h5R_create_group", .ePtr(h5Obj), groupName), groupName)
  .flush(h5Obj)
  return(h5Group)
})

setMethod("createH5Attribute", c("H5Obj"), function(h5Obj, attrName, attrValue, overwrite = TRUE) {
  if (h5AttributeExists(h5Obj, attrName)) {
    if (overwrite) {
      deleteH5Attribute(h5Obj, attrName)
    } else {
      stop("Attribute exists, delete first, or specify overwrite.")
    }
  }
  dType <- as.integer(match(storage.mode(attrValue), .h5Types) - 1)
  
  if (is.null(dim(attrValue))) {
    nr <- length(attrValue)
    nc <- 1
  } else {
    if (length(dim(attrValue)) > 2) {
      stop("Don't support greater than 2-d attributes")
    }
    nr <- nrow(attrValue)
    nc <- ncol(attrValue)
  }

  h5Attr <- .H5Attribute(.Call("h5R_create_attribute", .ePtr(h5Obj), as.character(attrName),
                               as.integer(dType),
                               as.integer(c(nr, nc))), attrName)
  if (.Call("h5R_write_attribute", .ePtr(h5Attr), attrValue) && .flush(h5Obj)) {
    return(h5Attr)
  } else {
    stop("Unable to create attribute.")
  }
})

setMethod("deleteH5Obj", c("H5Container"), function(h5Obj, h5ObjName) {
  if (h5ObjectExists(h5Obj, h5ObjName)) {
    return(.myCall("h5R_delete_object", .ePtr(h5Obj), as.character(h5ObjName)) && .flush(h5Obj))
  } else {
    return(FALSE)
  }
})

setMethod("deleteH5Attribute", c("H5Obj"), function(h5Obj, attrName) {
  .myCall("h5R_delete_attribute", .ePtr(h5Obj), as.character(attrName)) && .flush(h5Obj)
})

##
## The whole slicing infrastructure.
## 
.internalSlice <- function(x, i, j, ..., drop = TRUE) {
  if (!.hasData(x)) {
    .putData(x, .loadDataset(x))
  }
  d <- .getData(x)
  
  if (is.null(dim(x))) {
    if (! missing(j))
      stop("incorrect number of dimensions")
    d[i]
  }
  else {
    d[i, j, ..., drop = drop]
  }
}

.marginCheck <- function(i, d) {
  if (any(i <= 0))
    stop("Non-positive selections not allowed when subsetting H5Datasets")
  if (max(i) > d)
    stop("Index out of range.")
}

.getExtras <- function(kall, dims) {
  d <- match("drop", names(kall))
  if (! is.na(d))
    kall <- kall[-d]

  j <- match("j", names(kall))
  if (! is.na(j))
    kall <- kall[-j]

  i <- match("i", names(kall))
  if (! is.na(i))
    kall <- kall[-i]

  kall <- kall[-(1:2)]

  if (length(kall) != length(dims))
    stop("Incorrect number of dimensions.")
  
  mapply(function(a,b) {
    if (is.call(a) || is.numeric(a) && !(as.character(a) == ""))
      eval(a)
    else
      seq.int(1, b)
  }, kall, dims, SIMPLIFY = FALSE)
}

setMethod("[", "H5DataContainer", .internalSlice)
setMethod("[", "H5Dataset", function(x, i, j, ..., drop = TRUE) {
  iMissing <- TRUE
  if (! missing(i)) {
    iMissing <- FALSE
    .marginCheck(i, nrow(x))
  } 
  
  jMissing <- TRUE
  if (! missing(j)) {
    jMissing <- FALSE
    .marginCheck(j, ncol(x))
  }
  
  if (.inMemory(x)) {
    ## this is a copy of internal slice, if I don't do it this way
    ## then the arg '...' doesn't really stay consistent and I cannot
    ## pass it through to the '[' built-in.
    if (!.hasData(x)) {
      .putData(x, .loadDataset(x))
    }
    d <- .getData(x)
    
    if (is.null(dim(x))) {
      if (! jMissing)
        stop("incorrect number of dimensions")
      d[i]
    }
    else {
      d[i, j, ..., drop = drop]
    }
  }
  else {
    ## One dimensional dataset.
    if (is.null(dim(x))) {
      if (! jMissing)
        stop("incorrect number of dimensions")
      if (! iMissing) {
        ## dta <- readSlab(x, min(i), max(i) - min(i) + 1)
        ## ## contiguity
        ## if (any(diff(i) != 1)) {
        ##   dta <- dta[i - min(i) + 1]
        ## }
        dta <- readPoints(x, i)
      }
      else
        dta <- readSlab(x, 1, length(x))
    }

    ## > 1-D dataset.
    else {

      if (length(dim(x)) > 3) {
        ## Need to call this function w/in this scope, but don't want
        ## to absorb cost if we won't need it.
        kall <- as.list(match.call())
      }
      
      extras <- tryCatch(list(...), simpleError = function(e) {
        if (length(dim(x)) > 3) {
          .getExtras(kall, dim(x)[-(1:2)]) # remove i,j
        } else {
          return(list())
        }
      })

      nExtra <- 0
      if (length(extras) > 0) {
        for (k in 3:(2 + length(extras))) {
          nExtra <- nExtra + 1
          .marginCheck(extras[[k-2]], dim(x)[k])
        }
      }
      
      ## need to specify the range to select.
      sel <- matrix(NA, nrow = length(dim(x)), ncol = 2)
      lst <- list(`[`, x = quote(dta))
      
      if (! iMissing) {
        sel[1, ] <- range(i)
        lst$i <- i - min(i) + 1
      } else {
        ## retain original dimensions.
        sel[1, ] <- c(1, dim(x)[1])
        lst$i <- seq.int(sel[1,1], sel[1,2])
      }

      if (! jMissing) {
        sel[2, ] <- range(j)
        lst$j <- j - min(j) + 1
      } 
      else {
        ## retain original dimensions.
        sel[2, ] <- c(1, dim(x)[2])
        lst$j <- seq.int(sel[2,1], sel[2,2])
      }

      if (nrow(sel) > 2) {
        for (k in 3:nrow(sel)) {
          if (length(extras) >= k - 2) {
            sel[k, ] <- range(extras[[k - 2]]) # the offset into the list.
            lst[[k+2]] <- extras[[k-2]] - min(extras[[k-2]]) + 1
          }
          else {
            sel[k, ] <- c(1, dim(x)[k])
            lst[[k+2]] <- seq.int(sel[k,1], sel[k,2])
          }
        }
      }
      ext <- sel[,2] - sel[,1] + 1
      dta <- readSlab(x, sel[,1], ext)

      ## Now I have to fix things up because of the contiguity
      ## issue. Essentially, if the i, j, ... specified by the user
      ## aren't contiguous then I have to subselect the dta to conform
      ## to their selection.
      dta <- eval(as.call(lst))
    }
    dta <- if (drop) drop(dta) else dta

    ## This is so dim(x[]) matches dim(x).
    if (is.null(dim(x)))
      as.vector(dta)
    else
      dta
  }
})

## This function is written to leverage the possibility of fast
## contiguous range access.
setMethod("[", c("H5Dataset", "hSlab", "missing", "missing"), function(x, i) {
  if (.inMemory(x))
    stop("Not implemented for inMemory datasets.")
  
  nr <- length(i)
  if (! ((nr == 1 && is.null(dim(x))) || (nr == length(dim(x)))))
    stop("Dimension mismatch: nrow(x) == length(dim(x))")
  
  readSlab(x, i@s, i@w)
})

##
## Note: the two reverses.
##
.loadDataset <- function(h5Dataset) {
  d <- readH5Data(h5Dataset)
  dim(d) <- rev(dim(h5Dataset))
  
  if (! is.null(dim(h5Dataset))) aperm(d) else d
}

read1DSlabs <- function(h5Dataset, offsets, dims) {
  .myCall("h5R_read_1d_slabs", .ePtr(h5Dataset), as.integer(offsets - 1), as.integer(dims))
}

readSlab <- function(h5Dataset, offsets, dims) {
  if (! all((offsets + dims - 1) <= dim(h5Dataset)))
    stop("error invalid slice specification in readSlab.")
  
  d <- .myCall("h5R_read_slab", .ePtr(h5Dataset), as.integer(offsets - 1), as.integer(dims))
  dim(d) <- rev(dims)

  if (! is.null(dim(h5Dataset))) aperm(d) else d
}

readPoints <- function(h5Dataset, idxs) {
  if (is.null(dim(idxs))) {
    nr <- length(idxs)
    nc <- 1
  } else {
    stop("readPoints doesn't work on higher dimensional data.")
  }
  .myCall("h5R_read_points", .ePtr(h5Dataset), as.integer(idxs - 1), as.integer(nr), as.integer(nc))
}

setMethod("readH5Data", "H5Dataset", function(h5Obj) {
  .myCall('h5R_read_dataset', .ePtr(h5Obj))
})

setMethod("readH5Data", "H5Attribute", function(h5Obj) {
  .myCall('h5R_read_attr', .ePtr(h5Obj))
})

setMethod("show", "H5Obj", function(object) {
  cat("class of:", class(object), "\n")
})

setMethod("show", "H5File", function(object) {
  callNextMethod(object)
  cat("file:", object@fileName, "\n")
})

setMethod("show", "H5Group", function(object) {
  callNextMethod(object)
  cat("name:", object@name, "\n")
})

.getTypeString <- function(h5Dataset) {
  .h5Types[h5Dataset@h5Type + 1]
}

setMethod("show", "H5DataContainer", function(object) {
  callNextMethod(object)
  cat("name:", object@name, "\n")
  cat("dim: ", object@dims, "\n")
  cat("type:", .getTypeString(object), "\n")
})

setMethod("dim", "H5DataContainer", function(x) if (length(x@dims) < 2) NULL else x@dims)
setMethod("length", "H5DataContainer", function(x) if (is.null(dim(x))) x@dims else prod(x@dims))
setMethod("nrow", "H5DataContainer", function(x) {
  x@dims[1]
})
setMethod("ncol", "H5DataContainer", function(x) {
  x@dims[2]
})


##
## Examining the file contents.
##

## construct a list of elements in the file.
.listH5Contents <- function(h5Obj) .myCall("h5R_list_contents", .ePtr(h5Obj))
listH5Attributes <- function(h5Obj) .myCall("h5R_list_attributes", .ePtr(h5Obj))

listH5Contents <- function(h5Obj) {
  contents <- .listH5Contents(h5Obj)
  lst <- lapply(contents, function(a) {
    h5Obj <- switch(as.character(a[[2]]),
                    '0' = { getH5Group(h5Obj, a[[1]]) },
                    '1' = { getH5Dataset(h5Obj, a[[1]]) })

    if (class(h5Obj) == "H5Dataset") {
      dim <- getH5Dim(h5Obj)
    } else {
      dim <- NA
    }
    list(name = a[[1]],
         type = a[[2]],
         attributes = listH5Attributes(h5Obj),
         dim = dim)
  })
  names(lst) <- sapply(lst, "[[", 1)
  lst <- lapply(lst, function(a) {
    a$name <- basename(a$name)
    a
  })
  return(lst)
}

setMethod("ls", "H5Obj", function(name) {
  names(listH5Contents(name))
})

h5ObjectExists <- function(h5Obj, name) {
  .myCall("h5R_dataset_exists", .ePtr(h5Obj), name)
}

h5GroupExists <- function(h5Obj, name) {
  .myCall("h5R_dataset_exists", .ePtr(h5Obj), name)
}

h5DatasetExists <- function(h5Obj, name) {
  .myCall("h5R_dataset_exists", .ePtr(h5Obj), name)
}

h5AttributeExists <- function(h5Obj, name) {
  .myCall("h5R_attribute_exists", .ePtr(h5Obj), name)
}



################################################################
## 
## H5DataFrame interface
##
################################################################
H5DataFrame <- function(fileName, nms = NA) {
  .getCols <- function(h5) {
    nms <- names(listH5Contents(h5))
    nms[nms != "."] 
  }

  h5File <- H5File(fileName)
  if (is.na(nms)) {
    nms <- .getCols(h5File)
  }
  h5Datasets <- lapply(nms, function(nm) {
    getH5Dataset(h5File, nm)
  })
  names(h5Datasets) <- nms

  stopifnot(length(unique(sapply(h5Datasets, length))) == 1)
  
  obj <- new("H5DataFrame")
  obj@h5File <- h5File
  obj@h5Datasets <- h5Datasets
  return(obj)
}

setMethod("ncol", "H5DataFrame", function(x) {
  length(x@h5Datasets)
})

setMethod("nrow", "H5DataFrame", function(x) {
  length(x@h5Datasets[[1]])
})

setMethod("dim", "H5DataFrame", function(x) {
  c(nrow(x), ncol(x))
})

setMethod("colnames", "H5DataFrame", function(x) {
  names(x@h5Datasets)
})

setMethod("$", "H5DataFrame", function(x, name) {
  if (! (name %in% names(x@h5Datasets))) {
    return(NULL)
  }
  x[[name]]
})

setMethod("[", c("H5DataFrame", "ANY", "ANY"), function(x, i, j) {
  missingI <- missingJ <- FALSE

  if (missing(j)) missingJ <- TRUE
  if (missing(i)) missingI <- TRUE

  ## cat(paste("missingJ:", missingJ,
  ##           "\nmissingI:", missingI,
  ##           "\nclass  I:", if (! missingI) class(i) else "missing",
  ##           "\nclass  J:", if (! missingJ) class(j) else "missing"), "\n")
  
  if (missingJ && missingI) {
    ## [] -- return everything.
    ## -> data.frame
    as.data.frame(lapply(colnames(x), function(n) {
      x@h5Datasets[[n]][]
    }))
  } else if (missingJ & !missingI) {
    ## [i,] -- return i rows across all colummns
    ## -> data.frame
    z <- as.data.frame(lapply(colnames(x), function(n) {
      x@h5Datasets[[n]][i]
    }))
    colnames(z) <- colnames(x)
    z
  } else if (!missingJ && !missingI) {
    ## [i,j] -- return sub-matrix
    ## -> data.frame | matrix
    if (length(j) == 1) {
      ## -> vector
      x@h5Datasets[[j]][i]
    } else {
      ## -> data.frame
      z <- as.data.frame(lapply(j, function(jj) {
        (x@h5Datasets[[jj]])[i]
      }))
      colnames(z) <- colnames(x)[j]
      z
    }
  } else if (! missingJ) {
    if (length(j) == 1) {
      ## -> vector
      x@h5Datasets[[j]][]
    } else {
      ## -> data.frame
      z <- as.data.frame(lapply(j, function(jj) {
        (x@h5Datasets[[jj]])[]
      }))
      colnames(z) <- colnames(x)[j]
      z
    }
  } else {
    ## cat(paste("missingJ:", missingJ,
    ##             "missingI:", missingI,
    ##             "class  I:", class(i),
    ##             "class  J:", class(i)))
  }
})

setMethod("[[", c("H5DataFrame", "ANY", "ANY"), function(x, i, j) {
  missingI <- missingJ <- FALSE
  
  if (missing(j)) missingJ <- TRUE
  if (missing(i)) missingI <- TRUE

  ## cat(paste("missingJ:", missingJ,
  ##           "\nmissingI:", missingI,
  ##           "\nclass  I:", if (! missingI) class(i) else "missing",
  ##           "\nclass  J:", if (! missingJ) class(j) else "missing"), "\n")

  if (!missingI && missingJ) {
    x@h5Datasets[[i]][]
  }
})

setMethod("as.data.frame", "H5DataFrame", function(x) {
  d <- as.data.frame(lapply(colnames(x), function(nm) x[[nm]]))
  colnames(d) <- colnames(x)
  return(d)
})

writeH5DataFrame <- function(df, fileName, overwrite = T) {
  if (file.exists(fileName) && overwrite) {
    file.remove(fileName)
  } else if (file.exists(fileName) && !overwrite) {
    stop("File exists!")
  }
  h <- H5File(fileName, 'w')
  mapply(df, colnames(df), FUN = function(a, nm) {
    createH5Dataset(h, nm, a)
  })
  H5DataFrame(fileName)
}

##
## hSlab stuff.
##
hSlab <- function(start, width = NA, end = NA) {
  stopifnot(length(start) == length(width) || length(start) == length(end))

  if (any(is.na(end)) & any(is.na(width))) {
    stop("Must specify either end or width.")
  } else {
    if(any(is.na(end))) {
      width <- width
    }
    else {
      width <- (end - start + 1)
    }
  }
  obj <- new("hSlab")
  obj@s <- as.integer(start)
  obj@w <- as.integer(width)
  return(obj)
}

setMethod("length", "hSlab", function(x) {
  length(x@s)
})
