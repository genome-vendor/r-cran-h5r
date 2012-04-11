##
## These are tests which error in hdf5 libraries and should give an R error.
##
require(h5r)

source("tinyTestHarness.R")

TH <- TestHarness()

file <- system.file("h5_files", "ex_1.h5", package = 'h5r')

f <- H5File(file)

TH("file existence", assertError(d <- H5File("sdfsdf")))
TH("dataset existence", assertError(x <- getH5Dataset(f, "sdfsf")))
TH("group existence", assertError(x <- getH5Group(f, "sdfasdf")))
TH("attribute existence", assertError(getH5Attribute(f, "sfds")))

TH(action='print')
TH(action='throw')
