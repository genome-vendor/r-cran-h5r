require(h5r)

##
## The tests.
##
file <- system.file("h5_files", "ex_1.h5", package = 'h5r')
f <- H5File(file)

gc()
v <- replicate(100000, {
  getH5Group(f, "group_1")
})
rm(v)
lapply(1:10, function(i) gc())
