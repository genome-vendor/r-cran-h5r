##
## This isn't really a test of the h5r package - so probably best to
## just comment it out. Mostly, this is about me allocating too small
## chunks of memory and then running out.
##

##
## Currently, it seems as if I hold on to too much memory - this
## corresponds to me not cleaning something up in HDF5 because
## Valgrind says I'm fine.
##
## require(h5r)



## showPS <- function() system(paste('ps -eo pid,vsz,%mem | grep', Sys.getpid()))
## gcl <- function() { lapply(1:10, gc, verbose = F)[[10]] }

## showPS()
## m <- .Call("h5R_allocate_gig")
## rm(m)
## gcl()
## showPS()

## m <- sapply(1:1000, function(a) {
##   .Call("h5R_allocate_meg")
## })
## rm(m)
## gcl()
## showPS()

## m <- sapply(1:100000, function(a) {
##   .Call("h5R_allocate_k")
## })
## rm(m)
## gcl()
## showPS()

## m <- sapply(1:1000000, function(a) {
##   .Call("h5R_allocate_k")
## })
## rm(m)
## gcl()
## showPS()


