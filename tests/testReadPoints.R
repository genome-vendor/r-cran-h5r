require(h5r)

h5 <- H5File("test.h5", 'w')

createH5Attribute(h5, "e", 10:1)
createH5Attribute(h5, "d", c("jome?", "jeosfmdlmf", "s"))

d <- createH5Dataset(h5, "jim", cbind(c('jime ', 'joe', 'mikey'),
                                      c('jime 2 ', 'joe 2', 'mikey 2')))
d[]

writeH5Data(d, c("johnson", "jodhsd"), c(1,0), c(2, 1))

m <- createH5Dataset(h5, "mm", cbind(rnorm(1000), rnorm(1000)))
m[1:10, 2]
                

d1 <- createH5Dataset(h5, "jon", runif(100000))
p <- readPoints(d1, ss <- sample(1:length(d1), size = 1000, replace = T))
all(p == d1[ss])


d2 <- createH5Dataset(h5, "jodf", paste(runif(200010)))
d2[sample(1:length(d2), size = 1000)]
