assertError <- function(expr) {
  tryCatch({{expr}; FALSE}, simpleError = function(e) {
    return(TRUE)
  })
}

TestHarness <- function() {
  tests <- list()

  getTime <- function(elt) {
    elt[["time"]][3]
  }
  getResult <- function(elt) {
    elt[["result"]]
  }
  printResults <- function() {
    mwidth <- max(nchar(names(tests))) + 5
    fmtString <- paste("\t%-", mwidth, "s %-10g %-10s\n", sep = "")

    cat(sprintf("%s Results for %d tests %s \n\n", paste(rep("-", 30), collapse = ""), length(tests),
                paste(rep("-", 30), collapse = "")))
    
    for (elt in names(tests)) {
      cat(sprintf(fmtString, elt, getTime(tests[[elt]]), getResult(tests[[elt]])))
    }
  }
  
  function(nm, test, action = c("test", "print", "throw")) {
    action <- match.arg(action)
    switch(action,
           test = {
             tm <- system.time({
               b <- tryCatch(test, simpleError = function(e) {
                 return(FALSE)
               }, simpleWarning = function(e) return(FALSE))
             })
             tests[[nm]] <<- list("result" = b, "time" = tm)
           },
           print = {
             printResults()
           },
           throw = {
             errs <- ! sapply(tests, getResult)
             if (any(errs)) {
               stop(simpleError(paste("Tests in error:\n", paste(paste("\t", names(tests)[errs], sep = ""),
                                                                 collapse = "\n"),
                                      sep = "")))
             }
           })
  }
}
