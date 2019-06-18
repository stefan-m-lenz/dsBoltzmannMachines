.onLoad <- function(libname, pkgname) {
   juliaUsing("BoltzmannMachines")
}


monitored_fitrbmDS <- function(newobj = 'rbm', x, ...) {
   # trainingresult <- monitored_fitrbm(x, ...)
   # monitoringresult <- trainingresult[[1]]
   # rbm <- trainingresult[[2]]
   # assign(newobj, rbm)
   # return(monitoringresult)
   return(c(x, list(...)))
}
