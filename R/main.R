.onLoad <- function(libname, pkgname) {
   juliaUsing("BoltzmannMachines")
}


monitored_fitrbmDS <- function(newobj = 'rbm',
                               data = "D") {

   x <- as.matrix(eval(parse(text=data)))

   trainingresult <-
      monitored_fitrbm(x,
                       monitoring = juliaExpr("monitorreconstructionerror!"))

   monitoringresult <- trainingresult[[1]]
   rbm <- trainingresult[[2]]
   assign(newobj, rbm)
   return(monitoringresult)
}
