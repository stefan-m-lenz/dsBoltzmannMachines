.onLoad <- function(libname, pkgname) {
   juliaUsing("BoltzmannMachines")
}


monitored_fitrbmDS <- function(newobj = 'rbm',
                               data = "D",
                               monitoringdata = NULL,
                               learningrate = NULL) {

   x <- as.matrix(eval(parse(text=data)))

   trainingresult <-
      monitored_fitrbm(x,
                       monitoring = juliaExpr("monitorreconstructionerror!"))

   monitoringresult <- trainingresult[[1]]
   rbm <- trainingresult[[2]]
   assign(newobj, rbm)

   if (getOption("datashield.shareBoltzmannMachines", default = FALSE)) {
      return(list(monitoringresult, rbm))
   } else {
      return(monitoringresult)
   }
}
