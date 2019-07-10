.onLoad <- function(libname, pkgname) {
   juliaUsing("BoltzmannMachines")
}


RBM_MONITORING_OPTS <- list("reconstructionerror" = juliaExpr("monitorreconstructionerror!"),
                            "exactloglikelihood" = juliaExpr("monitorexactloglikelihood!"),
                            "loglikelihood" = juliaExpr("monitorloglikelihood!"))

monitored_fitrbmDS <- function(newobj = 'rbm',
                               data = "D",
                               monitoring = "reconstructionerror",
                               monitoringdata = NULL,
                               nhidden = NULL,
                               epochs = NULL,
                               upfactor = NULL,
                               downfactor = NULL,
                               learningrate = NULL,
                               learningrates = NULL,
                               pcd = NULL,
                               cdsteps = NULL,
                               batchsize = NULL,
                               rbmtype = NULL,
                               startrbm = NULL) {

   x <- as.matrix(eval(parse(text=data)))

   minRequiredTrainingSamples <- getOption("datashield.BoltzmannMachines.privacyLevel", default = 20)
   if (is.character(minRequiredTrainingSamples)) {
      minRequiredTrainingSamples <- as.numeric(minRequiredTrainingSamples)
   }
   if (nrow(x) < minRequiredTrainingSamples) {
      stop('Too few samples in data (see DataSHIELD option "datashield.BoltzmannMachines.privacyLevel")')
   }

   if (is.null(monitoring)) {
      monitoring <- juliaExpr("(x...) -> nothing")
   } else {
      monitoring <- RBM_MONITORING_OPTS[[monitoring]]
      if (is.null(monitoring)) {
         stop("Invalid monitoring argument")
      }
   }

   if (!is.null(monitoringdata)) {
      monitoringdata <- as.list(unlist(strsplit(monitoringdata, split = ",")))
      monitoringdatalabels <- monitoringdata
      monitoringdata <- lapply(monitoringdata, function(x) {
         eval(parse(text = x))
      })
      monitoringdata <- juliaLet("DataDict(zip(keys, values))",
                                 keys = monitoringdatalabels,
                                 values = monitoringdata)
   }

   if (!is.null(epochs)) {
      epochs <- as.integer(epochs)
   }
   if (!is.null(nhidden)) {
      nhidden <- as.integer(nhidden)
   }
   if (!is.null(learningrates)) {
      learningrates <- as.numeric(unlist(strsplit(learningrates, split=",")))
   }
   if (!is.null(cdsteps)) {
      cdsteps <- as.integer(cdsteps)
   }
   if (!is.null(batchsize)) {
      batchsize <- as.integer(batchsize)
   }
   if (!is.null(startrbm)) {
      startrbm <- eval(parse(text=startrbm))
   }
   if (!is.null(rbmtype)) {
      rbmtype <- eval(parse(text=rbmtype))
   }

   # Avoid passing NULL arguments to Julia
   # Collect all the keyword arguments in a list ...
   kwargs <- list(monitoring = monitoring,
                  monitoringdata = monitoringdata,
                  nhidden = nhidden,
                  epochs = epochs,
                  upfactor = upfactor,
                  downfactor = downfactor,
                  learningrate = learningrate,
                  learningrates = learningrates,
                  pcd = pcd,
                  cdsteps = cdsteps,
                  batchsize = batchsize,
                  rbmtype = rbmtype,
                  startrbm = startrbm)

   # ... to be able to filter out all the null arguments.
   kwargs <- Filter(Negate(is.null), kwargs)
   # Then the call can be assembled and evaluated.
   monitoredfitCall <- as.call(c(list(monitored_fitrbm, x), kwargs))
   trainingresult <- eval(monitoredfitCall)

   monitoringresult <- trainingresult[[1]]
   rbm <- trainingresult[[2]]
   assign(newobj, rbm, envir = .GlobalEnv)

   if (getOption("datashield.BoltzmannMachines.shareModels", default = FALSE)) {
      return(list(monitoringresult, rbm))
   } else {
      return(monitoringresult)
   }
}


splitdataDS <- function(data, ratio, newobj1, newobj2) {
   d1_d2 <- splitdata(as.matrix(eval(parse(text=data))), as.numeric(ratio))
   assign(newobj1, d1_d2[[1]], envir = .GlobalEnv)
   assign(newobj2, d1_d2[[2]], envir = .GlobalEnv)
   return()
}

setJuliaSeedDS <- function(seed) {
   juliaLet("using Random; Random.seed!(seed)", seed = as.integer(seed))
}


