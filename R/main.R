
# allows lazy connection to Julia
pkgLocal <- new.env(parent = emptyenv())
requiresJuliaPkgBoltzmannMachines <- function() {
   if (is.null(pkgLocal$BoltzmannMachinesLoaded)) {
      juliaUsing("BoltzmannMachines")
      pkgLocal$BoltzmannMachinesLoaded <- TRUE
   }
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

   requiresJuliaPkgBoltzmannMachines()

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
      #monitoring <- unlist(strsplit(x, split= ","))
      #for (m in monitoring) {}
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

   epochs <- asJuliaIntArgOrNull(epochs)
   nhidden <- asJuliaIntArgOrNull(nhidden)
   learningrates <- asJuliaFloat64ArrayArgOrNull(learningrates)
   cdsteps <- asJuliaIntArgOrNull(cdsteps)
   batchsize <- asJuliaIntArgOrNull(batchsize)
   startrbm <- asRObjectOrNull(startrbm)
   rbmtype <- asRObjectOrNull(rbmtype)

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

   trainingresult <- callWithNonNullKwargs(monitored_fitrbm, x, kwargs)
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
   requiresJuliaPkgBoltzmannMachines()
   d1_d2 <- splitdata(as.matrix(eval(parse(text=data))), as.numeric(ratio))
   assign(newobj1, d1_d2[[1]], envir = .GlobalEnv)
   assign(newobj2, d1_d2[[2]], envir = .GlobalEnv)
   return()
}


setJuliaSeedDS <- function(seed) {
   requiresJuliaPkgBoltzmannMachines()
   juliaLet("using Random; Random.seed!(seed)", seed = as.integer(seed))
}


samplesDS <- function(bm, nsamples,
                      burnin = NULL,
                      conditionIndex = NULL,
                      conditionValue = NULL,
                      samplelast = NULL) {

   requiresJuliaPkgBoltzmannMachines()

   bm <- eval(parse(text = bm))
   nsamples <- as.integer(nsamples)
   burnin <- asJuliaIntArgOrNull(burnin)
   samplelast <- asJuliaBoolArgOrNull(samplelast)

   if (is.null(conditionIndex)) {
      conditions <- NULL
   } else {
      conditionIndex <- as.integer(decodeNumVec(conditionIndex))
      conditionValue <- decodeNumVec(conditionValue)
      if (length(conditionIndex) != length(conditionValue)) {
         stop("conditionIndex and conditionValue must have same length.")
      }
      conditions <- juliaLet("[k => v for (k, v) in zip(x, y)]",
                             x = conditionIndex, y = conditionValue)
   }

   kwargs <- list(burnin = burnin,
                  conditions = conditions,
                  samplelast = samplelast)

   return(callWithNonNullKwargs(samples, list(bm, nsamples), kwargs))
}


defineLayerDS <- function(epochs = NULL,
                           learningrate = NULL,
                           learningrates = NULL,
                           sdlearningrate = NULL,
                           sdlearningrates = NULL,
                           categories = NULL,
                           monitoring = NULL,
                           rbmtype = NULL,
                           nhidden = NULL,
                           nvisible = NULL,
                           batchsize = NULL,
                           pcd = NULL,
                           cdsteps = NULL,
                           startrbm = NULL) {

   return(list(epochs = epochs,
               learningrate = learningrate,
               learningrates = learningrates,
               sdlearningrate = sdlearningrate,
               sdlearningrates = sdlearningrates,
               categories = categories,
               monitoring = monitoring,
               rbmtype = rbmtype,
               nhidden = nhidden,
               nvisible = nvisible,
               batchsize = batchsize,
               pcd = pcd,
               cdsteps = cdsteps,
               startrbm = startrbm))
}


