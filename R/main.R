
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
      stop('Too few samples - see option "datashield.BoltzmannMachines.privacyLevel"')
   }

   monitoring <- asMonitoringArg(monitoring, RBM_MONITORING_OPTS)

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
   return()
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


stackrbmsDS <- function(data,
                        nhiddens = NULL,
                        epochs = NULL,
                        predbm = NULL,
                        samplehidden = NULL,
                        learningrate = NULL,
                        batchsize = NULL,
                        trainlayers = NULL,
                        monitoringdata = NULL) {

   requiresJuliaPkgBoltzmannMachines()

   x <- as.matrix(eval(parse(text=data)))
   nhiddens <- asJuliaIntArrayArgOrNull(nhiddens)
   epochs <- asJuliaIntArgOrNull(epochs)
   predbm <- asJuliaBoolArgOrNull(predbm)
   samplehidden <- asJuliaBoolArgOrNull(samplehidden)
   learningrate <- asJuliaFloat64ArgOrNull(learningrate)
   batchsize <- asJuliaIntArgOrNull(batchsize)
   trainlayers <- NULL

   kwargs <- list(nhiddens = nhiddens,
                  epochs = epochs,
                  predbm = predbm,
                  samplehidden = samplehidden,
                  learningrate = learningrate,
                  batchsize = batchsize,
                  trainlayers = trainlayers,
                  # TODOmonitoringdata = NULL
                  )

   stackrbms()
}


defineLayerDS <- function(newobj, epochs = NULL,
                          learningrate = NULL,
                          learningrates = NULL,
                          sdlearningrate = NULL,
                          sdlearningrates = NULL,
                          categories = NULL,
                          rbmtype = NULL,
                          nhidden = NULL,
                          nvisible = NULL,
                          batchsize = NULL,
                          pcd = NULL,
                          cdsteps = NULL,
                          startrbm = NULL) {

   requiresJuliaPkgBoltzmannMachines()

   kwargs <- list(epochs = asJuliaIntArgOrNull(epochs),
                  learningrate = asJuliaFloat64ArgOrNull(learningrate),
                  learningrates = asJuliaFloat64ArrayArgOrNull(learningrates),
                  sdlearningrate = asJuliaFloat64ArgOrNull(sdlearningrate),
                  sdlearningrates = asJuliaFloat64ArrayArgOrNull(sdlearningrates),
                  categories = asJuliaIntArrayArgOrNull(categories),
                  rbmtype = asRObjectOrNull(rbmtype),
                  nhidden = asJuliaIntArgOrNull(nhidden),
                  nvisible = asJuliaIntArgOrNull(nvisible),
                  batchsize = asJuliaIntArgOrNull(batchsize),
                  pcd = asJuliaBoolArgOrNull(pcd),
                  cdsteps = asJuliaIntArgOrNull(cdsteps),
                  startrbm = asRObjectOrNull(startrbm))

   t <- callWithNonNullKwargs(TrainLayer, kwargs = kwargs)
   assign(newobj, t)

   return()
}


