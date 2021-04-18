pkgLocal <- new.env(parent = emptyenv())

# allows lazy connection to Julia
requiresJuliaPkgBoltzmannMachines <- function() {
   if (!exists("BMs", envir = pkgLocal)) {
      # assign variable to the package environment
      pkgLocal$BMs <- juliaImport("BoltzmannMachines")
   }
}

RBM_MONITORING_OPTS <- list("reconstructionerror" = juliaExpr("BoltzmannMachines.monitorreconstructionerror!"),
                            "exactloglikelihood" = juliaExpr("BoltzmannMachines.monitorexactloglikelihood!"),
                            "loglikelihood" = juliaExpr("BoltzmannMachines.monitorloglikelihood!"))
DBM_MONITORING_OPTS <- list("logproblowerbound" = juliaExpr("BoltzmannMachines.monitorlogproblowerbound!"),
                            "exactloglikelihood" = juliaExpr("BoltzmannMachines.monitorexactloglikelihood!"))


monitored_fitrbmDS <- function(newobj = 'rbm',
                               data = "D",
                               monitoring = "reconstructionerror",
                               monitoringdata = "D",
                               nhidden = NULL,
                               epochs = NULL,
                               upfactor = NULL,
                               downfactor = NULL,
                               learningrate = NULL,
                               learningrates = NULL,
                               pcd = NULL,
                               cdsteps = NULL,
                               categories = NULL,
                               batchsize = NULL,
                               rbmtype = NULL,
                               startrbm = NULL) {

   requiresJuliaPkgBoltzmannMachines()

   x <- as.matrix(asRObject(data))
   checkNumberOfSamples(x)

   kwargs <- list(monitoring = asMonitoringArg(monitoring, RBM_MONITORING_OPTS),
                  monitoringdata = asBMsDataDictOrNull(monitoringdata),
                  nhidden = asJuliaIntArgOrNull(nhidden),
                  epochs = asJuliaIntArgOrNull(epochs),
                  upfactor = asJuliaFloat64ArgOrNull(upfactor),
                  downfactor = asJuliaFloat64ArgOrNull(downfactor),
                  learningrate = asJuliaFloat64ArgOrNull(learningrate),
                  learningrates = asJuliaFloat64ArrayArgOrNull(learningrates),
                  pcd = asJuliaBoolArgOrNull(pcd),
                  cdsteps = asJuliaIntArgOrNull(cdsteps),
                  categories = asJuliaIntArrayArgOrNull(categories),
                  batchsize = asJuliaIntArgOrNull(batchsize),
                  rbmtype = asRObjectOrNull(rbmtype),
                  startrbm = asRObjectOrNull(startrbm))

   trainingresult <- doJuliaCall(pkgLocal$BMs$monitored_fitrbm, x, kwargs)
   return(assignAndReturnMonitoredFittingResult(newobj, trainingresult))
}


splitdataDS <- function(data, ratio, newobj1, newobj2) {
   requiresJuliaPkgBoltzmannMachines()
   d1_d2 <- pkgLocal$BMs$splitdata(as.matrix(asRObject(data)), as.numeric(ratio))
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

   bm <- asRObject(bm)
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

   return(doJuliaCall(pkgLocal$BMs$samples, list(bm, nsamples), kwargs))
}


monitored_stackrbmsDS <- function(newobj,
                                  data = "D",
                                  monitoring = "reconstructionerror",
                                  monitoringdata = data,
                                  nhiddens = NULL,
                                  epochs = NULL,
                                  predbm = NULL,
                                  samplehidden = NULL,
                                  learningrate = NULL,
                                  batchsize = NULL,
                                  trainlayers = NULL) {

   requiresJuliaPkgBoltzmannMachines()

   x <- as.matrix(asRObject(data))
   checkNumberOfSamples(x)

   kwargs <- list(monitoring = asMonitoringArg(monitoring, RBM_MONITORING_OPTS),
                  monitoringdata = asBMsDataDictOrNull(monitoringdata),
                  nhiddens = asJuliaIntArrayArgOrNull(nhiddens),
                  epochs = asJuliaIntArgOrNull(epochs),
                  predbm = asJuliaBoolArgOrNull(predbm),
                  samplehidden = asJuliaBoolArgOrNull(samplehidden),
                  learningrate = asJuliaFloat64ArgOrNull(learningrate),
                  batchsize = asJuliaIntArgOrNull(batchsize),
                  trainlayers = asRObjectListOrNull(trainlayers))

   trainingresult <- doJuliaCall(pkgLocal$BMs$monitored_stackrbms, x, kwargs)
   return(assignAndReturnMonitoredFittingResult(newobj, trainingresult))
}


monitored_traindbmDS <- function(startdbm = "dbm", newobj = "dbm",
                                 data = "D",
                                 monitoring = "logproblowerbound",
                                 monitoringdata = data,
                                 epochs = NULL,
                                 nparticles = NULL,
                                 learningrate = NULL,
                                 learningrates = NULL) {

   #tryCatch({
   requiresJuliaPkgBoltzmannMachines()
   dbm <- asRObject(startdbm)
   x <- as.matrix(asRObject(data))
   checkNumberOfSamples(x)

   kwargs <- list(monitoring = asMonitoringArg(monitoring, DBM_MONITORING_OPTS),
                  monitoringdata = asBMsDataDictOrNull(monitoringdata),
                  epochs = asJuliaIntArgOrNull(epochs),
                  nparticles = asJuliaIntArgOrNull(nparticles),
                  learningrate = asJuliaFloat64ArgOrNull(learningrate),
                  learningrates = asJuliaFloat64ArrayArgOrNull(learningrates))

   trainingresult <- doJuliaCall(pkgLocal$BMs$monitored_traindbm, list(dbm, x), kwargs)
   return(assignAndReturnMonitoredFittingResult(newobj, trainingresult))
   #}, error = function(e) {return(paste(e))})
}


monitored_fitdbmDS <- function(newobj,
                               data = "D",
                               monitoring = "logproblowerbound",
                               monitoringdata = data,
                               monitoringpretraining = "reconstructionerror",
                               monitoringdatapretraining = monitoringdata,
                               nhiddens = NULL,
                               epochs = NULL,
                               nparticles = NULL,
                               learningrate = NULL,
                               learningrates = NULL,
                               learningratepretraining = NULL,
                               epochspretraining = NULL,
                               batchsizepretraining = NULL,
                               pretraining = NULL) {


   requiresJuliaPkgBoltzmannMachines()

   x <- as.matrix(asRObject(data))
   checkNumberOfSamples(x)

   kwargs <- list(monitoring = asMonitoringArg(monitoring, DBM_MONITORING_OPTS),
                  monitoringdata = asBMsDataDictOrNull(monitoringdata),
                  monitoringpretraining = asMonitoringArg(monitoringpretraining, RBM_MONITORING_OPTS),
                  monitoringdatapretraining = asBMsDataDictOrNull(monitoringdatapretraining),
                  nhiddens = asJuliaIntArrayArgOrNull(nhiddens),
                  epochs = asJuliaIntArgOrNull(epochs),
                  nparticles = asJuliaIntArgOrNull(nparticles),
                  learningrate = asJuliaFloat64ArgOrNull(learningrate),
                  learningrates = asJuliaFloat64ArrayArgOrNull(learningrates),
                  learningratepretraining = asJuliaFloat64ArgOrNull(learningratepretraining),
                  epochspretraining = asJuliaIntArgOrNull(epochspretraining),
                  batchsizepretraining = asJuliaIntArgOrNull(batchsizepretraining),
                  pretraining = asRObjectListOrNull(pretraining))

   trainingresult <- doJuliaCall(pkgLocal$BMs$monitored_fitdbm, x, kwargs)
   return(assignAndReturnMonitoredFittingResult(newobj, trainingresult))
}


bm.defineLayerDS <- function(newobj, epochs = NULL,
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

   t <- doJuliaCall(pkgLocal$BMs$TrainLayer, kwargs = kwargs)
   assign(newobj, t, envir = .GlobalEnv)

   return()
}


bm.definePartitionedLayerDS <- function(newobj, parts) {
   parts <- asRObjectListOrNull(parts)
   if (!is.null(parts) || isempty(parts)) {
      parts <- doJuliaCall(pkgLocal$BMs$TrainPartitionedLayer, list(parts))
   }
   assign(newobj, parts, envir = .GlobalEnv)
   return()
}


#' If there are more than two hidden nodes, perform a PCA and return only
#' the top two principal components.
dbm.top2LatentDimsDS <- function(dbm, data) {
   mf <- doJuliaCall(pkgLocal$BMs$meanfield, list(asRObject(dbm), as.matrix(asRObject(data))))
   h <- mf[[length(mf)]]

   # logit transform
   h <- -log(1 / h - 1)

   if (ncol(h) > 2) {
      comps <- prcomp(h, scale = TRUE)
      h <- comps$x[, 1:2]
   }

   # random permutation of rows to increase data protection
   h <- h[sample.int(nrow(h), replace = FALSE), ]

   return(h)
}


aisParamList <- function(parallelized = NULL,
                         ntemperatures = NULL,
                         nparticles = NULL,
                         burnin = NULL) {
   list(parallelized = asJuliaBoolArgOrNull(parallelized),
        ntemperatures = asJuliaIntArgOrNull(ntemperatures),
        nparticles = asJuliaIntArgOrNull(nparticles),
        burnin = asJuliaIntArgOrNull(burnin))
}


rbm.loglikelihoodDS <- function(rbm, data, ...) {
   requiresJuliaPkgBoltzmannMachines()
   kwargs <- aisParamList(...)
   rbm <- asRObject(rbm)
   logz <- doJuliaCall(pkgLocal$BMs$logpartitionfunction, list(rbm), kwargs)
   doJuliaCall(pkgLocal$BMs$loglikelihood, list(rbm, as.matrix(asRObject(data)), logz))
}


dbm.loglikelihoodDS <- function(dbm, data, ...) {
   requiresJuliaPkgBoltzmannMachines()
   kwargs <- aisParamList(...)
   doJuliaCall(pkgLocal$BMs$loglikelihood, list(asRObject(dbm), as.matrix(asRObject(data))), kwargs)
}


dbm.logproblowerboundDS <- function(dbm, data, ...) {
   requiresJuliaPkgBoltzmannMachines()
   kwargs <- aisParamList(...)
   dbm <- asRObject(dbm)
   logz <- doJuliaCall(pkgLocal$BMs$logpartitionfunction, list(dbm), kwargs)
   doJuliaCall(pkgLocal$BMs$logproblowerbound, list(dbm, as.matrix(asRObject(data)), logz))
}


bm.exactloglikelihoodDS <- function(bm, data) {
   requiresJuliaPkgBoltzmannMachines()
   doJuliaCall(pkgLocal$BMs$exactloglikelihood, list(asRObject(bm), as.matrix(asRObject(data))))
}
