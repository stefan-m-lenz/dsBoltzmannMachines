.onLoad <- function(libname, pkgname) {
   juliaUsing("BoltzmannMachines")
}

decodeNumVec <- function(x) {
   as.numeric(unlist(strsplit(x, split= ",")))
}

RBM_MONITORING_OPTS <- list("reconstructionerror" = juliaExpr("monitorreconstructionerror!"),
                            "exactloglikelihood" = juliaExpr("monitorexactloglikelihood!"),
                            "loglikelihood" = juliaExpr("monitorloglikelihood!"))


#' Creates a call to a (Julia) function, passing only the non-null arguments as
#' key-value arguments
callWithNonNullKwargs <- function(fun, args, kwargs) {
   if (!is.list(args)) {
      args <- list(args)
   }

   # Avoid passing NULL arguments to Julia
   # Collect all the keyword arguments in the list kwargs...
   # ... to be able to filter out all the null arguments.
   kwargs <- Filter(Negate(is.null), kwargs)
   # Then the call can be assembled and evaluated.
   cally <- as.call(c(fun, args, kwargs))
   return(eval(cally))
}


asJuliaIntArgOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      return(as.integer(x))
   }
}

asJuliaFloat64ArgOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      return(as.numeric(x))
   }
}

asJuliaFloat64ArrayArgOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      return(as.numeric(unlist(strsplit(x, split= ","))))
   }
}

asJuliaIntArrayArgOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      return(as.integer(unlist(strsplit(x, split= ","))))
   }
}

asJuliaBoolArgOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      return(as.logical(x))
   }
}

asRObjectOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      return(eval(parse(text = x)))
   }
}


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
   d1_d2 <- splitdata(as.matrix(eval(parse(text=data))), as.numeric(ratio))
   assign(newobj1, d1_d2[[1]], envir = .GlobalEnv)
   assign(newobj2, d1_d2[[2]], envir = .GlobalEnv)
   return()
}


setJuliaSeedDS <- function(seed) {
   juliaLet("using Random; Random.seed!(seed)", seed = as.integer(seed))
}


samplesDS <- function(bm, nsamples,
                      burnin = NULL,
                      conditionIndex = NULL,
                      conditionValue = NULL,
                      samplelast = NULL) {

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


