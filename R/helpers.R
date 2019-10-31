
decodeNumVec <- function(x) {
   as.numeric(unlist(strsplit(x, split= ",")))
}


#' Creates a call to a (Julia) function, passing only the non-null arguments as
#' key-value arguments
callWithNonNullKwargs <- function(fun, args = NULL, kwargs) {
   if (!is.null(args) && !is.list(args)) {
      args <- list(args)
   }

   # Avoid passing NULL arguments to Julia
   # Collect all the keyword arguments in the list kwargs...
   # ... to be able to filter out all the null arguments.
   kwargs <- Filter(Negate(is.null), kwargs)
   # Then the call can be assembled and evaluated.
   cally <- as.call(c(fun, args, kwargs))
   return(eval(cally, envir = .GlobalEnv))
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


asRObject <- function(x) {
   if (is.null(x)) {
      stop("Name of variable is not allowed to be NULL.")
   } else {
      return(eval(parse(text = x), envir = .GlobalEnv))
   }
}


asRObjectOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      return(eval(parse(text = x), envir = .GlobalEnv))
   }
}


asRObjectListOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      return(lapply(unlist(strsplit(x, split= ",")),
                    asRObjectOrNull))
   }
}


asBMsDataDictOrNull <- function(x) {
   if (is.null(x)) {
      return(NULL)
   } else {
      requiresJuliaPkgBoltzmannMachines()
      monitoringdata <- as.list(unlist(strsplit(x, split = ",")))
      monitoringdatalabels <- monitoringdata
      monitoringdata <- lapply(monitoringdata,
                               function(x) {as.matrix(asRObject(x))})
      monitoringdata <- juliaLet("DataDict(zip(keys, values))",
                                 keys = monitoringdatalabels,
                                 values = monitoringdata)
      return(monitoringdata)
   }
}


asMonitoringArg <- function(monitoring, monitoringopts) {
   if (is.null(monitoring)) {
      return(juliaExpr("(x...) -> nothing"))
   } else {
      monitoring <- unlist(strsplit(monitoring, split= ","))
      monitoringarg <- monitoringopts[monitoring]
      if (length(monitoring) != length(unlist(monitoringarg))) {
         stop(paste("Unrecognized monitoring option:",
                    monitoring[!(monitoring %in% names(monitoringopts))]))
      }
      if (length(monitoringarg) > 1) {
         monitoringarg <- paste0(monitoringarg, collapse = ";")
         monitoringarg <- juliaExpr(paste0("[", monitoringarg, "]"))
      } else {
         monitoringarg <- monitoringarg[[1]]
      }
      return(monitoringarg)
   }
}


assignAndReturnMonitoredFittingResult <- function(newobj, trainingresult) {
   monitoringresult <- trainingresult[[1]]
   class(monitoringresult) <- "monitoringresult"
   model <- trainingresult[[2]]
   assign(newobj, model, envir = .GlobalEnv)
   if (getOption("datashield.BoltzmannMachines.shareModels", default = FALSE)) {
      return(list(monitoringresult, model))
   } else {
      return(monitoringresult)
   }
}


checkNumberOfSamples <- function(x) {
   minRequiredTrainingSamples <- getOption("datashield.BoltzmannMachines.privacyLevel", default = 20)
   if (is.character(minRequiredTrainingSamples)) {
      minRequiredTrainingSamples <- as.numeric(minRequiredTrainingSamples)
   }
   if (nrow(x) < minRequiredTrainingSamples) {
      stop('Too few samples - see option "datashield.BoltzmannMachines.privacyLevel"')
   }
}
