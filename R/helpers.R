
decodeNumVec <- function(x) {
   as.numeric(unlist(strsplit(x, split= ",")))
}


extractJuliaErrorType <- function(errorMsg) {
   paste(regmatches(errorMsg, regexpr("[a-zA-Z0-9]*Error[a-zA-Z0-9]*", errorMsg)),
         collapse = ", ")
}


logJuliaError <- function(errorMsg, funname, args, kwargs) {
   logfile <- getOption("dsBoltzmannMachines.logFile", default = "")
   activeDebug <- as.logical(getOption("dsBoltzmannMachines.debug", default = FALSE))
   if (logfile != "") {
      if (!file.exists(logfile)) {
         file.create(logfile)
      }
      if (file.access(logfile, mode = 0) < 0) { # file writable
         stop(paste0("Error while attempting to log error: Log-file\"", logfile, "\" not writable."))
      } else {
         getTypeInfo <- function(x) {
            ret <- juliaCall("typeof", x)
            rTypeInfo <- paste0("\"", c(typeof(x), class(x)), "\"")
            rTypeInfo <- paste(rTypeInfo, collapse = "/")
            paste0(rTypeInfo, " -> ::", ret)
         }
         argTypes <- lapply(args, getTypeInfo)
         argTypes <- paste0(argTypes, collapse = ", ")
         kwargsAndTypes <- paste(names(kwargs),
                                 lapply(kwargs, getTypeInfo),
                                 collapse = ", ")
         logEntry <- paste0("Julia error: ", extractJuliaErrorType(errorMsg), " calling ",
                            funname,
                            "(", argTypes, "; ", kwargsAndTypes, ")")
         if (activeDebug) {
            # log everything - this must be turned on explicitly - may contain patient information!
            logEntry <- paste(logEntry, errorMsg, sep = " - ")
         }
         write(paste0(Sys.time(), " ", logEntry), file = logfile, append = TRUE)
      }
   }
}


handleJuliaErrorAndStop <- function(e, funname, args, kwargs) {
   errorMsg <- conditionMessage(e)
   logJuliaError(errorMsg, funname, args, kwargs)
   stop(paste0("Julia returned an error (", extractJuliaErrorType(errorMsg),
               "). If logging is enabled, see details there."))
}


#' Calls a wrapped Julia function.
#'
#' Only the the arguments with non-null value in kwargs are passed as
#' named arguments.
#'
#' If Julia returns an error, the error may be logged.
#'
doJuliaCall <- function(fun, args = NULL, kwargs = NULL) {
   if (!is.null(args) && !is.list(args)) {
      args <- list(args)
   }

   # Avoid passing NULL arguments to Julia
   # Collect all the keyword arguments in the list kwargs...
   # ... to be able to filter out all the null arguments.
   if (!is.null(kwargs)) {
      kwargs <- Filter(Negate(is.null), kwargs)
   }

   # Then the call can be assembled and evaluated.
   cally <- as.call(c(fun, args, kwargs))
   funname <- substitute(fun)
   tryCatch({return(eval(cally, envir = .GlobalEnv))},
            error = function(e) {handleJuliaErrorAndStop(e, funname, args, kwargs)})
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
   if (getOption("dsBoltzmannMachines.shareModels", default = FALSE)) {
      return(list(monitoringresult, model))
   } else {
      return(monitoringresult)
   }
}


checkNumberOfSamples <- function(x) {
   minRequiredTrainingSamples <- getOption("dsBoltzmannMachines.privacyLevel", default = 20)
   if (is.character(minRequiredTrainingSamples)) {
      minRequiredTrainingSamples <- as.numeric(minRequiredTrainingSamples)
   }
   if (nrow(x) < minRequiredTrainingSamples) {
      stop('Too few samples - see option "dsBoltzmannMachines.privacyLevel"')
   }
}
