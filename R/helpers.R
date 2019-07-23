
decodeNumVec <- function(x) {
   as.numeric(unlist(strsplit(x, split= ",")))
}


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
