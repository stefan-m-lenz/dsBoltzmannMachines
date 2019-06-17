monitored_fitrbmDS <- function(newobj = 'dbm', ...) {
  trainingresult <- monitored_fitdbm(...)
  monitoringresult <- trainingresult[[1]]
  dbm <- trainingresult[[2]]
  assign(newobj, dbm)
  return(monitoringresult)
}
