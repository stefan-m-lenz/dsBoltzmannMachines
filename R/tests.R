# should error
library(tools)
assertError(dsBoltzmannMachines:::asMonitoringArg("notarealthing", dsBoltzmannMachines:::DBM_MONITORING_OPTS))
D <- matrix(c(1,2,3,4), nrow=2)
#dsBoltzmannMachines:::asBMsDataDictOrNull("D")
