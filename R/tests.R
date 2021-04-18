# should error
library(tools)
assertError(dsBoltzmannMachines:::asMonitoringArg("notarealthing", dsBoltzmannMachines:::DBM_MONITORING_OPTS))
D <- as.double(matrix(rbinom(4*20, 1, 0.2), ncol=4))
#dsBoltzmannMachines:::requiresJuliaPkgBoltzmannMachines()
#dsBoltzmannMachines:::asBMsDataDictOrNull("D")

#dsBoltzmannMachines:::setJuliaSeedDS(2)
# test log
testlog <- FALSE
if (testlog) {
   testlogfile <- "./test-log.log"
   library(JuliaConnectoR)
   BMs <- juliaImport("BoltzmannMachines")
   options("dsBoltzmannMachines.logFile" = "./test-log.log")
   options("dsBoltzmannMachines.debug" = FALSE)
   assertError(dsBoltzmannMachines:::doJuliaCall(BMs$exactloglikelihood, list("+", "1")))
   assertError(dsBoltzmannMachines:::doJuliaCall(BMs$exactloglikelihood, list("+", "1"), list(a = "2", b = "3")))
   length(readLines(testlogfile)) == 2
   file.remove(testlogfile)

   options("dsBoltzmannMachines.debug" = TRUE)
   assertError(dsBoltzmannMachines:::doJuliaCall(BMs$exactloglikelihood, list("+", "1")))
   length(readLines(testlogfile)) == 1
   file.remove(testlogfile)
}

