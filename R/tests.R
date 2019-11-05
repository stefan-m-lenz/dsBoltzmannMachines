# should error
library(tools)
assertError(dsBoltzmannMachines:::asMonitoringArg("notarealthing", dsBoltzmannMachines:::DBM_MONITORING_OPTS))
D <- matrix(c(1,2,3,4), nrow=2)
#dsBoltzmannMachines:::asBMsDataDictOrNull("D")

# test log
testlog <- FALSE
if (testlog) {
   testlogfile <- "./test-log.log"
   library(JuliaConnectoR)
   juliaUsing("BoltzmannMachines")
   options("dsBoltzmannMachines.logFile" = "./test-log.log")
   options("dsBoltzmannMachines.debug" = FALSE)
   assertError(dsBoltzmannMachines:::doJuliaCall(exactloglikelihood, list("+", "1")))
   assertError(dsBoltzmannMachines:::doJuliaCall(exactloglikelihood, list("+", "1"), list(a = "2", b = "3")))
   length(readLines(testlogfile)) == 2
   file.remove(testlogfile)

   options("dsBoltzmannMachines.debug" = TRUE)
   assertError(dsBoltzmannMachines:::doJuliaCall(exactloglikelihood, list("+", "1")))
   length(readLines(testlogfile)) == 1
   file.remove(testlogfile)
}

