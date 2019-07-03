library(opaladmin)

devtools::build()
system("bash -e tests/move.sh")

# In case of new functions:
o <- opal.login("administrator", "password",
                "http://10.5.10.57:8080")
dsadmin.set_package_methods(o, "dsBoltzmannMachines")

# install or Update JuliaConnectorR
devtools::build("../JuliaConnectoR")
system("bash -e tests/updateJuliaConnector.sh")


#system('bash -c "ssh root@opal service rserver restart"')
