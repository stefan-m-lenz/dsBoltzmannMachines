library(opaladmin)

devtools::build()
system("bash -e tests/move.sh")
o <- opal.login("administrator", "password",
                "http://10.5.10.57:8080")
dsadmin.set_package_methods(o, "dsBoltzmannMachines")
system('bash -c "ssh root@opal service rserver restart"')
