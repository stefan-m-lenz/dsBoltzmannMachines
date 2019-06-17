library(opaladmin)

system("bash -e tests/move.sh")
o <- opal.login("administrator", "password",
                "http://10.5.10.57:8080")
dsadmin.set_package_methods(o, "dsBoltzmannMachines")
