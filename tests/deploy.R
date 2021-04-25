library(opalr)

devtools::build()
o <- opalr::opal.login(username = "administrator",
                       password = "password",
                       url = "http://10.5.10.57:8080")
opalr::oadmin.install_local_package(o, "../dsBoltzmannMachines_1.0.2.tar.gz")
