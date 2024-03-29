# Voraussetzung: Ubuntu 18.04


######################
# Opal-Installation
# http://wiki.obiba.org/display/OPALDOC/How+to+install+and+use+Opal+and+DataSHIELD+for+Data+Harmonization+and+Federated+Analysis
######################

# !!!!!!! Folgende Variablen bitte anpassen !!!!!!!
ADMIN_PASSWORD='password'

# MongoDB 3.4 installieren
# Das ist eigentlich veraltet. Aber egal, es funktioniert erst mal.
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 9DA31620334BD75D9DCB49F368818C72E52529D4
echo "deb [ arch=amd64 ] https://repo.mongodb.org/apt/ubuntu bionic/mongodb-org/4.0 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-4.0.list
sudo apt-get update
sudo apt-get install -y mongodb-org

# Daten-Verzeichnis für MongoDB-Dienst anlegen
sudo mkdir /data
sudo mkdir /data/db
sudo chown -c mongodb /data/db

sudo service mongod start

# MongoDB-Benutzer anlegen, für identifier database and data database
cat << EOF | mongo
use admin
db.createUser(
  {
    user: "userAdmin",
    pwd: "$ADMIN_PASSWORD",
    roles: [ { role: "root", db: "admin" } ]
  }
);

use opal_data
db.createUser(
  {
    user: "opal",
    pwd: "$ADMIN_PASSWORD",
    roles: [
      {
        "role" : "readWrite",
        "db" : "opal_ids"
      },
      {
        "role" : "dbAdmin",
        "db" : "opal_ids"
      },
      {
        "role" : "readWrite",
        "db" : "opal_data"
      },
      {
        "role" : "dbAdmin",
        "db" : "opal_data"
      },
      {
          "role": "clusterMonitor",
          "db": "admin"
      },
      {
          "role": "readAnyDatabase",
          "db": "admin"
      }
    ]
  }
)

use opal_ids
db.createUser(
  {
    user: "opal",
    pwd: "$ADMIN_PASSWORD",
    roles: [
      {
        "role" : "readWrite",
        "db" : "opal_ids"
      },
      {
        "role" : "dbAdmin",
        "db" : "opal_ids"
      },
      {
        "role" : "readWrite",
        "db" : "opal_data"
      },
      {
        "role" : "dbAdmin",
        "db" : "opal_data"
      },
      {
          "role": "clusterMonitor",
          "db": "admin"
      },
      {
          "role": "readAnyDatabase",
          "db": "admin"
      }
    ]
  }
)
exit;
EOF

sudo systemctl enable mongod.service
# Hier keine Absicherung der MongoDB

# Jave-Runtime
sudo apt-get install -y openjdk-8-jre

# Opal installieren:
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 379CE192D401AB61
echo 'deb https://dl.bintray.com/obiba/deb all main' | sudo tee /etc/apt/sources.list.d/obiba.list

sudo apt-get update
sudo apt-get install -y opal # Opal-Adminipasswordstator-Passwort eingeben (ADMIN_PASSWORD)
sudo apt-get install -y opal-python-client
sudo apt-get install -y opal-rserver
sudo service rserver restart

# ACHTUNG MANUELLE ARBEIT:
# * Seite https://<opal-server-ip>:8443 aufrufen
# * Einloggen als "administrator", Passwort ADMIN_PASSWORD
# * "Post-Install Configuration" durchführen:
#    + Register Identifiers Database: MongoDB
#      (Username "opal", Password ADMIN_PASSWORD)
#    + Register Data Databases: MongoDB
#      (Name "MongoDB", Häkchen bei "Project default storage",
#      Username "opal", Password ADMIN_PASSWORD)
# * Weitere R-Pakete installieren:
#   Administration -> DataSHIELD -> Add Package -> Install all DataSHIELD packages
# * Test-Projekt "TestProjekt" anlegen und Datei "LifeLines.sav" importieren,
#   siehe DataSHIELD-Tutorial Punkte 2.2.2 und 2.2.3:
#   https://wiki.obiba.org/display/OPALDOC/How+to+install+and+use+Opal+and+DataSHIELD+for+Data+Harmonization+and+Federated+Analysis#HowtoinstallanduseOpalandDataSHIELDforDataHarmonizationandFederatedAnalysis-2.2SetuptheDataforTesting


# Root-Zugang per SSH:
# https://wiki.linuxmuster.net/community/anwenderwiki:ssh:ssh-keys

# Install Julia
JULIA_VERSION=julia-1.0.4
wget https://julialang-s3.julialang.org/bin/linux/x64/1.0/$JULIA_VERSION-linux-x86_64.tar.gz
tar -xzf $JULIA_VERSION-linux-x86_64.tar.gz
rm $JULIA_VERSION-linux-x86_64.tar.gz
mkdir /opt/julia
mv $JULIA_VERSION /opt/julia/$JULIA_VERSION/
ln -s /opt/julia/$JULIA_VERSION/bin/julia /usr/local/bin/julia

# Set JULIA_BINDIR environment variable for the rserver
BOLTZMANN_UPDATE_CMD=$(cat << 'END'
julia -e 'using Pkg; Pkg.add(PackageSpec(name = "BoltzmannMachines", version = "1.2"))'
END
)
sudo su -s "/bin/bash" rserver -c "$BOLTZMANN_UPDATE_CMD"
JULIA_BINDIR="/opt/julia/$JULIA_VERSION/bin/"
echo "JULIA_BINDIR=$JULIA_BINDIR" >> /etc/environment
printf "\nSys.setenv(JULIA_BINDIR=\"$JULIA_BINDIR\")\n\n" >> /var/lib/rserver/conf/Rprofile.R
sudo service rserver restart


# (Files need to be copied first)
R -e 'install.packages("JuliaConnectoR_0.2.0.9000.tar.gz", repos = NULL, type = "source")'
R -e 'install.packages("dsBoltzmannMachines_0.1.0.tar.gz", repos = NULL, type = "source")'

Rscript -e 'install.packages("opalr", repos="https://ftp.fau.de/cran/")'
