scp ../dsBoltzmannMachines*.tar.gz root@opal:
ssh root@opal <<'ENDSSH'
  R -e 'install.packages("dsBoltzmannMachines_0.1.0.tar.gz", repos = NULL, type="source")'
ENDSSH
