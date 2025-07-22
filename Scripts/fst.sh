#!/bin/bash
set -e

cd ./../../REUProject_LargeFiles/Data/PCadapt_output

#Need to run r code between to add population labels to file (add_pop_to_psam.R)

sed -i '1s/^FID/#FID/' Boltonia_decurrens_100kb0.8.withpop.psam

plink2 \
  --pfile Boltonia_decurrens_100kb0.8 \
  --psam Boltonia_decurrens_100kb0.8.withpop.psam \
  --fst Sample_Group \
  --out ./../../../MOBOT_Boltonia/Data/Boltonia_decurrens_100kb0.8 \
  --allow-extra-chr \
