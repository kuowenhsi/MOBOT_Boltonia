#! bin/bash

cd ./../Data

plink --bfile ./ADMIXTURE_hybrid/Boltonia_hybrid_ID_fillmissing_LD \
    --allow-extra-chr \
    --pca \
    --out ./ADMIXTURE_hybrid/Boltonia_hybrid_ID_fillmissing_LD