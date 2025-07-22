#! bin/bash

cd /../REUProject_2_Outgroups

plink --bfile ./Data/PLINK_files/Boltonia_Chr_1_1-23679183.filtered_subsetfilter_filtered_all_samples.pruned \
    --allow-extra-chr \
    --pca \
    --out ./Data/PCA/Boltonia_Chr_1_1-23679183.filtered_subsetfilter_filtered_all_samples.pruned