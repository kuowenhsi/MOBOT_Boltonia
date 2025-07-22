#!/bin/bash

set -e
cd /../REUProject_2_Outgroups

for CHR in {1..1}; do
  echo "Processing Chromosome $CHR..."
 
 VCF_FILES=(./Data/Boltonia_outgroup/Boltonia_Chr_${CHR}_*.vcf.gz)

 if [[ ${#VCF_FILES[@]} -eq 0 ]]; then
  echo "  No VCF files found for chromosome $CHR"
  continue
 fi

  for VCF in "${VCF_FILES[@]}"; do
  echo " Processing file $VCF"
  FILE="./Data/PLINK_files/$(basename "$VCF" .vcf.gz)"

  # Generate PLINK files
  plink --vcf "$VCF" --make-bed --out "$FILE" --allow-extra-chr

  # Replace chromosome names with dummy values + rename SNPs
  awk -v OFS='\t' '{ $1 = "1"; $2 = "snp" NR; print }' "${FILE}.bim" > "${FILE}.bim.tmp" && mv "${FILE}.bim.tmp" "${FILE}.bim"

  # Imputation
  plink --bfile "$FILE" \
        --make-bed \
        --out "${FILE}.imputed" \
        --geno 0.1 --fill-missing-a2

  # LD Pruning
  PRUNE_OUT="${FILE}.pruned"
  plink --bfile "${FILE}.imputed" \
        --indep-pairwise 50 10 0.2 \
        --out "$PRUNE_OUT"

  plink --bfile "${FILE}.imputed" \
        --extract "${PRUNE_OUT}.prune.in" \
        --make-bed \
        --out "$PRUNE_OUT"

  echo "  Done with Chromosome $CHR."
  done
done