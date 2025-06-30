#! /bin/bash
set -e
cd /REUProject

FILE=./vcftools/Boltonia_Chr_1_1-23679183.filtered2

# # Generate the input file in plink format
plink --vcf $FILE.vcf.gz --make-bed --out $FILE --allow-extra-chr

# # ADMIXTURE does not accept chromosome names that are not human chromosomes. We will thus just exchange the first column by 0
awk -v OFS='\t' '{ $1 = "1"; $2 = "snp" NR; print }' $FILE.bim > $FILE.bim.tmp && mv $FILE.bim.tmp $FILE.bim

#the following line needs to be run in PLINK 1.9
plink --bfile $FILE --make-bed --out $FILE.imputed --geno 0.1 --mind 0.1 --fill-missing-a2

#LD Prunning
plink --bfile $FILE.imputed \
  --indep-pairwise 50 10 0.1 \
  --out Boltonia_Chr_1_1-23679183_pruned_data
plink --bfile $FILE.imputed \
  --extract Boltonia_Chr_1_1-23679183_pruned_data.prune.in \
  --make-bed \
  --out Boltonia_Chr_1_1-23679183_pruned_data


# the following line needs to be run in admixture
# admixture --cv=10 -j1 Boltonia_Chr_1_1-23679183_pruned_data.bed 2


