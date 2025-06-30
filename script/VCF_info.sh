#! /bin/bash
set -e
cd /REUProject

SUBSET_VCF=./VCF_secondfilter/Boltonia_Chr_1_1-23679183.filtered.vcf.gz
OUT=./vcftools

vcftools --gzvcf $SUBSET_VCF --freq2 --out $OUT --max-alleles 2
vcftools --gzvcf $SUBSET_VCF --depth --out $OUT
vcftools --gzvcf $SUBSET_VCF --site-mean-depth --out $OUT
vcftools --gzvcf $SUBSET_VCF --site-quality --out $OUT
vcftools --gzvcf $SUBSET_VCF --missing-indv --out $OUT
vcftools --gzvcf $SUBSET_VCF --missing-site --out $OUT
vcftools --gzvcf $SUBSET_VCF --het --out $OUT
