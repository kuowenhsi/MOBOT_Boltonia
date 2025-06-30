#! /bin/bash
set -e
cd /REUProject

VCF_IN=./VCF_secondfilter/Boltonia_Chr_1_1-23679183.filtered.vcf.gz
VCF_OUT=./vcftools/Boltonia_Chr_1_1-23679183.filtered2.vcf.gz

# set filters

MAF=0
# MISS=0.9
QUAL=30
MIN_DEPTH=3
MAX_DEPTH=50

# perform the filtering with vcftools
vcftools --gzvcf $VCF_IN \
--remove-indels --maf $MAF --minQ $QUAL \
--min-meanDP $MIN_DEPTH --max-meanDP $MAX_DEPTH \
--minDP $MIN_DEPTH --maxDP $MAX_DEPTH --recode --stdout | gzip -c > \
$VCF_OUT

#the following line goes after --maf $MAF
# --max-missing $MISS