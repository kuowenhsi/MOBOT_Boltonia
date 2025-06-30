#docker run -it -v "C:\Users\User\Desktop\REUProject:/REUProject/" kuowenhsi/wen_gatk:2.0

#! /bin/bash
set -e
cd /REUProject
INPUT_BAM=$1
OUTPUT_GVCF=$(basename $INPUT_BAM "_merged.HQ.RG.bam")

echo $INPUT_BAM
echo $OUTPUT_GVCF

gatk --java-options "-Xmx3g" HaplotypeCaller \
    -R ./Boltonia_toy/Boltonia_hap1_1.0.fasta \
    -I ./Boltonia_toy/$INPUT_BAM \
    -O ./Boltonia_GVCF/$OUTPUT_GVCF.vcf.gz \
    -ERC GVCF