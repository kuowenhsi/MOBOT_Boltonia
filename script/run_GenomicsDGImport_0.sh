#!/bin/bash
cd /REUProject

gatk --java-options "-Xmx3g -Xms3g" GenomicsDBImport \
-V ./Boltonia_GVCF/Boltonia_001.vcf.gz \
--genomicsdb-workspace-path Boltonia_genomicDB_GVCF_0 \
--intervals Chr_1:1-10000000 \
--reader-threads 4

gatk --java-options "-Xmx3g -Xms3g" GenotypeGVCFs \
-R ./Boltonia_toy/Boltonia_hap1_1.0.fasta \
-V gendb://Boltonia_genomicDB_GVCF_0 \
-O Boltonia_Chr_1_10000000.vcf.gz
