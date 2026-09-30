#! /bin/bash

SnpSift() { java -Xmx8g -jar /storage1/fs1/christine.e.edwards/Active/Wen/IMLS_Ref/snpeff/SnpSift.jar "$@"; }

# SnpSift filter "( SIFTINFO[*] has 'NONSYNONYMOUS' )" Boltonia_decurrens_imputed_SIFTpredictions.vcf > Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS.vcf

# SnpSift filter "( SIFTINFO[*] has 'NONSYNONYMOUS' ) & ( SIFTINFO[*] has 'DELETERIOUS' )" Boltonia_decurrens_imputed_SIFTpredictions.vcf > Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS_DELETERIOUS.vcf

SnpSift filter "( SIFTINFO[*] has 'NONSYNONYMOUS' ) & ( SIFTINFO[*] has 'TOLERATED' )" Boltonia_decurrens_imputed_SIFTpredictions.vcf > Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS_TOLERATED.vcf

SnpSift filter "( SIFTINFO[*] has 'SYNONYMOUS' )" Boltonia_decurrens_imputed_SIFTpredictions.vcf > Boltonia_decurrens_imputed_SIFTpredictions_SYNONYMOUS.vcf
