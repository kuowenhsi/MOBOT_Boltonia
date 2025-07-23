# /bin/bash

## Calculate pairwise population level Fst

plink2 --pfile Boltonia_decurrens_100kb0.8 --psam Boltonia_decurrens_100kb0.8_meta.psam --fst MaternalLine --out Boltonia_decurrens_100kb0.8_MaternalLine_fst
	# PLINK v2.00a5 M1 (16 May 2023)                 www.cog-genomics.org/plink/2.0/
	# (C) 2005-2023 Shaun Purcell, Christopher Chang   GNU General Public License v3
	# Logging to Boltonia_decurrens_100kb0.8_MaternalLine_fst.log.
	# Options in effect:
	# --fst MaternalLine
	# --out Boltonia_decurrens_100kb0.8_MaternalLine_fst
	# --pfile Boltonia_decurrens_100kb0.8
	# --psam Boltonia_decurrens_100kb0.8_meta.psam

	# Start time: Mon Jul 21 21:34:33 2025
	# 16384 MiB RAM detected; reserving 8192 MiB for main workspace.
	# Using up to 10 threads (change this with --threads).
	# 424 samples (0 females, 0 males, 424 ambiguous; 424 founders) loaded from
	# Boltonia_decurrens_100kb0.8_meta.psam.
	# 1536138 variants loaded from Boltonia_decurrens_100kb0.8.pvar.
	# 44 phenotypes loaded (1 binary, 41 quantitative, 2 categorical).
	# --fst: Analyzing 424 samples across 18 populations.
	# Autosomal --fst: done.
	# Autosomal --fst: Summary written to
	# Boltonia_decurrens_100kb0.8_MaternalLine_fst.fst.summary .
	# End time: Mon Jul 21 21:34:34 2025

plink2 --pfile Boltonia_decurrens_100kb0.8 --psam Boltonia_decurrens_100kb0.8_meta.psam --fst County --out Boltonia_decurrens_100kb0.8_County_fst
	# PLINK v2.00a5 M1 (16 May 2023)                 www.cog-genomics.org/plink/2.0/
	# (C) 2005-2023 Shaun Purcell, Christopher Chang   GNU General Public License v3
	# Logging to Boltonia_decurrens_100kb0.8_County_fst.log.
	# Options in effect:
	# --fst County
	# --out Boltonia_decurrens_100kb0.8_County_fst
	# --pfile Boltonia_decurrens_100kb0.8
	# --psam Boltonia_decurrens_100kb0.8_meta.psam

	# Start time: Mon Jul 21 21:36:21 2025
	# 16384 MiB RAM detected; reserving 8192 MiB for main workspace.
	# Using up to 10 threads (change this with --threads).
	# 424 samples (0 females, 0 males, 424 ambiguous; 424 founders) loaded from
	# Boltonia_decurrens_100kb0.8_meta.psam.
	# 1536138 variants loaded from Boltonia_decurrens_100kb0.8.pvar.
	# 44 phenotypes loaded (1 binary, 41 quantitative, 2 categorical).
	# --fst: Analyzing 424 samples across 15 populations.
	# Autosomal --fst: done.
	# Autosomal --fst: Summary written to
	# Boltonia_decurrens_100kb0.8_County_fst.fst.summary .
	# End time: Mon Jul 21 21:36:22 2025

plink2 --pfile Boltonia_decurrens_100kb0.8 --psam Boltonia_decurrens_100kb0.8_meta.psam --het small-sample --keep-if MaternalLine==M2011-2599-1 --out Boltonia_decurrens_100kb0.8_het
	# PLINK v2.00a5 M1 (16 May 2023)                 www.cog-genomics.org/plink/2.0/
	# (C) 2005-2023 Shaun Purcell, Christopher Chang   GNU General Public License v3
	# Logging to Boltonia_decurrens_100kb0.8_het.log.
	# Options in effect:
	# --het small-sample
	# --keep-if MaternalLine==M2011-2599-1
	# --out Boltonia_decurrens_100kb0.8_het
	# --pfile Boltonia_decurrens_100kb0.8
	# --psam Boltonia_decurrens_100kb0.8_meta.psam

	# Start time: Mon Jul 21 21:54:40 2025
	# 16384 MiB RAM detected; reserving 8192 MiB for main workspace.
	# Using up to 10 threads (change this with --threads).
	# 424 samples (0 females, 0 males, 424 ambiguous; 424 founders) loaded from
	# Boltonia_decurrens_100kb0.8_meta.psam.
	# 1536138 variants loaded from Boltonia_decurrens_100kb0.8.pvar.
	# 44 phenotypes loaded (1 binary, 41 quantitative, 2 categorical).
	# --keep-if: 404 samples removed.
	# 20 samples (0 females, 0 males, 20 ambiguous; 20 founders) remaining after main
	# filters.
	# --het: done.
	# Warning: 296621 autosomal variants skipped because they were monomorphic.
	# --het: Results written to Boltonia_decurrens_100kb0.8_het.het .
	# End time: Mon Jul 21 21:54:41 2025

plink2 --pfile Boltonia_decurrens_100kb0.8 --psam Boltonia_decurrens_100kb0.8_meta.psam --make-king-table  --out Boltonia_decurrens_100kb0.8_king
	# PLINK v2.00a5 M1 (16 May 2023)                 www.cog-genomics.org/plink/2.0/
	# (C) 2005-2023 Shaun Purcell, Christopher Chang   GNU General Public License v3
	# Logging to Boltonia_decurrens_100kb0.8_king.log.
	# Options in effect:
	# --make-king-table
	# --out Boltonia_decurrens_100kb0.8_king
	# --pfile Boltonia_decurrens_100kb0.8
	# --psam Boltonia_decurrens_100kb0.8_meta.psam

	# Start time: Mon Jul 21 22:09:37 2025
	# 16384 MiB RAM detected; reserving 8192 MiB for main workspace.
	# Using up to 10 threads (change this with --threads).
	# 424 samples (0 females, 0 males, 424 ambiguous; 424 founders) loaded from
	# Boltonia_decurrens_100kb0.8_meta.psam.
	# 1536138 variants loaded from Boltonia_decurrens_100kb0.8.pvar.
	# 44 phenotypes loaded (1 binary, 41 quantitative, 2 categorical).
	# --make-king-table pass 1/1: Scanning for rare variants... done.
	# 206245 variants handled by initial scan (1329893 remaining).
	# --make-king-table pass 1/1: Writing... done.
	# --make-king-table: 1536138 variants processed.
	# Results written to Boltonia_decurrens_100kb0.8_king.kin0 .
	# End time: Mon Jul 21 22:09:38 2025

plink2 --pfile Boltonia_decurrens --psam Boltonia_decurrens_100kb0.8_meta.psam --pheno-name Flowered_2024 --glm --covar-name PC1 PC2 PC3 PC4 --out Boltonia_decurrens_glm
# PLINK v2.00a5 M1 (16 May 2023)                 www.cog-genomics.org/plink/2.0/
# (C) 2005-2023 Shaun Purcell, Christopher Chang   GNU General Public License v3
# Logging to Boltonia_decurrens_glm.log.
# Options in effect:
#   --covar-name PC1 PC2 PC3 PC4
#   --glm
#   --out Boltonia_decurrens_glm
#   --pfile Boltonia_decurrens
#   --pheno-name Flowered_2024
#   --psam Boltonia_decurrens_100kb0.8_meta.psam

# Start time: Tue Jul 22 23:05:09 2025
# 16384 MiB RAM detected; reserving 8192 MiB for main workspace.
# Using up to 10 threads (change this with --threads).
# 424 samples (0 females, 0 males, 424 ambiguous; 424 founders) loaded from
# Boltonia_decurrens_100kb0.8_meta.psam.
# 5128049 variants loaded from Boltonia_decurrens.pvar.
# 1 binary phenotype loaded (175 cases, 249 controls).
# 4 covariates loaded from Boltonia_decurrens_100kb0.8_meta.psam.
# Calculating allele frequencies... done.
# --glm logistic-Firth hybrid regression on phenotype 'Flowered_2024': done.
# Results written to Boltonia_decurrens_glm.Flowered_2024.glm.logistic.hybrid .
# End time: Tue Jul 22 23:05:32 2025

plink2 --pfile Boltonia_decurrens --psam Boltonia_decurrens_100kb0.8_meta.psam --pheno-name Stem_Length Cul_Flowers --glm --covar-name PC1 PC2 PC3 PC4 --out Boltonia_decurrens_glm
