#!/usr/bin/env Rscript

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

# ============================================================
# Heritability (animal model) using PLINK2 --make-rel GRM + PCs
# - Phenotype: Boltonia_metadata.csv
# - PCs: Boltonia_decurrens_imputed_maf_mildLDpruned_pca.eigenvec
# - GRM IDs: Boltonia_grm.rel.id   (NO FID column assumed)
# - GRM matrix: Boltonia_grm.rel   (already decompressed; plain text)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sommer)
  library(Matrix)  # for nearPD if needed
})

# -----------------------------
# USER EDITS
# -----------------------------
trait_col <- "Stem.Length.2025"    # <-- CHANGE to the phenotype column name in Boltonia_metadata.csv
pc_names  <- c("PC1","PC2","PC3","PC4")

# -----------------------------
# FILE PATHS (edit if needed)
# -----------------------------
pheno_file <- "Boltonia_all_metadata_20251010.xlsx"

pca_eigenvec_file <- "./data/Heritability_data/Boltonia_decurrens_imputed_maf_mildLDpruned_pca.eigenvec"

grm_id_file  <- "./data/Heritability_data/Boltonia_decurrens_imputed_maf_mildLDpruned_grm.rel.id"   # produced by plink2 --make-rel
grm_mat_file <- "./data/Heritability_data/Boltonia_decurrens_imputed_maf_mildLDpruned_grm.rel.txt"      # decompressed text matrix (square)

# -----------------------------
# Helpers
# -----------------------------
stop_if_missing <- function(files) {
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    stop("Missing file(s):\n", paste0("  - ", missing, collapse = "\n"), call. = FALSE)
  }
}

check_has_cols <- function(df, cols, df_name="data.frame") {
  miss <- setdiff(cols, colnames(df))
  if (length(miss) > 0) {
    stop(df_name, " is missing required columns: ", paste(miss, collapse=", "), call. = FALSE)
  }
}

# -----------------------------
# Check files exist
# -----------------------------
stop_if_missing(c(pheno_file, pca_eigenvec_file, grm_id_file, grm_mat_file))

# -----------------------------
# 1) Read phenotype and require Sample_Name + trait
# -----------------------------
pheno <- readxl::read_xlsx(pheno_file) %>%
  mutate(Sample_Name = as.character(Sample_Name))
check_has_cols(pheno, c("Sample_Name", trait_col), "Boltonia_metadata.csv")

# -----------------------------
# 2) Read PCA eigenvec and left_join by Sample_Name == IID
#    plink2 eigenvec: FID IID PC1 PC2 ...
# -----------------------------
pcs_raw <- read_table(pca_eigenvec_file, col_names = TRUE, show_col_types = FALSE)
if (ncol(pcs_raw) < 1 + length(pc_names)) {
  stop("PCA file has ", ncol(pcs_raw), " columns; expected at least ", 1 + length(pc_names),
       " (IID PC1..PC4).", call. = FALSE)
}
colnames(pcs_raw)[1] <- c("IID")

pcs <- pcs_raw %>%
  mutate(IID = as.character(IID)) %>%
  dplyr::select(IID, all_of(pc_names))

dat <- pheno %>%
  left_join(pcs, by = c("Sample_Name" = "IID"))

# Drop rows missing trait or PCs (you can change this if you prefer imputation)
dat <- dat %>%
  filter(!is.na(.data[[trait_col]])) %>%
  filter(if_all(all_of(pc_names), ~ !is.na(.x)))

if (nrow(dat) < 20) {
  stop("Too few rows after filtering for trait + PC1-4. N=", nrow(dat), call. = FALSE)
}

# -----------------------------
# 3) Read GRM ID file (NO FID column assumed)
#    Accepts either:
#      - 1 column: IID
#      - 2 columns: FID IID (we'll still use IID)
# -----------------------------
gid <- read_table(grm_id_file, col_names = TRUE, show_col_types = FALSE)

if (ncol(gid) == 1) {
  colnames(gid) <- "IID"
} else if (ncol(gid) >= 2) {
  colnames(gid)[1:2] <- c("FID","IID")
} else {
  stop("GRM .rel.id file has 0 columns?!", call. = FALSE)
}

gid <- gid %>%
  mutate(IID = as.character(IID))

# Ensure IID uniqueness
if (anyDuplicated(gid$IID) > 0) {
  stop("Duplicate IIDs detected in GRM .rel.id. This should not happen.", call. = FALSE)
}

# -----------------------------
# 4) Read GRM matrix (square, plain text)
# -----------------------------
G <- as.matrix(read.table(grm_mat_file, header = FALSE))

n <- nrow(gid)
if (nrow(G) != n || ncol(G) != n) {
  stop("GRM matrix dimension mismatch: matrix is ", nrow(G), "x", ncol(G),
       " but .rel.id has ", n, " rows.", call. = FALSE)
}

rownames(G) <- colnames(G) <- gid$IID

hist(G)
# -----------------------------
# 5) Align phenotype+PCs to GRM IDs
# -----------------------------
dat2 <- dat %>%
  mutate(IID = as.character(Sample_Name)) %>%
  filter(IID %in% rownames(G)) %>%
  arrange(match(IID, rownames(G)))

if (nrow(dat2) < 20) {
  stop("Too few samples overlap between phenotype and GRM IDs. N=", nrow(dat2), call. = FALSE)
}

G2 <- G[dat2$IID, dat2$IID]
stopifnot(all(dat2$IID == rownames(G2)))
dim(G2)
# -----------------------------
# 6) Basic GRM conditioning
#    - symmetrize (safety)
#    - rescale mean diagonal to 1
#    - PSD-fix only if needed
# -----------------------------
G2 <- (G2 + t(G2)) / 2

# Rescale to mean(diag)=1 (helps interpretability/conditioning)
md <- mean(diag(G2), na.rm = TRUE)
if (!is.finite(md) || md <= 0) stop("Mean diagonal of GRM is not positive/finite.", call. = FALSE)
G2 <- G2 / md

hist(G2)

# Check eigenvalues; PSD-correct only if needed
ev <- eigen(G2, symmetric = TRUE, only.values = TRUE)$values
message("GRM eigen min = ", signif(min(ev), 5), " ; median = ", signif(median(ev), 5))

if (min(ev) < -1e-6) {
  message("Applying nearPD() to make GRM PSD (min eigen < -1e-6)...")
  G2 <- as.matrix(nearPD(G2, corr = FALSE, keepDiag = TRUE)$mat)
}

# Add a tiny nugget for numerical stability
diag(G2) <- diag(G2) + 1e-6

# Quick sanity: off-diagonal distribution should be near 0
message("GRM off-diagonal quantiles:")
print(quantile(G2[upper.tri(G2)], c(0, .001, .01, .5, .99, .999, 1), na.rm = TRUE))

# -----------------------------
# 7) Fit animal model in sommer and compute h2
# -----------------------------
fixed_formula <- as.formula(paste(trait_col, "~", paste(pc_names, collapse = " + ")))

fit <- mmer(
  fixed  = fixed_formula,
  random = ~ vs(IID, Gu = G2),
  rcov   = ~ units,
  data   = dat2,
  tolParInv = 1e-4
)

vc <- summary(fit)$varcomp

# Find the additive genetic (u:IID...) row and residual (units...) row
u_row <- grep("^u:IID\\b", rownames(vc), value = TRUE)
e_row <- grep("^units\\b", rownames(vc), value = TRUE)

if (length(u_row) != 1 || length(e_row) != 1) {
  stop(
    "Could not uniquely identify variance-component rows.\n",
    "u_row matches: ", paste(u_row, collapse = ", "), "\n",
    "e_row matches: ", paste(e_row, collapse = ", "), "\n",
    "All rows: ", paste(rownames(vc), collapse = ", "),
    call. = FALSE
  )
}

Vg <- vc[u_row, "VarComp"]
Ve <- vc[e_row, "VarComp"]
h2 <- as.numeric(Vg / (Vg + Ve))

# Optional: delta-method SE for h2 (rough approximation)
Vg_se <- vc[u_row, "VarCompSE"]
Ve_se <- vc[e_row, "VarCompSE"]
h2_se <- as.numeric(sqrt((Ve/(Vg+Ve)^2)^2 * Vg_se^2 + (Vg/(Vg+Ve)^2)^2 * Ve_se^2))

cat("\n=============================\n")
cat("Vg (additive):", signif(Vg, 6), "SE:", signif(Vg_se, 6), "\n")
cat("Ve (residual):", signif(Ve, 6), "SE:", signif(Ve_se, 6), "\n")
cat("h2:", signif(h2, 6), "approx SE:", signif(h2_se, 6), "\n")
cat("=============================\n\n")

