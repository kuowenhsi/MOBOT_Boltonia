#!/usr/bin/env Rscript

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

# ============================================================
# Batch heritability (animal model) using PLINK2 --make-rel GRM + PCs
# Output: table of h2 and SE for multiple traits
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sommer)
  library(Matrix)
  library(readxl)
})

# -----------------------------
# USER SETTINGS
# -----------------------------
traits <- c(
  "Stem.Length.2025", "Num.Stems.2025", "Stem.Length.2024",
  "leafLong", "leafWide",
  "FlowerDays.2024", "FlowerDays.2025", "FlowerDays.total"
)
pc_names <- c("PC1","PC2","PC3","PC4")

# Files
pheno_file <- "Boltonia_all_metadata_20260205.xlsx"
pca_eigenvec_file <- "./data/Heritability_data/Boltonia_decurrens_imputed_maf_mildLDpruned_pca.eigenvec"
grm_id_file  <- "./data/Heritability_data/Boltonia_decurrens_imputed_maf_mildLDpruned_grm.rel.id"
grm_mat_file <- "./data/Heritability_data/Boltonia_decurrens_imputed_maf_mildLDpruned_grm.rel.txt"

# Model tuning
tolParInv_val <- 1e-4
psd_eig_tol   <- -1e-6
nugget        <- 1e-6

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

extract_vc_h2 <- function(fit, trait_name) {
  vc <- summary(fit)$varcomp
  u_row <- grep("^u:IID\\b", rownames(vc), value = TRUE)
  e_row <- grep("^units\\b", rownames(vc), value = TRUE)
  
  if (length(u_row) != 1 || length(e_row) != 1) {
    return(tibble(
      trait = trait_name, N = NA_integer_,
      Vg = NA_real_, Vg_se = NA_real_,
      Ve = NA_real_, Ve_se = NA_real_,
      h2 = NA_real_, h2_se = NA_real_,
      note = paste0("VC name mismatch: ", paste(rownames(vc), collapse=" | "))
    ))
  }
  
  Vg <- as.numeric(vc[u_row, "VarComp"])
  Ve <- as.numeric(vc[e_row, "VarComp"])
  Vg_se <- as.numeric(vc[u_row, "VarCompSE"])
  Ve_se <- as.numeric(vc[e_row, "VarCompSE"])
  
  h2 <- Vg / (Vg + Ve)
  
  # delta-method SE (approx)
  h2_se <- sqrt((Ve/(Vg+Ve)^2)^2 * Vg_se^2 + (Vg/(Vg+Ve)^2)^2 * Ve_se^2)
  
  tibble(
    trait = trait_name, N = NA_integer_,
    Vg = Vg, Vg_se = Vg_se,
    Ve = Ve, Ve_se = Ve_se,
    h2 = h2, h2_se = h2_se,
    note = ""
  )
}

# -----------------------------
# Check files
# -----------------------------
stop_if_missing(c(pheno_file, pca_eigenvec_file, grm_id_file, grm_mat_file))

# -----------------------------
# Read phenotype
# -----------------------------
pheno <- read_xlsx(pheno_file) %>%
  mutate(Sample_Name = as.character(Sample_Name))

check_has_cols(pheno, c("Sample_Name"), "Phenotype file")
check_has_cols(pheno, traits, "Phenotype file")

# -----------------------------
# Read PCs (your file format: first col IID, then PCs)
# -----------------------------
pcs_raw <- read_table(pca_eigenvec_file, col_names = TRUE, show_col_types = FALSE)
colnames(pcs_raw)[1] <- "IID"
check_has_cols(pcs_raw, c("IID", pc_names), "PCA eigenvec")

pcs <- pcs_raw %>%
  mutate(IID = as.character(IID))

# -----------------------------
# Read GRM IDs (your format may be 1 col IID, or >=2 with IID present)
# -----------------------------
gid <- read_table(grm_id_file, col_names = TRUE, show_col_types = FALSE)

if (!("IID" %in% names(gid))) {
  # If no header or different header, fall back by position
  gid2 <- read_table(grm_id_file, col_names = TRUE, show_col_types = FALSE)
  if (ncol(gid2) == 1) {
    gid <- tibble(IID = as.character(gid2[[1]]))
  } else {
    gid <- tibble(IID = as.character(gid2[[2]]))
  }
} else {
  gid <- gid %>% mutate(IID = as.character(IID))
}

if (anyDuplicated(gid$IID) > 0) stop("Duplicate IIDs in GRM .rel.id", call. = FALSE)

# -----------------------------
# Read GRM matrix once and condition once
# -----------------------------
G <- as.matrix(read.table(grm_mat_file, header = FALSE))
n <- nrow(gid)
if (nrow(G) != n || ncol(G) != n) {
  stop("GRM matrix dimension mismatch: ", nrow(G), "x", ncol(G), " vs IDs=", n, call. = FALSE)
}
rownames(G) <- colnames(G) <- gid$IID

# Symmetrize
G <- (G + t(G)) / 2

# Rescale mean diagonal to 1
md <- mean(diag(G), na.rm = TRUE)
if (!is.finite(md) || md <= 0) stop("Mean diagonal of GRM is not positive/finite.", call. = FALSE)
G <- G / md

# PSD-fix only if needed
ev <- eigen(G, symmetric = TRUE, only.values = TRUE)$values
message("GRM eigen min = ", signif(min(ev), 5), " ; median = ", signif(median(ev), 5))
if (min(ev) < psd_eig_tol) {
  message("Applying nearPD() to make GRM PSD...")
  G <- as.matrix(nearPD(G, corr = FALSE, keepDiag = TRUE)$mat)
}

# Nugget for stability
diag(G) <- diag(G) + nugget

# -----------------------------
# Run models trait-by-trait
# -----------------------------
results <- vector("list", length(traits))
names(results) <- traits

for (tr in traits) {
  message("\n--- Fitting trait: ", tr, " ---")
  
  dat <- pheno %>%
    dplyr::select(Sample_Name, all_of(tr)) %>%
    left_join(pcs, by = c("Sample_Name" = "IID")) %>%
    rename(IID = Sample_Name) %>%
    mutate(IID = as.character(IID)) %>%
    filter(!is.na(.data[[tr]])) %>%
    filter(if_all(all_of(pc_names), ~ !is.na(.x))) %>%
    filter(IID %in% rownames(G)) %>%
    arrange(match(IID, rownames(G)))
  
  if (nrow(dat) < 20) {
    results[[tr]] <- tibble(
      trait = tr, N = nrow(dat),
      Vg = NA_real_, Vg_se = NA_real_,
      Ve = NA_real_, Ve_se = NA_real_,
      h2 = NA_real_, h2_se = NA_real_,
      note = "N < 20 after filtering"
    )
    next
  }
  
  G2 <- G[dat$IID, dat$IID]
  stopifnot(all(dat$IID == rownames(G2)))
  
  fixed_formula <- as.formula(paste(tr, "~", paste(pc_names, collapse = " + ")))
  
  fit <- tryCatch(
    mmer(
      fixed  = fixed_formula,
      random = ~ vs(IID, Gu = G2),
      rcov   = ~ units,
      data   = dat,
      tolParInv = tolParInv_val
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    results[[tr]] <- tibble(
      trait = tr, N = nrow(dat),
      Vg = NA_real_, Vg_se = NA_real_,
      Ve = NA_real_, Ve_se = NA_real_,
      h2 = NA_real_, h2_se = NA_real_,
      note = paste0("Model error: ", fit$message)
    )
    next
  }
  
  out <- extract_vc_h2(fit, tr) %>% mutate(N = nrow(dat))
  results[[tr]] <- out
}

res_tbl <- bind_rows(results) %>%
  mutate(
    h2 = round(h2, 4),
    h2_se = round(h2_se, 4),
    Vg = round(Vg, 4),
    Ve = round(Ve, 4)
  )

print(res_tbl)

# Optional: write to CSV
write_csv(res_tbl, "./data/Heritability_data/heritability_all_traits_h2_SE.csv")
message("\nSaved: ./data/Heritability_data/heritability_all_traits_h2_SE.csv\n")
