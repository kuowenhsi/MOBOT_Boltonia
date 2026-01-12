library(tidyverse)
library(vcfR)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_metadata <- readxl::read_xlsx("Boltonia_all_metadata_20251010.xlsx")
intersect_sig_tbl <- read_tsv("./data/BayPass/intersect_sig_tbl.tsv")%>%
  mutate(CHR = as.character(CHR))

envfile <- read_csv("./data/Boltonia_buf_climate_data_20250421.csv")[,c(1,12,36)] %>%
  left_join(Boltonia_metadata, by = "Sample_Name")%>%
  arrange(Pop_Index, Sample_Name)%>%
  group_by(Pop_Index, Pop)%>%
  summarize_at(2:3, .funs = "mean")%>%
  ungroup()%>%
  arrange(Pop_Index)%>%
  mutate(Pop_Name = paste(seq(n()), Pop, sep = " - "))%>%
  mutate(Pop_Name = factor(Pop_Name, levels = Pop_Name))

p <- ggplot(data = envfile, aes(x = wc2.1_30s_bio_7/10, y = Pop_Name))+
  geom_line(group = 1)+
  labs(x = "BIO7 Temperature Annual Range (°C)", y = "")+
  theme_bw()

p


list.files("./data/BayPass", pattern = "*.vcf")

# install.packages(c("vcfR","dplyr","tidyr"), repos="https://cloud.r-project.org")
library(vcfR)
library(dplyr)
library(tidyr)

vcf_path <- "./data/BayPass/Boltonia_decurrens_BayPass_annotated.recode.vcf"

# 1) Load VCF
vcf <- read.vcfR(vcf_path, verbose = TRUE)

# 2) Extract fixed fields (site info) and genotype matrix
fixdf <- as.data.frame(vcf@fix, stringsAsFactors = FALSE) %>%
  transmute(
    CHROM = as.character(CHROM),
    POS   = as.integer(POS),
    ID    = ifelse(ID == ".", NA, ID),
    REF   = REF,
    ALT   = ALT,  # may contain multiple ALT alleles (comma-separated)
    INFO  = INFO
  )

gt <- extract.gt(vcf, element = "GT", as.numeric = FALSE)  # strings like "0/1", "1|0", "./."

# Helper to compute ALT1 counts per genotype vector (assumes diploid)
count_alt1 <- function(x) {
  # Normalize separators and mark missing
  x <- gsub("\\|", "/", x, fixed = FALSE)
  miss <- is.na(x) | x %in% c(".", "./.", ".|.")
  x[miss] <- NA
  
  # For non-missing, extract first and second allele characters
  # (guard for any malformed genotypes)
  a1 <- substr(x, 1, 1)
  a2 <- substr(x, 3, 3)
  # Count how many of the two are '1' (ALT1)
  c1 <- as.integer(a1 == "1")
  c2 <- as.integer(a2 == "1")
  csum <- c1 + c2
  csum[is.na(x)] <- NA_integer_
  return(csum)
}

allele_freq_alt1 <- function(g) {
  # g: character vector of GT for a single site across samples
  alt_counts <- count_alt1(g)
  n_genotypes <- sum(!is.na(alt_counts))           # number of non-missing diploid genotypes
  AN <- 2L * n_genotypes                           # total allele number
  AC <- sum(alt_counts, na.rm = TRUE)              # ALT1 allele count
  AF <- if (AN > 0) AC / AN else NA_real_
  c(AC = AC, AN = AN, AF = AF)
}

midpoint_from_range <- function(x, return = c("numeric", "expr")) {
  return <- match.arg(return)
  # Extract start/end with a simple regex: "<chr>:<start>-<end>"
  parts <- utils::strcapture(
    pattern = "^[^:]+:(\\d+)-(\\d+)$",
    x = x,
    proto = list(start = double(), end = double())
  )
  # parts$start / parts$end will be NA if a string doesn't match the pattern
  mid <- (parts$start + parts$end) / 2
  
  if (return == "numeric") {
    return(mid)
  } else {
    # Build "(start + end)/2" as a character vector
    out <- ifelse(
      is.na(mid),
      NA_character_,
      paste0("(", format(parts$start, scientific = FALSE), " + ",
             format(parts$end, scientific = FALSE), ")/2")
    )
    return(out)
  }
}


# 3) Compute per-site AF for ALT1 (first ALT). For multi-ALT sites, this is the frequency of allele "1".
af_mat <- t(apply(gt, 1, allele_freq_alt1))
af_df  <- cbind(fixdf, as.data.frame(af_mat)) %>%
  cbind(data.frame(Ref_Genotype = gt[,"Boltonia_435"])) %>%
  mutate(
    AC = as.integer(AC),
    AN = as.integer(AN),
    AF_ALT1 = as.numeric(AF)
  ) %>%
  select(CHROM, POS, ID, REF, ALT, AC, AN, AF_ALT1, Ref_Genotype, INFO)

# Peek
print(head(af_df, 10))

# =========================
# OPTIONAL: Per-population AFs
# Provide a popmap with columns: Sample <tab> Population
# =========================
popmap_path <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/BayPass/popmap.tsv"  # uncomment and set if you have it
popmap <- read.delim(popmap_path, header = FALSE, col.names = c("Sample","Population"), stringsAsFactors = FALSE)

# Keep only samples that are in both GT and popmap
common_samples <- intersect(colnames(gt), popmap$Sample)
if (length(common_samples) == 0) stop("No overlapping samples between VCF and popmap.")
gt_sub <- gt[, common_samples, drop = FALSE]
popmap_sub <- popmap[match(common_samples, popmap$Sample), , drop = FALSE]

# Compute AF per population
pop_levels <- unique(popmap_sub$Population)
perpop_list <- lapply(pop_levels, function(pp) {
  samp_idx <- which(popmap_sub$Population == pp)
  vals <- t(apply(gt_sub[, samp_idx, drop = FALSE], 1, allele_freq_alt1))
  out <- as.data.frame(vals)
  names(out) <- paste0(c("AC_","AN_","AF_"), pp)
  out
})
perpop_df <- bind_cols(perpop_list)
bin_bp <- 3e6
af_perpop <- bind_cols(fixdf, perpop_df) %>%
  group_by(CHROM) %>%                     # important: bin within each chromosome
  mutate(
    win_start = ((POS - 1L) %/% bin_bp) * bin_bp + 1L,  # 1-based start
    win_end   = win_start + bin_bp - 1L,
    POS_100K  = win_start / 1000L,                       # optional compact label
    bin_id    = sprintf("%s:%d-%d", CHROM, win_start, win_end)
  )%>%
  select(CHROM, POS, bin_id,ID, REF, ALT, INFO, everything())
print(head(af_perpop, 10))

# =========================
# Notes
# - AF_ALT1 is the frequency of the first ALT allele (“allele 1”).
# - For multi-allelic sites (ALT like "A,C"), AF_ALT1 corresponds to "A" (allele index 1).
# - Missing genotypes (“./.” or “.”) are excluded from AN.
# - Assumes diploid samples; adjust if you have different ploidy.

##

# 0) Decide a stable ordering (here by POS_10K, then POS)
af_perpop <- af_perpop %>% arrange(CHROM, POS, bin_id)

# 1) Build groups
grp  <- af_perpop %>% group_by(CHROM, bin_id) %>%
  left_join(intersect_sig_tbl, by = c("CHROM" = "CHR", "POS"))

# 2) Group keys (unique POS_10K in order)
keys <- grp %>% 
  select("CHROM","POS","bin_id","ID","REF","ALT","INFO","XtXst","eBPis")%>%
  pivot_longer(names_to = "Method", cols = c("XtXst","eBPis"), values_to = "P")%>%
  group_by(bin_id, Method)%>%
  mutate(IF_peak = (P == max(P)), Num_Sites = n())%>%
  filter(IF_peak == TRUE)%>%
  group_by(CHROM, bin_id)%>%
  summarize(POS = mean(POS))%>%
  ungroup%>%
  arrange(CHROM, POS)%>%
  mutate(Intersect_Name = 1:n())%>%
  select(CHROM, POS, everything())

write_tsv(keys, "./data/BayPass/Intersect_keys.tsv")

keys <- read_tsv("./data/BayPass/Intersect_keys.tsv")%>%
  mutate(CHROM = as.character(CHROM))
# Optional: make a character UID per bin and a numeric order index
bin_uid   <- paste0("bin_", keys)         # "bin_353", "bin_1804", ...
bin_order <- seq_along(keys)              # 1, 2, 3, ...

# Handy lookup for later joins/facet ordering
bin_lookup <- tibble(
  POS_10K   = keys,
  bin_uid   = bin_uid,
  bin_order = bin_order
)

chr_len_temp <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_hap1/Boltonia_hap1_1.0.fasta.fai", col_names = c("chr", "end")) %>%
  select(1:2) %>%
  mutate(start = 1) %>%
  mutate(chr = as.integer(str_remove(chr, "Chr_"))) %>%
  arrange(chr) %>%
  mutate(lag_pos = lag(end, default = 0)) %>%
  mutate(pos_pad = cumsum(lag_pos)) %>%
  mutate(padded_start = start + pos_pad - 1, padded_end = end + pos_pad) %>%
  mutate(padded_chr_pos = (padded_start + padded_end) / 2)%>%
  drop_na()

all_intersect <- read_tsv("./data/BayPass/intersect_sig_tbl.tsv") %>%
  left_join(read_tsv("./data/LFMM_PCadapt/LFMM_PCadapt_sig.tsv"), by = c("CHR" = "chr", "POS"  = "POSITION"))%>%
  filter(!is.na(ID))%>%
  left_join(chr_len_temp, by = c("CHR" = "chr"))%>%
  mutate(padded_pos = POS + pos_pad)%>%
  filter(CHR == 1)

write_tsv(all_intersect, "./data/BayPass/all_intersect_20251119.tsv")


grp_all_intersect <- grp %>%
  filter(ID %in% all_intersect$ID)


# 3) Split into a list and add the IDs to each tibble
af_perpop_list <- grp %>%
  ungroup()%>%
  left_join(keys %>% select(CHROM, bin_id, Intersect_Name), by = c("CHROM", "bin_id"))%>%
  mutate(Intersect_Name = factor(Intersect_Name, levels = sort(unique(Intersect_Name))))%>%
  group_by(Intersect_Name)%>%
  group_split(.keep = TRUE) %>%
  map(
  ~ .x %>%
    # Drop AC_/AN_ just in case they’re present
    select(-starts_with("AC_"), -starts_with("AN_"), everything()) %>%
    # Keep only the id columns + AF_Pop_* columns (INFO might not exist in all; any_of handles that)
    select(any_of(c("CHROM","POS","bin_id","ID","REF","ALT","INFO")),
           matches("^AF_Pop_")) %>%
    pivot_longer(
      cols = matches("^AF_Pop_"),
      names_to = "Population",
      names_prefix = "AF_Pop_",
      values_to = "AF"
    ) %>%
    mutate(Pop_Index = paste("Pop", Population,sep = "_"))%>%
    arrange(POS, Pop_Index)%>%
    left_join(envfile, by = "Pop_Index")
)

sapply(af_perpop_list, nrow)/17

af_perpop_list[[2]]

df <- af_perpop_list[[2]] %>%
  mutate(
    variant_id = paste(CHROM, POS, sep = ":"),
    Pop_Index   = factor(Pop_Index, levels = sort(unique(Pop_Index)))
  ) %>%
  filter(ID %in% all_intersect$ID)%>%
  arrange(variant_id, Pop_Name)%>%
  mutate(ID = factor(ID))

p <- ggplot(df, aes(y = Pop_Index, x = AF,group = ID)) +
  geom_line(color = "gray65", show.legend = FALSE, orientation = "y") +
  geom_line(aes(x = (wc2.1_30s_bio_7-38)/3), group = 1, linetype = 2, color = "gray60", linewidth = 0.2)+
  geom_point(fill = "#F1CEFF", size = 1.5, shape = 21, stroke = .1) +
  # geom_text(x = 1.1, y = 1.15, label = 
              # paste("Intersect ID:", keys$Intersect_Name[[1]], "\n", " > Position:",
              #       keys$bin_id[[1]],", n =", keys$Num_Sites[[1]]), check_overlap = TRUE, hjust = 0)+
  scale_x_continuous(name = "Allele freq.", sec.axis = sec_axis(name = "Temperature Annual Range (°C)", transform = function(x){(3*x + 38)}), limits = c(-0.03, 1.2))+
  # scale_y_discrete(labels = 1:17)+
  labs(y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))

p

ggsave("./figures/phenotypes/Intersect_allele_freq_all_intersect_20251119.png", width =3, height = 6, dpi = 600)

############
outdir <- "./figures/phenotypes"
for (i in 1:19) {
  df <- af_perpop_list[[i]] %>%
    mutate(
      variant_id = paste(CHROM, POS, sep = ":"),
      Pop_Name   = factor(Pop_Name, levels = unique(Pop_Name))
    ) %>%
    arrange(variant_id, Pop_Name)
  
  # label text from keys (assumes keys aligns with list order)
  krow <- keys[i, , drop = FALSE]
  lab  <- paste0(
    "Intersect ID: ", krow$Intersect_Name,
    "\n > Position: ", krow$bin_id,
    ", n = ", krow$Num_Sites
  )
  
  p <- ggplot(df, aes(x = Pop_Index, y = AF)) +
    geom_line(aes(group = variant_id, color = variant_id), show.legend = FALSE) +
    geom_line(
      aes(y = (wc2.1_30s_bio_7 - 38) / 3),
      group = 1, linetype = 2, color = "gray60", linewidth = 0.2
    ) +
    geom_point(aes(color = variant_id), show.legend = FALSE, size = 0.3) +
    geom_text(x = 1.1, y = 1.15, label = lab, check_overlap = TRUE, hjust = 0) +
    scale_y_continuous(
      name = "Allele freq.",
      sec.axis = sec_axis(~ (3 * . + 38), name = "Temperature Annual Range (°C)"),
      limits = c(-0.03, 1.2)
    ) +
    # If Pop_Index is something like "Pop_01"... this maps labels to 1..17:
    scale_x_discrete(labels = seq_len(nlevels(df$Pop_Name))) +
    labs(x = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))
  
  outfile <- file.path(outdir, sprintf("Intersect_allele_freq_%02d.png", i))
  ggsave(outfile, plot = p, width = 4, height = 3, dpi = 600)
}
