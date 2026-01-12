library(tidyverse)
library(data.table)
library(VennDiagram)
library(ggrepel)

source("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/BayPass/baypass_utils.R")


setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_metadata <- readxl::read_xlsx("Boltonia_all_metadata_20251010.xlsx")



envfile <- read_csv("./data/Boltonia_buf_climate_data_20250421.csv")[,c(1,12,36)] %>%
  left_join(Boltonia_metadata[,c(1,3)], by = "Sample_Name")%>%
  arrange(Pop_Index, Sample_Name)%>%
  group_by(Pop_Index)%>%
  summarize_at(2:3, .funs = "mean")%>%
  pivot_longer(-Pop_Index, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = Pop_Index, values_from = value) %>%
  relocate(variable)
  

head(envfile)

colnames(envfile)

envfile_t <- envfile[,-1]

write_tsv(envfile_t, "./data/BayPass/envfile.tsv", col_names = FALSE)


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


ggsave("./figures/phenotypes/Sites_bio7.png", width =6, height = 7, dpi = 600)


# [Mon Oct  6 16:00:53 UTC 2025] start file conversion
# [INFO] VCF path: /storage1/fs1/christine.e.edwards/Active/Wen/IMLS_analyses/Boltonia_decurrens_imputed_MAF_Low_LD/Boltonia_decurrens_imputed_maf.vcf
# [INFO] Total variant records in VCF: 6950268
# [INFO] Popmap path: /storage1/fs1/christine.e.edwards/Active/Wen/IMLS_analyses/popmap.tsv
# [INFO] Number of populations: 17
# [INFO] Populations (order): Pop_01, Pop_09, Pop_11, Pop_10, Pop_13, Pop_03, Pop_04, Pop_16, Pop_17, Pop_08, Pop_15, Pop_12, Pop_07, Pop_14, Pop_06, Pop_05, Pop_02
# [INFO] Wrote 6950268 records to Boltonia_decurrens_imputed_maf_XtX.baypass
# [INFO] Wrote marker index to Boltonia_decurrens_imputed_maf_XtX.chr_pos_mrk.tsv
# [INFO] Done.
# [Mon Oct  6 16:26:30 UTC 2025] end file conversion
# [Mon Oct  6 16:26:30 UTC 2025] BayPass core model with provided Omega

# Load chromosome information from a .fasta index file
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

omega.mat <- read.table("./data/BayPass/Boltonia_decurrens_imputed_MAF_High_LD_XtX_core_mat_omega.out", header = FALSE) %>% as.matrix()
class(omega.mat)
sim_data <- simulate.baypass(omega.mat,
                 nsnp=1000,
                 beta.coef=NA,
                 beta.pi=c(1,1),
                 pop.trait=0,
                 sample.size=100,
                 pi.maf=0.05,
                 suffix="sim",
                 print.sim.params.values=FALSE,
                 output.bayenv.format=FALSE,
                 remove.fixed.loci=FALSE,
                 coverage=NA)

om.bta.svd=plot.omega(omega=omega.mat)


XtX_BP <- fread("./data/BayPass/Boltonia_decurrens_imputed_maf_XtX_XtX_summary_pi_xtx.out.gz", header = TRUE)

Bay_BP <- fread("./data/BayPass/Boltonia_decurrens_imputed_maf_ENV_ENV_summary_betai_reg.out.gz", header = TRUE)

chr_pos_mrk <- fread("./data/BayPass/Boltonia_decurrens_imputed_maf_XtX.chr_pos_mrk.tsv.gz", header = TRUE)
head(chr_pos_mrk)

keys <- read_tsv("./data/BayPass/Intersect_keys.tsv")%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(padded_pos = POS + pos_pad)

GWAS_sig <- read_tsv("./data/GWAS/GLM_sig_markers_all.tsv")

LFMM_PCadapt_sig <- read_tsv("./data/LFMM_PCadapt/LFMM_PCadapt_sig.tsv")

XtX_BP_mrk <- left_join(XtX_BP, chr_pos_mrk, by = "MRK")%>%
  left_join(chr_len_temp, by = c("CHR" = "chr"))%>%
  left_join(GWAS_sig, by = c("CHR" = "#CHROM", "POS"))%>%
  mutate(padded_pos = POS + pos_pad)%>%
  mutate(color = "gray90")%>%
  mutate(color = ifelse(.$XtXst > 46.71387, "red", color))
  
XtX_BP_mrk_sig <- XtX_BP_mrk %>%
  filter(color == "red")

XtX_BP_mrk_GWAS <- XtX_BP_mrk %>%
  filter(!is.na(GWAS_TRAIT))


head(XtX_BP_mrk)
quantile(XtX_BP_mrk$`log10(1/pval)`, 0.999)
quantile(XtX_BP_mrk$XtXst, 0.999)

head(Bay_BP)

Bay_BP_mrk <- left_join(Bay_BP, chr_pos_mrk, by = "MRK")%>%
  filter(COVARIABLE == 2, eBPis < 10)%>%
  left_join(chr_len_temp, by = c("CHR" = "chr"))%>%
  left_join(GWAS_sig, by = c("CHR" = "#CHROM", "POS"))%>%
  mutate(padded_pos = POS + pos_pad)%>%
  mutate(color = "gray90")%>%
  mutate(color = ifelse(.$eBPis > 2.759296, "red", color))
  
head(Bay_BP_mrk)

Bay_BP_mrk_sig <- Bay_BP_mrk %>%
  filter(color == "red")

Bay_BP_mrk_GWAS <- Bay_BP_mrk %>%
  filter(!is.na(GWAS_TRAIT))

intersect_sig <- intersect(XtX_BP_mrk_sig$MRK, Bay_BP_mrk_sig$MRK)


# VennDiagram::venn.diagram(list(A=XtX_BP_mrk_sig$MRK, B=Bay_BP_mrk_sig$MRK), filename = "./figures/BayPass/Venn.png", height = 2, width = , units = "in",resolution = 600)

intersect_sig_tbl <- tibble(MRK = intersect_sig) %>%
  left_join(chr_pos_mrk, by = "MRK")%>%
  # left_join(XtX_BP_mrk_sig %>% select(MRK, XtXst), by = "MRK")%>%
  # left_join(Bay_BP_mrk_sig%>% select(MRK, eBPis), by = "MRK")%>%
  select(-MRK)

write_tsv(intersect_sig_tbl, "./data/BayPass/intersect_sig_tbl.tsv", col_names = TRUE)


all_intersect <- intersect_sig_tbl %>%
  left_join(LFMM_PCadapt_sig, by = c("CHR" = "chr", "POS"  = "POSITION"))%>%
  filter(!is.na(ID))%>%
  left_join(chr_len_temp, by = c("CHR" = "chr"))%>%
  mutate(padded_pos = POS + pos_pad)

head(Bay_BP_mrk)
hist(Bay_BP_mrk$eBPis)
hist(10^(-Bay_BP_mrk$eBPis), breaks = 100)
sum(Bay_BP_mrk$Beta_is == 0)
sum(Bay_BP_mrk$eBPis > 3)
sum(Bay_BP_mrk$eBPis > 4)
sum(Bay_BP_mrk$eBPis > 4.5)
sum(Bay_BP_mrk$eBPis > -log10(5e-8))
sum(Bay_BP_mrk$color == "green")
sum(Bay_BP_mrk$`BF(dB)` > 14.34931)
sum(Bay_BP_mrk$color == "red")

quantile(Bay_BP_mrk$`BF(dB)`, 0.9999)
quantile(Bay_BP_mrk$eBPis, 0.999)



head(XtX_BP_mrk)
hist(10^(-XtX_BP_mrk$`log10(1/pval)`))
sum(XtX_BP_mrk$`log10(1/pval)` > 3)
sum(XtX_BP_mrk$`log10(1/pval)` > 4)
sum(XtX_BP_mrk$`log10(1/pval)` > 6)
sum(XtX_BP_mrk$`log10(1/pval)` > -log10(5e-8))
quantile(XtX_BP_mrk$XtXst, 0.9999)

p1 <- ggplot(data = XtX_BP_mrk, aes(x = padded_pos, y = XtXst))+
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(data = filter(XtX_BP_mrk, color == "gray90"), color = "gray90", size = .1, show.legend = FALSE)+
  geom_point(data = filter(XtX_BP_mrk, color == "red"), color = "#66DDAA", size = .1, show.legend = FALSE)+
  geom_point(data = XtX_BP_mrk_GWAS, color = "#00AAFF", size = .4)+
  geom_point(data = filter(XtX_BP_mrk_GWAS, color == "red"), fill = "#ffe136", size = 2, shape = 21, stroke = .1)+
  geom_segment(data = filter(XtX_BP_mrk_GWAS, color == "red"), aes(x = padded_pos, y = XtXst +10, yend = XtXst +5), arrow.fill = "#ffe136", arrow = arrow(type = "closed", length = unit(0.05, "inches")), linewidth = 0.2)+
  # geom_segment(data = keys, aes(yend = XtXst + c(15,35,25,25,25,15,15,45,35,15,25,40,15,15,15,35,15,15,15)), linewidth = 0.3)+
  # geom_label(data = keys, aes(label = Intersect_Name),size = 2, position = position_nudge(y = c(15,35,25,25,25,15,15,45,35,15,25,40,15,15,15,35,15,15,15)),fill = "white", label.r = unit(0.04, "in"))+
  geom_point(data = filter(XtX_BP_mrk, MRK %in% intersect_sig), color = "#FF6580", size = .2, show.legend = FALSE)+
  geom_point(data = all_intersect, fill = "#F1CEFF", size = 1.5, shape = 21, stroke = .1)+
  annotate(geom = "text", label = "BayPass - Genetic differentiation", x = Inf, y = Inf, hjust = 1.05, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous("XtXst", expand = c(0, 0.1, 0.1, 15)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
  guides(fill = "none")


ggsave("./figures/BayPass/Boltonia_XtXst_plot_single.png", width = 7.5, height = 1.75)


# Q-Q plot for PCadapt result
p_QQ_PCadapt <- ggplot(data = XtX_BP_mrk, aes(sample = `log10(1/pval)`/log10(exp(1)))) +
  stat_qq(aes(x = after_stat(theoretical) / log(10), y = after_stat(sample) / log(10)), distribution = qexp, size = 0.2) +
  stat_qq_line(aes(x = after_stat(x) / log(10), y = after_stat(y) / log(10)), distribution = qexp) +
  geom_hline(yintercept = -log10(5e-8), color = "red") +
  xlab(expression("Theoretical" ~ "-" * log[10] * "(p)")) +
  ylab(expression("Observed" ~ "-" * log[10] * "(p)")) +
  theme_bw() +
  theme(panel.grid = element_blank())

# ggsave("./figures/BayPass/Boltonia_XtXst_QQplot_single.png", width = 2.5, height = 2.5)


p <- ggplot(data = XtX_BP_mrk, aes(x = padded_pos, y = M_XtX))+
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(aes(color = (`log10(1/pval)` > log10(1/0.000001))), size = .1, show.legend = FALSE)+
  scale_color_manual(values = c("gray90", "red"))+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_x_continuous("", expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous("M_XtX", expand = c(0, 0, 0.1, 0.1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
  guides(fill = "none")

max(XtX_BP_mrk$`log10(1/pval)`)

# ggsave("./figures/BayPass/Boltonia_M_XtX_plot_single.png", width = 10, height = 3.5)

#############
# png("./figures/BayPass/Boltonia_BF_test.png", width = 7.5, height = 7.5, units = "in", res = 300)
# plot(Bay_BP_mrk$`BF(dB)`, Bay_BP_mrk$eBPis)
# dev.off()

p2 <- ggplot(data = Bay_BP_mrk, aes(x = padded_pos, y = eBPis))+
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(data = filter(Bay_BP_mrk, color == "gray90"), color = "gray90", size = .1, show.legend = FALSE)+
  geom_point(data = filter(Bay_BP_mrk, color == "red"), color = "#66DDAA", size = .1, show.legend = FALSE)+
  geom_point(data = Bay_BP_mrk_GWAS, color = "#00AAFF", size = .4)+
  # geom_segment(data = keys, aes(yend = eBPis + 0.1*c(15,5,25,25,25,15,15,45,35,15,25,40,15,15,35,35,15,15,15)), linewidth = 0.3)+
  # geom_label(data = keys, aes(label = Intersect_Name),size = 2, position = position_nudge(y = 0.1*c(15,5,25,25,25,15,15,45,35,15,25,40,15,15,35,35,15,15,15)),fill = "white", label.r = unit(0.04, "in"))+
  geom_point(data = filter(Bay_BP_mrk, MRK %in% intersect_sig), color = "#FF6580", size = .2, show.legend = FALSE)+
  geom_point(data = all_intersect, fill = "#F1CEFF", size = 1.5, shape = 21, stroke = .1)+
  annotate(geom = "text", label = "BayPass - Environmental association", x = Inf, y = Inf, hjust = 1.05, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous("eBPis", expand = c(0, 0.1, 0.1, 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
  guides(fill = "none")


ggsave("./figures/BayPass/Boltonia_BF_plot_single.png", width = 7.5, height = 1.75)

p_comb <- cowplot::plot_grid(p1, p2, align = "v", ncol = 1, labels = c("(C)", "(D)"), label_size = 12, label_fontface = "plain")

ggsave("./figures/BayPass/Boltonia_BayPass_comb.png", width = 7.5, height = 3.5, dpi = 600)


# Q-Q plot for PCadapt result
p_QQ_PCadapt <- ggplot(data = Bay_BP_mrk, aes(sample = eBPis/log10(exp(1)))) +
  stat_qq(aes(x = after_stat(theoretical) / log(10), y = after_stat(sample) / log(10)), distribution = qexp, size = 0.2) +
  stat_qq_line(aes(x = after_stat(x) / log(10), y = after_stat(y) / log(10)), distribution = qexp) +
  geom_hline(yintercept = -log10(5e-8), color = "red") +
  xlab(expression("Theoretical" ~ "-" * log[10] * "(p)")) +
  ylab(expression("Observed" ~ "-" * log[10] * "(p)")) +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("./figures/BayPass/Boltonia_BF_QQplot_single.png", width = 2.5, height = 2.5)

#############

p <- ggplot(data = XtX_BP_mrk, aes(x = POS, y = XtXst))+
  geom_point(aes(color = (`log10(1/pval)` > log10(1/0.001))), size = .1, show.legend = FALSE)+
  geom_point(data = XtX_BP_mrk_functional, color = "red", size = .1)+
  scale_color_manual(values = c("gray90", "black"))+
  facet_wrap(.~CHR, ncol = 1, scales = "free_x")+
  theme_bw()

ggsave("Boltonia_XtX_plot_functional.png", width = 10, height = 27)

hist(XtX_BP$`log10(1/pval)`[1:10000])

head(XtX_BP)
plot(XtX_BP$XtXst[1:10000])


############pair wise Fst scan

list.files("./data/Fst_by_pop/", pattern = "*.fst.var.zst")

pair_Fst <- fread(cmd = "zstd -dc ./data/Fst_by_pop/Boltonia_decurrens_imputed_maf_mildLDpruned_fst.Pop_01.Pop_02.fst.var.zst", header = TRUE)
head(pair_Fst)

pair_Fst <- pair_Fst %>%
  filter(HUDSON_FST != "NaN")%>%
  mutate(HUDSON_FST = as.numeric(HUDSON_FST))

head(pair_Fst)
head(chr_pos_mrk)
head(chr_len_temp)

pair_Fst_pos <- pair_Fst %>%
  left_join(chr_len_temp, by = c("#CHROM" = "chr"))%>%
  mutate(padded_pos = POS + pos_pad)

hist(pair_Fst_pos$HUDSON_FST)
Fst_threshold <- quantile(pair_Fst_pos$HUDSON_FST, 0.999)

p <- ggplot(data = pair_Fst_pos, aes(x = padded_pos, y = HUDSON_FST))+
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(aes(color = (HUDSON_FST > Fst_threshold)), size = .1, show.legend = FALSE)+
  geom_text(x = 6e6, y = 0.1, label = "Pop_01 - Pop_02", size = 6, hjust = 0)+
  scale_color_manual(values = c("gray90", "red"))+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_x_continuous("", expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous("Fst", expand = c(0, 0, 0.1, 0.1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
  guides(fill = "none")


ggsave("./figures/BayPass/Boltonia_Fst_Pop_01_Pop_02.png", width = 7.5, height = 2.5)

####################
# Packages
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(grid)      # for unit()

# Inputs (adjust if yours differ)
in_dir  <- "./data/Fst_by_pop"
out_dir <- "./figures/BayPass"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Expect these to exist in your environment:
# chr_len_temp with columns: chr, padded_start, padded_end, padded_chr_pos, pos_pad
# (as used in your example code)

# List all pairwise files
zst_files <- list.files(in_dir, pattern = "\\.fst\\.var\\.zst$", full.names = TRUE)

message(sprintf("Found %d files.", length(zst_files)))

for (f in zst_files) {
  # Parse Pop IDs from filename, e.g., "..._fst.Pop_01.Pop_02.fst.var.zst"
  base <- basename(f)
  pops <- str_match(base, "Pop_(\\d+)\\.Pop_(\\d+)")
  if (is.na(pops[1,1])) {
    warning("Could not parse Pop IDs from: ", base, " — skipping.")
    next
  }
  p1 <- pops[1,2]; p2 <- pops[1,3]
  pair_label <- sprintf("Pop_%s - Pop_%s", p1, p2)
  
  # Read pairwise Fst (zstd decompression via fread cmd)
  dt <- tryCatch(
    suppressWarnings(fread(cmd = sprintf("zstd -dc %s", shQuote(f)), header = TRUE)),
    error = function(e) { warning("Failed to read: ", base, " (", e$message, ")"); NULL }
  )
  if (is.null(dt) || !"HUDSON_FST" %in% names(dt) || !"POS" %in% names(dt) || !"#CHROM" %in% names(dt)) {
    warning("Required columns missing in: ", base, " — skipping.")
    next
  }
  
  # Clean FST column
  dt <- dt %>%
    mutate(HUDSON_FST = suppressWarnings(as.numeric(HUDSON_FST))) %>%
    filter(!is.na(HUDSON_FST))
  
  # Join chromosome padding info and compute genome-wide padded position
  dt <- dt %>%
    left_join(chr_len_temp, by = c("#CHROM" = "chr")) %>%
    mutate(padded_pos = POS + pos_pad)
  
  if (nrow(dt) == 0) { warning("No rows after cleaning for: ", base); next }
  
  # Per-pair threshold (0.999 quantile)
  Fst_threshold <- quantile(dt$HUDSON_FST, 0.999, na.rm = TRUE)
  
  # Plot
  p <- ggplot(dt, aes(x = padded_pos, y = HUDSON_FST)) +
    geom_rect(data = chr_len_temp,
              aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf,
                  fill = factor(chr, levels = unique(chr_len_temp$chr))),
              inherit.aes = FALSE) +
    geom_point(aes(color = HUDSON_FST > Fst_threshold), size = 0.1, show.legend = FALSE) +
    geom_text(x = min(chr_len_temp$padded_start, na.rm = TRUE) + 6e6, y = 0.1,
              label = pair_label, size = 6, hjust = 0) +
    scale_color_manual(values = c("gray90", "red")) +
    scale_fill_manual(name = "", values = rep(c("white", "gray95"), length.out = nrow(chr_len_temp))) +
    scale_x_continuous("", expand = c(0, 0),
                       breaks = chr_len_temp$padded_chr_pos,
                       labels = chr_len_temp$chr) +
    scale_y_continuous("Fst", expand = c(0, 0, 0.1, 0.1)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
    guides(fill = "none")
  
  # Save
  out_file <- file.path(out_dir, sprintf("Boltonia_Fst_Pop_%s_Pop_%s.png", p1, p2))
  ggsave(out_file, plot = p, width = 7.5, height = 2.5, dpi = 600)
  message("Saved: ", out_file)
}

message("Done.")

