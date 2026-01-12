library(tidyverse)
library(data.table)
library(qvalue)
library(cowplot)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

GWAS_sig <- read_tsv("./data/GWAS/GLM_sig_markers_all.tsv")

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

LFMM_data <- fread(file = "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/PCadapt_output/Boltonia_LFMM_output_K_5_wc2.1_30s_bio_7_20251003.txt.gz", col.names = c("chr", "POSITION", "ID", "pvalue", "climatic_varible", "K") )%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad, threshold = quantile(pvalue, 1-0.999)) %>%
  mutate(dot_color = case_when(pvalue < threshold ~ "#66DDAA", TRUE ~ "gray80")) 

quantile(LFMM_data$pvalue, 1-0.999)

LFMM_sig <- LFMM_data %>%
  filter(dot_color == "#66DDAA")

chr_pos_mrk <- fread("./data/BayPass/Boltonia_decurrens_imputed_maf_XtX.chr_pos_mrk.tsv.gz", header = TRUE)
BayPass_data <- fread("./data/BayPass/Boltonia_decurrens_imputed_maf_XtX_XtX_summary_pi_xtx.out.gz", header = TRUE)%>%
  left_join(chr_pos_mrk, by = "MRK")%>%
  left_join(chr_len_temp, by = c("CHR" = "chr"))%>%
  left_join(GWAS_sig, by = c("CHR" = "#CHROM", "POS"))%>%
  mutate(padded_pos = POS + pos_pad)%>%
  mutate(color = "gray90")%>%
  mutate(color = ifelse(.$XtXst > 46.71387, "red", color))

BayPass_sig <- BayPass_data%>%
  filter(color == "red", GWAS_P < 5e-8)
  
GWAS_sig_window <- GWAS_sig %>%
  dplyr::rename(CHROM = `#CHROM`, POSITION = POS) %>%
  mutate(CHROM = as.integer(CHROM), POSITION = as.integer(POSITION))%>%
  mutate(START = POSITION - 5000L, END = POSITION + 5000L)

# write_tsv(GWAS_sig_window, "./data/GWAS/GLM_sig_markers_all_window.tsv")

##########

LFMM_GWAS <- LFMM_data %>%
  inner_join(GWAS_sig, by = c("chr" = "#CHROM", "POSITION"  = "POS"))%>%
  mutate(IF_sig = (pvalue < quantile(LFMM_data$pvalue, 1-0.999))) %>%
  filter(IF_sig == TRUE)



# Download LFMM_output_20230606.txt from Dryad https://doi.org/10.5061/dryad.s7h44j1fd

glm_path <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/GWAS/"
glm_input <- list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/GWAS")
length(glm_input)

bin_input <- list.files("./data/GWAS")

FlowerDays_2024_bin <- read_tsv("./data/GWAS/FlowerDays_2024_bin.tsv")%>%
  rename(chr = `#CHROM`, POSITION = POS)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)

FlowerDays_2025_bin <- read_tsv("./data/GWAS/FlowerDays_2025_bin.tsv")%>%
  group_by(bin_Name)%>%
  filter(GWAS_P == min(GWAS_P))%>%
  summarise_all(.funs = "first")%>%
  ungroup()%>%
  rename(chr = `#CHROM`, POSITION = POS)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)

Stem_Length_20242025_bin <- read_tsv("./data/GWAS/Stem_Length_20242025_bin.tsv")%>%
  group_by(bin_Name)%>%
  filter(GWAS_P == min(GWAS_P))%>%
  summarise_all(.funs = "first")%>%
  ungroup()%>%
  rename(chr = `#CHROM`, POSITION = POS)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)%>%
  filter(bin_Name != 18)%>%
  mutate(bin_Name = 1:18)

Num_Stems_2025_bin <- read_tsv("./data/GWAS/Num_Stems_2025_bin.tsv")%>%
  group_by(bin_Name)%>%
  filter(GWAS_P == min(GWAS_P))%>%
  summarise_all(.funs = "first")%>%
  ungroup()%>%
  rename(chr = `#CHROM`, POSITION = POS)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)

Days_Length <- intersect(read_tsv("./data/GWAS/FlowerDays_2025_bin.tsv")$ID, 
                         read_tsv("./data/GWAS/Stem_Length_20242025_bin.tsv")$ID)
Days_Length
Length_Stem <- intersect(read_tsv("./data/GWAS/Stem_Length_20242025_bin.tsv")$ID,
                         read_tsv("./data/GWAS/Num_Stems_2025_bin.tsv")$ID)
Length_Stem
Days_Stem <- intersect(read_tsv("./data/GWAS/FlowerDays_2025_bin.tsv")$ID, 
                       read_tsv("./data/GWAS/Num_Stems_2025_bin.tsv")$ID)
Days_Stem

Num_Stems_gene <- readxl::read_xlsx("./data/GWAS/GLM_sig_markers_Num_Stems_protein_function_candidate.xlsx")%>%
  mutate(chr = as.integer(str_remove(gene_chr, "Chr_")), POSITION = (gene_start + gene_end)/2)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)

FlowerDays_2025_gene <- readxl::read_xlsx("./data/GWAS/GLM_sig_markers_FlowerDays_2025_protein_function_candidate.xlsx")%>%
  mutate(chr = as.integer(str_remove(gene_chr, "Chr_")), POSITION = (gene_start + gene_end)/2)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)

Stem_Length_gene <- readxl::read_xlsx("./data/GWAS/GLM_sig_markers_Stem_Length_protein_function_candidate.xlsx")%>%
  mutate(chr = as.integer(str_remove(gene_chr, "Chr_")), POSITION = (gene_start + gene_end)/2)%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad)%>%
  arrange(padded_pos)

glm_input
for (i in 1:7){
  
  i = 5
  
  GLM_data <- fread(file = paste0(glm_path, glm_input[[i]]))%>%
    rename(chr = `#CHROM`, POSITION = POS, pvalue = P)%>%
    left_join(chr_len_temp, by = "chr") %>%
    mutate(padded_pos = POSITION + pos_pad) %>%
    mutate(dot_color = case_when(pvalue < 5e-8 ~ "red", TRUE ~ "gray80")) 
  
  # Plot result of PCadapt analysis
  pGLM <- ggplot(data = GLM_data, aes(x = padded_pos, y = -log10(pvalue))) +
    geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
    geom_point(data = filter(GLM_data, dot_color == "gray80"), color = "gray80", size = 0.1) +
    # geom_segment(data = Num_Stems_2025_bin, aes(y = -log10(GWAS_P), yend = -log10(GWAS_P) + 1), linewidth = 0.3)+
    geom_point(data = filter(GLM_data, dot_color == "red"), color = "#00AAFF", size = 0.5) +
    # geom_label(data = Num_Stems_2025_bin, aes(y = -log10(GWAS_P) + 1, label = bin_Name), label.r = unit(0.04, units = "in"),size = 2) +
    annotate(geom = "text", x = 5e6, y = 0, label = "Number of stems 2025", hjust = 0, vjust = -1, check_overlap = TRUE)+
    geom_hline(yintercept = -log10(5e-8), color = "red", linewidth = 0.5, alpha = 0.5) +
    geom_segment(data = Num_Stems_gene, aes(xend = padded_pos), y = 0, yend = 5, linewidth = 0.2)+
    geom_label(data = Num_Stems_gene, aes(label = gene), y = c(5,6.3,5,5,5), linewidth = 0.2, size = 2.5, position = position_nudge(x = rep(-2e6, 5)))+
    scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
    scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
    scale_y_continuous(expression("-" * log[10] * "(p value)"), expand = c(0, 0, 0.1, 0.1)) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
    guides(fill = "none")
  
  # Save the combined figure
  ggsave(paste0("./figures/glm_output/", str_remove(glm_input[[i]], ".txt"), ".png"), width = 7.5, height = 2, dpi = 600)
  
  # Q-Q plot for PCadapt result
  p_QQ_PCadapt <- ggplot(data = GLM_data, aes(sample = -log(pvalue))) +
    stat_qq(aes(x = after_stat(theoretical) / log(10), y = after_stat(sample) / log(10)), distribution = qexp, size = 0.3) +
    stat_qq_line(aes(x = after_stat(x) / log(10), y = after_stat(y) / log(10)), distribution = qexp, color = "green4", linewidth = 0.3, alpha = 0.5) +
    geom_hline(yintercept = -log10(5e-8), color = "red", linewidth = 0.3, alpha = 0.5) +
    xlab(expression("Theoretical" ~ "-" * log[10] * "(p)")) +
    ylab(expression("Observed" ~ "-" * log[10] * "(p)")) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  ggsave(paste0("./figures/LFMM_output/", str_remove(glm_input[[i]], ".txt"), "_QQplot",".png"), width = 2, height = 2, dpi = 600)
  
  p_comb <- plot_grid(pGLM, p_QQ_PCadapt, align = "h", nrow = 1, rel_widths = c(7.5, 2))
  
  ggsave(paste0("./figures/glm_output/", str_remove(glm_input[[i]], ".txt"), "_combqq",".png"), width = 9.5, height = 2, dpi = 600)
  
  i = 2
  
  GLM_data <- fread(file = paste0(glm_path, glm_input[[i]]))%>%
    rename(chr = `#CHROM`, POSITION = POS, pvalue = P)%>%
    left_join(chr_len_temp, by = "chr") %>%
    mutate(padded_pos = POSITION + pos_pad) %>%
    mutate(dot_color = case_when(pvalue < 5e-8 ~ "#00AAFF", TRUE ~ "gray80")) 
  
  # Plot result of PCadapt analysis
  pGLM <- ggplot(data = GLM_data, aes(x = padded_pos, y = -log10(pvalue))) +
    geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
    geom_point(data = filter(GLM_data, dot_color == "gray80"), color = "gray80", size = 0.1) +
    # geom_segment(data = FlowerDays_2025_bin, aes(y = -log10(GWAS_P), yend = -log10(GWAS_P) + 1 + c(0,0.8,1.6,2.4,0,0,0,0,0,1.2,0,1.5,1,0,0)), linewidth = 0.1)+
    geom_point(data = filter(GLM_data, dot_color == "#00AAFF"), color = "#00AAFF", size = 0.5) +
    geom_point(data = BayPass_sig, aes(y = -log10(GWAS_P)),fill = "#ffe136", size = 2, shape = 21, stroke = .1)+
    # geom_label(data = FlowerDays_2025_bin, aes(y = -log10(GWAS_P) + 1, label = bin_Name), label.r = unit(0.04, units = "in"),size = 2,
               # position = position_nudge(y = c(0,0.8,1.6,2.4,0,0,0,0,0,1.2,0,1.5,1,0,0)),linewidth = 0.2) +
    annotate(geom = "text", x = 5e6, y = 0, label = "Days of first flower 2025", hjust = 0, vjust = -1, check_overlap = TRUE)+
    geom_hline(yintercept = -log10(5e-8), color = "red", linewidth = 0.1, alpha = 0.7) +
    geom_segment(data = FlowerDays_2025_gene, aes(xend = padded_pos), y = 0, yend = 5, linewidth = 0.2)+
    geom_label(data = FlowerDays_2025_gene, aes(label = gene), y = c(5,5,5,6.3), linewidth = 0.2, size = 2.5, position = position_nudge(x = rep(-2e6, 4)))+
    scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
    scale_x_continuous("", expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
    scale_y_continuous(expression("-" * log[10] * "(p value)"), expand = c(0, 0, 0.1, 0.1)) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
    guides(fill = "none")
  
  # Save the combined figure
  ggsave(paste0("./figures/glm_output/", str_remove(glm_input[[i]], ".txt"), ".png"), width = 7.5, height = 2, dpi = 600)
  
  # Q-Q plot for PCadapt result
  p_QQ_PCadapt <- ggplot(data = GLM_data, aes(sample = -log(pvalue))) +
    stat_qq(aes(x = after_stat(theoretical) / log(10), y = after_stat(sample) / log(10)), distribution = qexp, size = 0.3) +
    stat_qq_line(aes(x = after_stat(x) / log(10), y = after_stat(y) / log(10)), distribution = qexp, color = "green4", linewidth = 0.3, alpha = 0.5) +
    geom_hline(yintercept = -log10(5e-8), color = "red", linewidth = 0.3, alpha = 0.5) +
    xlab(expression("Theoretical" ~ "-" * log[10] * "(p)")) +
    ylab(expression("Observed" ~ "-" * log[10] * "(p)")) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  ggsave(paste0("./figures/LFMM_output/", str_remove(glm_input[[i]], ".txt"), "_QQplot",".png"), width = 2, height = 2, dpi = 600)
  
  p_comb <- plot_grid(pGLM, p_QQ_PCadapt, align = "h", nrow = 1, rel_widths = c(7.5, 2))
  
  ggsave(paste0("./figures/glm_output/", str_remove(glm_input[[i]], ".txt"), "_combqq",".png"), width = 9.5, height = 2, dpi = 600)
  
  i = 7
  
  GLM_data <- fread(file = paste0(glm_path, glm_input[[i]]))%>%
    rename(chr = `#CHROM`, POSITION = POS, pvalue = P)%>%
    left_join(chr_len_temp, by = "chr") %>%
    mutate(padded_pos = POSITION + pos_pad) %>%
    mutate(dot_color = case_when(pvalue < 5e-8 ~ "#00AAFF", TRUE ~ "gray80")) 
  
  # Plot result of PCadapt analysis
  pGLM <- ggplot(data = GLM_data, aes(x = padded_pos, y = -log10(pvalue))) +
    geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
    geom_point(data = filter(GLM_data, dot_color == "gray80"), color = "gray80", size = 0.1) +
    # geom_segment(data = Stem_Length_20242025_bin, aes(y = -log10(GWAS_P), yend = -log10(GWAS_P) + 1 + c(1,0,0,0,0,0,0,0.5,0,0,1,0,0,0,0,0,0,0)), linewidth = 0.1)+
    geom_point(data = filter(GLM_data, dot_color == "#00AAFF"), color = "#00AAFF", size = 0.5) +
    # geom_label(data = Stem_Length_20242025_bin, aes(y = -log10(GWAS_P) + 1, label = bin_Name), label.r = unit(0.04, units = "in"),size = 2,
    #            position = position_nudge(y = c(1,0,0,0,0,0,0,0.5,0,0,1,0,0,0,0,0,0,0)),linewidth = 0.2) +
    geom_point(data = LFMM_GWAS, aes(y = -log10(GWAS_P)),fill = "#ffe136", size = 2, shape = 21, stroke = .1)+
    annotate(geom = "text", x = 5e6, y = 0, label = "Stem length 2025", hjust = 0, vjust = -1, check_overlap = TRUE)+
    geom_hline(yintercept = -log10(5e-8), color = "red", linewidth = 0.1, alpha = 0.7) +
    geom_segment(data = Stem_Length_gene, aes(xend = padded_pos), y = 0, yend = c(5,5,4,4,4,5), linewidth = 0.2)+
    geom_label(data = Stem_Length_gene, aes(label = gene), y = c(5,5,4,5.3,6.6,5), linewidth = 0.2, size = 2.5, position = position_nudge(x = rep(-2e6, 6)))+
    scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
    scale_x_continuous("", expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
    scale_y_continuous(expression("-" * log[10] * "(p value)"), expand = c(0, 0, 0.1, 0.1)) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
    guides(fill = "none")
  
  # Save the combined figure
  ggsave(paste0("./figures/glm_output/", str_remove(glm_input[[i]], ".txt"), ".png"), width = 7.5, height = 2, dpi = 600)
  
  # Q-Q plot for PCadapt result
  p_QQ_PCadapt <- ggplot(data = GLM_data, aes(sample = -log(pvalue))) +
    stat_qq(aes(x = after_stat(theoretical) / log(10), y = after_stat(sample) / log(10)), distribution = qexp, size = 0.3) +
    stat_qq_line(aes(x = after_stat(x) / log(10), y = after_stat(y) / log(10)), distribution = qexp, color = "green4", linewidth = 0.3, alpha = 0.5) +
    geom_hline(yintercept = -log10(5e-8), color = "red", linewidth = 0.3, alpha = 0.5) +
    xlab(expression("Theoretical" ~ "-" * log[10] * "(p)")) +
    ylab(expression("Observed" ~ "-" * log[10] * "(p)")) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  ggsave(paste0("./figures/LFMM_output/", str_remove(glm_input[[i]], ".txt"), "_QQplot",".png"), width = 2, height = 2, dpi = 600)
  
  p_comb <- plot_grid(pGLM, p_QQ_PCadapt, align = "h", nrow = 1, rel_widths = c(7.5, 2))
  
  ggsave(paste0("./figures/glm_output/", str_remove(glm_input[[i]], ".txt"), "_combqq",".png"), width = 9.5, height = 2, dpi = 600)
}

head(GLM_data)

## Extracting markers with p < 5e-8

glm_input

paste(str_split(glm_input[[1]], pattern = "[.]")[[1]][2:3], collapse = "_")

GLM_sig_markers = list()
for (i in c(1:3, 5:7)){
  
  GLM_data <- fread(file = paste0(glm_path, glm_input[[i]]))%>%
    filter(P < 5e-8)%>%
    select(`#CHROM`, POS, REF, ALT, P, BETA, SE)%>%
    mutate(Trait = paste(str_split(glm_input[[i]], pattern = "[.]")[[1]][2:3], collapse = "_"))
  
  GLM_sig_markers[[i]] <- GLM_data
  
  rm(GLM_data)
  gc()

}

GLM_sig_markers_all <- bind_rows(GLM_sig_markers)%>%
  rename(GWAS_P = P, GWAS_BETA = BETA, GWAS_SE = SE, GWAS_TRAIT = Trait)%>%
  arrange(`#CHROM`, POS)


write_tsv(GLM_sig_markers_all, "./data/GWAS/GLM_sig_markers_all.tsv")
GLM_sig_markers_all
