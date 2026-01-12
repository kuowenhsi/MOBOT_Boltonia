library(tidyverse)
library(data.table)
library(cowplot)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


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

GWAS_sig <- read_tsv("./data/GWAS/GLM_sig_markers_all.tsv")

PCadapt_data <- fread("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/PCadapt_output/PCadapt_K_5_20251014.txt.gz")%>%
  left_join(chr_len_temp, by = c("CHR" = "chr")) %>%
  mutate(padded_pos = POSITION + pos_pad, threshold = quantile(pvalue, 1-0.999)) %>%
  mutate(dot_color = case_when(pvalue < threshold ~ "#66DDAA", TRUE ~ "gray80")) %>%
  mutate(name = "PCadapt")

quantile(PCadapt_data$pvalue, 1-0.999)

PCadapt_sig <- PCadapt_data %>%
  filter(dot_color == "#66DDAA")

PCadapt_GWAS <- PCadapt_data %>%
  inner_join(GWAS_sig, by = c("CHR" = "#CHROM", "POSITION"  = "POS"))%>%
  mutate(IF_sig = (pvalue < quantile(PCadapt_data$pvalue, 1-0.999)))

LFMM_data <- fread(file = "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/PCadapt_output/Boltonia_LFMM_output_K_5_wc2.1_30s_bio_7_20251003.txt.gz", col.names = c("chr", "POSITION", "ID", "pvalue", "climatic_varible", "K") )%>%
  left_join(chr_len_temp, by = "chr") %>%
  mutate(padded_pos = POSITION + pos_pad, threshold = quantile(pvalue, 1-0.999)) %>%
  mutate(dot_color = case_when(pvalue < threshold ~ "#66DDAA", TRUE ~ "gray80")) 

quantile(LFMM_data$pvalue, 1-0.999)

LFMM_sig <- LFMM_data %>%
  filter(dot_color == "#66DDAA")

LFMM_GWAS <- LFMM_data %>%
  inner_join(GWAS_sig, by = c("chr" = "#CHROM", "POSITION"  = "POS"))%>%
  mutate(IF_sig = (pvalue < quantile(LFMM_data$pvalue, 1-0.999)))

intersect_sig <- intersect(PCadapt_sig$ID, LFMM_sig$ID)

all_intersect <- read_tsv("./data/BayPass/intersect_sig_tbl.tsv") %>%
  left_join(read_tsv("./data/LFMM_PCadapt/LFMM_PCadapt_sig.tsv"), by = c("CHR" = "chr", "POS"  = "POSITION"))%>%
  filter(!is.na(ID))%>%
  left_join(chr_len_temp, by = c("CHR" = "chr"))%>%
  mutate(padded_pos = POS + pos_pad)


all_intersect <- all_intersect %>%
  rename(chr = CHR, POSITION = POS)%>%
  left_join(LFMM_data[,c(1,2,4)], by = c("chr", "POSITION"))%>%
  mutate(CHR = chr)%>%
  left_join(PCadapt_data[,c(1,4,7)], by = c("CHR", "POSITION"))

LFMM_PCadapt_sig <- tibble(ID = intersect_sig)%>%
  left_join(LFMM_data, by = "ID")%>%
  select(1:3)

# write_tsv(LFMM_PCadapt_sig, "./data/LFMM_PCadapt/LFMM_PCadapt_sig.tsv")

#plot LFMM with only 1:17410001 to 1:17930000

adapt_gene <- readxl::read_xlsx("./data/BayPass/structural_adaptation_candidates.xlsx")%>%
  mutate(CHROM = as.integer(str_remove(gene_chr, "Chr_")), POSITION = (gene_start + gene_end)/2)%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(padded_pos = POSITION + pos_pad)
mechanism_gene <- readxl::read_xlsx("./data/BayPass/structural_mechanism_candidates.xlsx")%>%
  mutate(CHROM = as.integer(str_remove(gene_chr, "Chr_")), POSITION = (gene_start + gene_end)/2)%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(padded_pos = POSITION + pos_pad)

Y_POS = c(5,7,9,18,22,
          24,30,34,15,
          17,30,28,26,24,
          22,20,18,16, 14)
Y_POS2 = c(12,16,20,22,12,
          28,30,32,34,
          36,3,26,24)

pLFMM <- ggplot(data = LFMM_data, aes(x = padded_pos, y = -log10(pvalue))) +
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_segment(data = adapt_gene, aes(xend = padded_pos), y = 0, yend = Y_POS, linewidth = 0.2, color = "gray65")+
  geom_segment(data = mechanism_gene, aes(xend = padded_pos), y = 0, yend = Y_POS2, linewidth = 0.2, color = "#00AAFF")+
  geom_point(data = filter(LFMM_data, dot_color == "gray80"), color = "gray80", size = .1, show.legend = FALSE)+
  geom_point(data = filter(LFMM_data, dot_color == "#66DDAA"), color = "#66DDAA", size = .1, show.legend = FALSE)+
  geom_point(data = filter(LFMM_data, ID %in% intersect_sig), color = "#FF6580", size = .2, show.legend = FALSE)+
  geom_point(data = all_intersect, aes(y = -log10(pvalue.x)),fill = "#F1CEFF", size = 1.5, shape = 21, stroke = .1)+
  geom_point(data = filter(LFMM_GWAS, IF_sig == FALSE), color = "#00AAFF", size = .4)+
  geom_point(data = filter(LFMM_GWAS, IF_sig == TRUE), fill = "#ffe136", size = 2, shape = 21, stroke = .1)+
  geom_segment(data = filter(LFMM_GWAS, IF_sig == TRUE), aes(x = padded_pos, y = -log10(pvalue) +10, yend = -log10(pvalue) +5), arrow.fill = "#ffe136", arrow = arrow(type = "closed", length = unit(0.05, "inches")), linewidth = 0.2)+
  geom_label(data = adapt_gene, aes(label = gene), y = Y_POS, size = 2.5)+
  geom_label(data = mechanism_gene, aes(label = gene), y = Y_POS2, size = 2.5, fill = "#00AAFF", alpha = 0.6)+
  annotate(geom = "text", label = "LFMM", x = Inf, y = Inf, hjust = 1.2, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_x_continuous(NULL, expand = c(0, 0), limits = c(17310001, 21310000))+
  scale_y_continuous(expression("-" * log[10] * "(p value)"), expand = c(0, 0, 0.1, 0.1), limits = c(0,36)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
  guides(fill = "none")



ggsave("./figures/PCadapt_LFMM/chr_1_structural_variant.png", width = 7, height = 3.5)


# Plot result of PCadapt analysis
pLFMM <- ggplot(data = LFMM_data, aes(x = padded_pos, y = -log10(pvalue))) +
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(data = filter(LFMM_data, dot_color == "gray80"), color = "gray80", size = .1, show.legend = FALSE)+
  geom_point(data = filter(LFMM_data, dot_color == "#66DDAA"), color = "#66DDAA", size = .1, show.legend = FALSE)+
  geom_point(data = filter(LFMM_data, ID %in% intersect_sig), color = "#FF6580", size = .2, show.legend = FALSE)+
  geom_point(data = all_intersect, aes(y = -log10(pvalue.x)),fill = "#F1CEFF", size = 1.5, shape = 21, stroke = .1)+
  geom_point(data = filter(LFMM_GWAS, IF_sig == FALSE), color = "#00AAFF", size = .4)+
  geom_point(data = filter(LFMM_GWAS, IF_sig == TRUE), fill = "#ffe136", size = 2, shape = 21, stroke = .1)+
  geom_segment(data = filter(LFMM_GWAS, IF_sig == TRUE), aes(x = padded_pos, y = -log10(pvalue) +10, yend = -log10(pvalue) +5), arrow.fill = "#ffe136", arrow = arrow(type = "closed", length = unit(0.05, "inches")), linewidth = 0.2)+
  annotate(geom = "text", label = "LFMM", x = Inf, y = Inf, hjust = 1.2, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous(expression("-" * log[10] * "(p value)"), expand = c(0, 0, 0.1, 0.1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
  guides(fill = "none")

ggsave("./figures/PCadapt_LFMM/LFMM_K5_single.png", width = 7.5, height = 2.5)

# Plot result of PCadapt analysis
pcap <- ggplot(data = PCadapt_data, aes(x = padded_pos, y = -log10(pvalue))) +
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(data = filter(PCadapt_data, dot_color == "gray80"), color = "gray80", size = .1, show.legend = FALSE)+
  geom_point(data = filter(PCadapt_data, dot_color == "#66DDAA"), color = "#66DDAA", size = .1, show.legend = FALSE)+
  geom_point(data = filter(PCadapt_data, ID %in% intersect_sig), color = "#FF6580", size = .2, show.legend = FALSE)+
  geom_point(data = all_intersect, aes(y = -log10(pvalue.y)),fill = "#F1CEFF", size = 1.5, shape = 21, stroke = .1)+
  geom_point(data = filter(PCadapt_GWAS, IF_sig == FALSE), color = "#00AAFF", size = .4)+
  geom_point(data = filter(PCadapt_GWAS, IF_sig == TRUE), fill = "#ffe136", size = 2, shape = 21, stroke = .1)+
  annotate(geom = "text", label = "PCadapt", x = Inf, y = Inf, hjust = 1.2, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous(expression("-" * log[10] * "(p value)"), expand = c(0, 0, 0.1, 0.1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
  guides(fill = "none")

ggsave("./figures/PCadapt_LFMM/PCadapt_K5_single.png", width = 7.5, height = 2.5)

p_comb <- cowplot::plot_grid(pcap, pLFMM, align = "v", ncol = 1, labels = c("(A)", "(B)"), label_size = 12, label_fontface = "plain")

ggsave("./figures/PCadapt_LFMM/PCadapt_LFMM_K5_comb.png", width = 7.5, height = 3.5, dpi = 600)

p_comb <- cowplot::plot_grid(pcap, pLFMM, p1, p2, align = "v", ncol = 1, labels = c("A", "B", "C", "D"), label_size = 12, label_fontface = "plain")

ggsave("./figures/PCadapt_LFMM/PCadapt_LFMM_K5_BayPass_comb.png", width = 7.5, height = 7, dpi = 600)
