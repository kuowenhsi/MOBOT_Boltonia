library(tidyverse)
library(data.table)
library(qvalue)
library(cowplot)
library(zoo)
library(VennDiagram)
library(ggpubr)
library(rstatix)

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


# Download LFMM_output_20230606.txt from Dryad https://doi.org/10.5061/dryad.s7h44j1fd

glm_path <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/asteroides_decurrens/"


Pi_input <- sort(list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/asteroides_decurrens", pattern = ".windowed.pi" ))

TajD_input <- sort(list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/asteroides_decurrens", pattern = ".Tajima.D" ))

Fst_input <- sort(list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/asteroides_decurrens", pattern = ".fst" ))

fst_data <-read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/asteroides_decurrens/decurrens_vs_asteroides.windowed.weir.fst")%>%
  filter(N_VARIANTS >= 5)%>%
  drop_na()%>%
  mutate(POS = (BIN_START + BIN_END - 1) / 2, ID = paste(CHROM, POS, sep = "_"))%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(padded_pos = POS + pos_pad) %>%
  mutate(color = (WEIGHTED_FST > 0.6667138))

quantile(fst_data$WEIGHTED_FST, 0.99)

Pi_data <- lapply(paste0(glm_path, Pi_input), read_tsv)
Pi_data[[1]] <- Pi_data[[1]] %>% rename(asteroides_N_VARIANTS = N_VARIANTS, asteroides_PI = PI)
Pi_data[[2]] <- Pi_data[[2]] %>% rename(decurrens_N_VARIANTS = N_VARIANTS, decurrens_PI = PI)
Pi_data_c <- full_join(Pi_data[[1]], Pi_data[[2]], by = c("CHROM", "BIN_START", "BIN_END") )%>%
  filter(asteroides_N_VARIANTS >= 5, decurrens_N_VARIANTS >= 5)%>%
  drop_na()%>%
  mutate(theta = asteroides_PI / decurrens_PI, POS = (BIN_START + BIN_END - 1) / 2, ID = paste(CHROM, POS, sep = "_"))%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(padded_pos = POS + pos_pad) %>%
  mutate(color = (theta > 7.643564))

Pi_data <- lapply(paste0(glm_path, Pi_input), read_tsv)
Pi_data_l <-bind_rows(Pi_data[[1]]%>%mutate(Sample_Species = "B. asteroides"), Pi_data[[2]]%>%mutate(Sample_Species = "B. decurrens"))%>%
  filter(N_VARIANTS >= 20)%>%
  drop_na()

stat.test <- Pi_data_l %>%
  t_test(PI ~ Sample_Species) %>%
  add_significance()%>%
  add_xy_position(x = "Sample_Species")
stat.test

pi <- ggplot(data = Pi_data_l,aes(x = Sample_Species, y = PI))+
  geom_violin(fill = "gray85", color = "black",linewidth = 0.2)+
  geom_boxplot(width = 0.25, median.color = "red", linewidth = 0.2, outlier.size = 0.2)+
  stat_pvalue_manual(stat.test, label = "p.signif", vjust = 0, bracket.nudge.y = .1) +
  scale_x_discrete(NULL)+
  scale_y_sqrt(expression(theta[pi]),expand = expansion(mult = c(0.05, 0.15)))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12))

pi

quantile(Pi_data_c$theta, 0.99)
hist(Pi_data_c$asteroides_N_VARIANTS, breaks = 100)

intersect_asteroides <- intersect(filter(Pi_data_c, color == TRUE)$ID, filter(TajD_data[[1]], color == TRUE)$ID)%>%
  intersect(filter(fst_data, color == TRUE)$ID)
intersect_decurrens <- intersect(filter(Pi_data_c, color == TRUE)$ID, filter(TajD_data[[2]], color == TRUE)$ID)%>%
  intersect(filter(fst_data, color == TRUE)$ID)

intersect_decurrens_tb <- tibble(ID = intersect_decurrens) %>%
  mutate(CHROM = as.integer(str_split_i(ID, "_",1)), POSITION = as.integer(str_split_i(ID, "_", 2)))%>%
  mutate(START = POSITION - 5000L, END = POSITION + 5000L)

write_tsv(intersect_decurrens_tb, "./data/asteroides_decurrens/intersect_decurrens_tb.tsv")


intersect_decurrens_genes <- readxl::read_xlsx("./data/asteroides_decurrens/intersect_decurrens_window_proteins_function_candidate.xlsx") %>%
  mutate(CHROM = as.integer(str_remove(gene_chr, "Chr_")), POSITION = (gene_start + gene_end)/2)%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(padded_pos = POSITION + pos_pad)

Y_POS = c(90, 1300, 300, 600, 150,
          450, 1300, 3000, 800, 200,
          800, 150, 300, 300, 5000,
          1000, 2000)

p1 <- ggplot(data = Pi_data_c, aes(x = padded_pos, y = theta))+
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = 0.2, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_segment(data = intersect_decurrens_genes, aes(xend = padded_pos), y = log10(0.2), yend = log10(Y_POS), linewidth = 0.2, color = "gray65")+
  geom_point(aes(color = color),size = 0.5, show.legend = FALSE)+
  geom_point(data = filter(Pi_data_c, ID %in% intersect_decurrens), color = "#FF6580",size = 1, show.legend = FALSE)+
  geom_label(data = intersect_decurrens_genes, aes(label = gene), y = log10(Y_POS), size = 2.5)+
  annotate(geom = "text", label = "Genetic diversity ratio", x = Inf, y = Inf, hjust = 1.05, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_color_manual(values = c("gray80", "#66DDAA"))+
  scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous(expression(theta[pi*"-asteroides"]~"/"~theta[pi*"-decurrens"]), expand = c(0, 0, 0.1, 1), transform = "log10", limits = c(0.2,700)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 1), "lines")) +
  guides(fill = "none")

p1

ggsave("./figures/asteroides_decurrens/asteroides_decurrens_theta.png", width = 7.5, height = 3)

TajD_data <- lapply(paste0(glm_path, TajD_input), read_tsv)
TajD_data[[1]] <- TajD_data[[1]] %>% rename(asteroides_N_SNPS = N_SNPS, asteroides_TajimaD = TajimaD)%>%
  filter(asteroides_N_SNPS >= 5)%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(POS = BIN_START + 5000, ID = paste(CHROM, POS, sep = "_"))%>%
  mutate(padded_pos = POS + pos_pad)%>%
  drop_na()%>%
  mutate(color = (asteroides_TajimaD < -1.571308))
TajD_data[[2]] <- TajD_data[[2]] %>% rename(decurrens_N_SNPS = N_SNPS, decurrens_TajimaD = TajimaD)%>%
  filter(decurrens_N_SNPS >= 5)%>%
  left_join(chr_len_temp, by = c("CHROM" = "chr"))%>%
  mutate(POS = BIN_START + 5000, ID = paste(CHROM, POS, sep = "_"))%>%
  mutate(padded_pos = POS + pos_pad)%>%
  drop_na()%>%
  mutate(color = (decurrens_TajimaD < -1.607281))

TajD_data <- lapply(paste0(glm_path, TajD_input), read_tsv)
TajD_data_l <- bind_rows(TajD_data[[1]]%>%mutate(Sample_Species = "B. asteroides"), TajD_data[[2]]%>%mutate(Sample_Species = "B. decurrens"))%>%
  filter(N_SNPS >= 20)%>%
  drop_na()

stat.test <- TajD_data_l %>%
  t_test(TajimaD ~ Sample_Species) %>%
  add_significance()%>%
  add_xy_position(x = "Sample_Species")
stat.test

TajD <- ggplot(data = TajD_data_l,aes(x = Sample_Species, y = TajimaD))+
  geom_violin(fill = "gray85", color = "black",linewidth = 0.2)+
  geom_boxplot(width = 0.25, median.color = "red", linewidth = 0.2, outlier.size = 0.2)+
  stat_pvalue_manual(stat.test, label = "p.signif", vjust = 0, bracket.nudge.y = .1) +
  scale_x_discrete(NULL)+
  scale_y_continuous("Tajima's D",expand = expansion(mult = c(0.05, 0.15)))+
  theme_bw()+
  theme(axis.title.y = element_text(size = 8))

TajD

plot_grid(pi, TajD, align = "h")
ggsave("./figures/asteroides_decurrens/theta_pi.png", width = 5, height = 3.5)


quantile(TajD_data[[1]]$asteroides_TajimaD, 0.01)
quantile(TajD_data[[2]]$decurrens_TajimaD, 0.01)
hist(TajD_data[[1]]$asteroides_N_SNPS, breaks = 100)
hist(TajD_data[[2]]$decurrens_N_SNPS, breaks = 100)

venn.diagram(x = list(Fst = filter(fst_data, color == TRUE)$ID, theta = filter(Pi_data_c, color == TRUE)$ID, TajimaD_decurrens = filter(TajD_data[[2]], color == TRUE)$ID), filename = "./figures/asteroides_decurrens/venn_decurrens.png", width = 3, height = 3,units = "in")
venn.diagram(x = list(Fst = filter(fst_data, color == TRUE)$ID, theta = filter(Pi_data_c, color == TRUE)$ID, TajimaD_asteroides = filter(TajD_data[[1]], color == TRUE)$ID), filename = "./figures/asteroides_decurrens/venn_asteroides.png", width = 4, height = 4, units = "in")

venn_list <- list(
  Fst_outliers        = filter(fst_data,  color)$ID,
  Pi_outliers         = filter(Pi_data_c, color)$ID,
  TajimasD_outliers   = filter(TajD_data[[2]], color)$ID
)

venn.diagram(
  x        = venn_list,
  filename = "./figures/asteroides_decurrens/venn_decurrens.png",
  
  # image size & quality (pixels, not inches)
  width       = 2000,
  height      = 2000,
  resolution  = 300,
  imagetype   = "png",
  
  # circle style
  col   = "black",
  lwd   = 2,
  fill  = c("#E69F00", "#56B4E9", "#009E73"),
  alpha = 0.5,
  
  # numbers in regions
  cex        = 1.4,
  fontface   = "bold",
  
  # category labels
  cat.cex      = 1.5,
  cat.fontface = "bold",
  cat.col      = c("#E69F00", "#56B4E9", "#009E73"),
  cat.dist     = c(0.05, 0.05, 0.05),
  
  # title
  main      = "Overlap among Fst, π and Tajima's D outliers in B. decurrens",
  main.cex  = 1.5
)

p2 <- ggplot(data = TajD_data[[1]], aes(x = padded_pos, y = asteroides_TajimaD))+
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(aes(color = color),size = 0.5, show.legend = FALSE)+
  geom_point(data = filter(TajD_data[[1]], ID %in% intersect_asteroides), color = "#FF6580",size = 1, show.legend = FALSE)+
  annotate(geom = "text", label = "B. asteroides", x = Inf, y = Inf, hjust = 1.05, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_color_manual(values = c("gray80", "#66DDAA"))+
  scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous(expression("Tajima's D"), expand = c(0, 0.1, 0.1, 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 1.5), "lines")) +
  guides(fill = "none")

p2

ggsave("./figures/asteroides_decurrens/asteroides_tajimaD.png", width = 7.5, height = 1.75)

p3 <- ggplot(data = TajD_data[[2]], aes(x = padded_pos, y = decurrens_TajimaD))+
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(aes(color = color),size = 0.5, show.legend = FALSE)+
  geom_point(data = filter(TajD_data[[2]], ID %in% intersect_decurrens), color = "#FF6580",size = 1, show.legend = FALSE)+
  annotate(geom = "text", label = "B. decurrens", x = Inf, y = Inf, hjust = 1.05, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_color_manual(values = c("gray80", "#66DDAA"))+
  scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous(expression("Tajima's D"), expand = c(0, 0.1, 0.1, 1)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 1.5), "lines")) +
  guides(fill = "none")

p3

ggsave("./figures/asteroides_decurrens/decurrens_tajimaD.png", width = 7.5, height = 1.75)


p4 <- ggplot(data = fst_data, aes(x = padded_pos, y = WEIGHTED_FST))+
  geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
  geom_point(aes(color = color),size = 0.5, show.legend = FALSE)+
  geom_point(data = filter(fst_data, ID %in% intersect_decurrens), color = "#FF6580",size = 1, show.legend = FALSE)+
  annotate(geom = "text", label = "B. asteroides vs. B. decurrens", x = Inf, y = Inf, hjust = 1.05, vjust = 1.4)+
  scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
  scale_color_manual(values = c("gray80", "#66DDAA"))+
  scale_x_continuous(NULL, expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
  scale_y_continuous(expression("Weighted"~F[ST]), expand = c(0, 0.1, 0.1, 0.3)) +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 1.5), "lines"), ) +
  guides(fill = "none")

p4

ggsave("./figures/asteroides_decurrens/Fst.png", width = 7.5, height = 1.75)


p_comb <- plot_grid(p4,p1,p2,p3, ncol = 1, align = "v", labels = c("A","B","C","D"), label_size = 12, label_fontface = "plain")

ggsave("./figures/asteroides_decurrens/asteroides_decurrens_Comb.png", width = 7.5, height = 1.75*4)
