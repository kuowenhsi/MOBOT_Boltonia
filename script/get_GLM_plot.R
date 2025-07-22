library(tidyverse)
library(data.table)
library(qvalue)
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


# Download LFMM_output_20230606.txt from Dryad https://doi.org/10.5061/dryad.s7h44j1fd

glm_path <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/alloutgroup/VCF_decurrens/GLM_Boltonia_decurrens_100kb0.8/"
glm_input <- list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/alloutgroup/VCF_decurrens/GLM_Boltonia_decurrens_100kb0.8")
length(glm_input)


for (i in 1:38){
  
  GLM_data <- fread(file = paste0(glm_path, glm_input[[i]]))%>%
    rename(chr = `#CHROM`, POSITION = POS, pvalue = P)%>%
    left_join(chr_len_temp, by = "chr") %>%
    mutate(padded_pos = POSITION + pos_pad) %>%
    mutate(dot_color = case_when(pvalue < 5e-8 ~ "black", TRUE ~ "gray80")) 
  
  # Plot result of PCadapt analysis
  pGLM <- ggplot(data = GLM_data, aes(x = padded_pos, y = -log10(pvalue))) +
    geom_rect(data = chr_len_temp, aes(xmin = padded_start, xmax = padded_end, ymin = -Inf, ymax = Inf, fill = factor(chr, levels = 1:9)), inherit.aes = FALSE) +
    geom_point(color = GLM_data$dot_color, size = 0.5) +
    geom_hline(yintercept = -log10(5e-8), color = "red", linewidth = 0.5, alpha = 0.5) +
    scale_fill_manual(name = "", values = rep(c("white", "gray95"), 8)) +
    scale_x_continuous("", expand = c(0, 0), breaks = chr_len_temp$padded_chr_pos, labels = 1:9) +
    scale_y_continuous(expression("-" * log[10] * "(p value)"), expand = c(0, 0, 0.1, 0.1)) +
    theme_bw() +
    theme(panel.grid = element_blank(), plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "lines")) +
    guides(fill = "none")
  
  # Save the combined figure
  ggsave(paste0("./figures/glm_output/", str_remove(glm_input[[i]], ".glm.linear_ADD.txt"), ".png"), width = 10, height = 3.5, dpi = 600)
}







LFMM_data_sub_q <- qvalue(LFMM_data_sub$V4)
hist(LFMM_data_sub_q)