library(tidyverse)
library(RColorBrewer)

###Admixture on loop, with and without labels###

#changing the following 3 lines (and K range below) should make the 
#code work with any files

setwd("/Users/User/Desktop/MOBOT_Boltonia")
file_prefix <- "./Data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8." #file prefix (used for .Q and .fam)
file_out_prefix <- "./Figures/Boltonia_decurrens_ADMIXTURE_K" #file output prefix and destination
metadata <- read_csv("./Data/DNA_stock_Boltonia.csv") |>
  mutate(Sample_Name = paste0("Boltonia_", str_pad(index, 3, "left","0")))

#change range of K values in following line "(i in #:#)"
for (i in 2:12) {
  #ADMIXTURE structure with K = i
  file_name <- paste0(file_prefix, i, ".Q")
  save_name1 <- paste0(file_out_prefix, i, "_labels.png")
  save_name2 <- paste0(file_out_prefix, i, ".png")
  qmat <- read_table(file_name, col_names = FALSE) |>
    mutate(Sample_Name = read_table(paste0(file_prefix, "fam"), col_names = FALSE) |> pull(X2)) |> 
    left_join(metadata |> select("FlowerHead", "Sample_Group", "Sample_Name", "Google_Latitude"), by = "Sample_Name")
  #Swap DNA results for Boltonia_126 & 127
  cols_to_swap <- paste0("X", 1:i)
  row_126 <- which(qmat$Sample_Name == "Boltonia_126")
  row_127 <- which(qmat$Sample_Name == "Boltonia_127")
  tmp <- qmat[row_126, cols_to_swap]
  qmat[row_126, cols_to_swap] <- qmat[row_127, cols_to_swap]
  qmat[row_127, cols_to_swap] <- tmp
  qmat <- qmat |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "Ancestry",
      values_to = "Proportion"
    )
  
  #rearranges by Flowerhead
  qmat <- qmat |> 
    mutate(Sample_Label = paste0(Sample_Name, "_(", FlowerHead, ")"))
  
  qmat <- qmat |>
    mutate(Sample_Label = paste0(Sample_Name, "_(", FlowerHead, ")")) |>
    arrange(FlowerHead, Sample_Name) |>
    mutate(Sample_Label = factor(Sample_Label, levels = unique(Sample_Label)))
  
  
  custom_order <- c("saint_clair", "madison", "alton", "jersey", "scott", "morgan", "cass", "schuyler", "frederick", "fulton2", "fulton1", "fulton3", "tazewell", "peoria", "woodford", "marshal", "hennepin")
  qmat$Sample_Group <- factor(qmat$Sample_Group, levels = custom_order)
  
  #creates admixture plot with individual labels
  p <- ggplot(qmat, aes(x = Sample_Label, y = Proportion, fill = Ancestry)) +
    geom_bar(stat = "identity", width = 1) +
    theme_bw() +
    ggh4x::facet_nested(. ~ Sample_Group, scales = "free_x", space = "free_x") +
  labs(
      title = paste("ADMIXTURE Plot (K =", i, ")"),
      x = "Individuals",
      y = "Ancestry Proportion"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      axis.ticks.x.top = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      plot.background = element_rect(fill = "white"),
      legend.position = "none"
    ) +
    scale_fill_brewer(palette = "Set3")
  
  p
  
  ggsave(save_name1, width = 45, height = 6, dpi = 600)
  
  #creates admixture plot without individual labels
  p <- ggplot(qmat, aes(x = Sample_Label, y = Proportion, fill = Ancestry)) +
    geom_bar(stat = "identity", width = 1) +
    theme_bw() +
    ggh4x::facet_nested(. ~ Sample_Group, scales = "free_x", space = "free_x") +
  labs(
      title = paste("ADMIXTURE Plot (K =", i, ")"),
      x = "Individuals",
      y = "Ancestry Proportion"
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      plot.background = element_rect(fill = "white"),
      legend.position = "none"
    ) +
    scale_fill_brewer(palette = "Set3")
  
  p
  
  ggsave(save_name2, width = 18, height = 6, dpi = 600)
}


