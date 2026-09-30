plot_trait_vs_bio7 <- function(trait, y_axis_name, file_output_name, ratio) {
  suppressPackageStartupMessages({
    library(readxl)
    library(tidyverse)
    library(ggpmisc)
  })
  
  # ----- Load + prep metadata -----
  Boltonia_metadata_decurrens <- readxl::read_excel(
    "Boltonia_all_metadata_20260205.xlsx",
    na = c("NA", "", "NA (NA)")
  ) %>%
    filter(!is.na(Pop_Index)) %>%
    mutate(Pop_Index = factor(Pop_Index, levels = stringr::str_sort(unique(Pop_Index)))) %>%
    arrange(Pop_Index) %>%
    mutate(Pop_Name = factor(Pop_Name, levels = unique(Pop_Name))) %>%
    left_join(
      readr::read_csv("./data/Boltonia_buf_climate_data_20250421.csv", show_col_types = FALSE)[, c(1, 36)],
      by = "Sample_Name"
    )%>%
    filter(Sample_Species == "B. decurrens")
  
  # ----- Checks -----
  if (!trait %in% names(Boltonia_metadata_decurrens)) {
    stop(sprintf("trait '%s' not found in Boltonia_metadata_decurrens.", trait))
  }
  if (!("wc2.1_30s_bio_7" %in% names(Boltonia_metadata_decurrens))) {
    stop("Column 'wc2.1_30s_bio_7' not found. Check your climate join / column index.")
  }
  if (!is.numeric(ratio) || length(ratio) != 1 || is.na(ratio)) {
    stop("ratio must be a single numeric value (e.g., 0.1).")
  }
  
  # ----- Summaries for population labels -----
  Boltonia_metadata_decurrens_t <- Boltonia_metadata_decurrens %>%
    filter(MaternalLine != "2011-2644-1") %>%
    group_by(Pop_Index, wc2.1_30s_bio_7) %>%
    summarise(
      trait_median = median(.data[[trait]], na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  # Your original nudge pattern, now scaled by `ratio`
  nudge_vec <- c(
    5,-5,-5,5, 5,
    -5,-5,-5,5,10,
    -5,-10,-5,10,10,
    -10,-5
  ) * ratio
  
  # Recycle if needed
  if (nrow(Boltonia_metadata_decurrens_t) > length(nudge_vec)) {
    nudge_vec <- rep(nudge_vec, length.out = nrow(Boltonia_metadata_decurrens_t))
  } else {
    nudge_vec <- nudge_vec[seq_len(nrow(Boltonia_metadata_decurrens_t))]
  }
  
  # ----- Plot -----
  p <- ggplot(Boltonia_metadata_decurrens, aes(x = wc2.1_30s_bio_7, y = .data[[trait]])) +
    geom_point(color = "gray80", size = 0.5) +
    stat_summary(geom = "line", fun = "median", group = 1, color = "red") +
    geom_text(
      data = Boltonia_metadata_decurrens_t,
      aes(
        x = wc2.1_30s_bio_7,
        y = trait_median,
        label = stringr::str_remove(Pop_Index, "Pop_")
      ),
      size = 3,
      position = position_nudge(y = nudge_vec)
    ) +
    ggpmisc::stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
    ggpmisc::stat_poly_eq(
      formula = y ~ x,
      ggpmisc::use_label("eq", "R2", "p"),
      parse = TRUE,
      label.x = "left",
      label.y = "top",
      size = 3.5
    ) +
    scale_y_continuous(y_axis_name, expand = c(0.1, 0.1, 0.3, 0.1)) +
    labs(x = "BIO7 Temperature Annual Range (°C)") +
    theme_bw()
  
  ggplot2::ggsave(file_output_name, plot = p, width = 4, height = 4, dpi = 600)
  return(p)
}


setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

p1 <- plot_trait_vs_bio7(
  trait = "leafWide",
  y_axis_name = "Leaf width 2024-05-15 (cm)",
  file_output_name = "./figures/phenotypes/leafwidth.20240515_bio7_small.png",
  ratio = 0.15
)
p1

p2 <- plot_trait_vs_bio7(
  trait = "leafLong",
  y_axis_name = "Leaf length 2024-05-15 (cm)",
  file_output_name = "./figures/phenotypes/leafLong.20240515_bio7_small.png",
  ratio = 1
)
p2

p4 <- plot_trait_vs_bio7(
  trait = "Num.Stems.2025",
  y_axis_name = "Number of stem 2025",
  file_output_name = "./figures/phenotypes/Num.Stems.2025_bio7_small.png",
  ratio = 1
)
p4

p3 <- plot_trait_vs_bio7(
  trait = "Stem.Length.2024",
  y_axis_name = "Flowering stem length 2024 (cm)",
  file_output_name = "./figures/phenotypes/Stem.Length.2024_bio7_small.png",
  ratio = 6
)
p3

p5 <- plot_trait_vs_bio7(
  trait = "Stem.Length.2025",
  y_axis_name = "Flowering stem length 2025 (cm)",
  file_output_name = "./figures/phenotypes/Stem.Length.2025_bio7_small.png",
  ratio = 6
)
p5

p6 <- plot_trait_vs_bio7(
  trait = "FlowerDays.2024",
  y_axis_name = "Days of first flower 2024",
  file_output_name = "./figures/phenotypes/FlowerDays.2024_bio7_small.png",
  ratio = 6
)
p6

p7 <- plot_trait_vs_bio7(
  trait = "FlowerDays.2025",
  y_axis_name = "Days of first flower 2025",
  file_output_name = "./figures/phenotypes/FlowerDays.2025_bio7_small.png",
  ratio = 5
)
p7

p8 <- plot_trait_vs_bio7(
  trait = "FlowerDays.total",
  y_axis_name = "Days of first flower total",
  file_output_name = "./figures/phenotypes/FlowerDays.total_bio7_small.png",
  ratio = 5
)
p8

comb_p <- cowplot::plot_grid(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 4, ncol = 2, labels = "AUTO")

ggsave("./figures/phenotypes/all_clines_comb_20260205.png", height = 12, width = 8, dpi = 600, )


