library(tidyverse)
library(ggpmisc)
library(cowplot)

plot_genetic_load <- function(dataset, trait, y_title = NULL) {
  # Usage:
  #   plot_genetic_load(Boltonia_metadata_decurrens_new, Total_Flower.2024)
  #   plot_genetic_load(Boltonia_metadata_decurrens_new, leafLong_19844, y_title = "Leaf length (mm)")
  #
  # Dependencies: ggplot2, ggpmisc, cowplot, rlang
  
  stopifnot(is.data.frame(dataset))
  
  trait_quo  <- rlang::enquo(trait)
  trait_name <- rlang::as_name(trait_quo)
  
  if (!trait_name %in% names(dataset)) {
    stop("`trait` must be a column name in `dataset`. Got: ", trait_name)
  }
  if (!is.numeric(dataset[[trait_name]])) {
    stop("`", trait_name, "` must be numeric.")
  }
  
  required_cols <- c(
    "Pd_Total Load", "Pn_Total Load", "Ps_Total Load",
    "Pd_Heterozygous Load", "Pn_Heterozygous Load", "Ps_Heterozygous Load",
    "Pd_Homozygous Load", "Pn_Homozygous Load", "Ps_Homozygous Load"
  )
  missing_cols <- setdiff(required_cols, colnames(dataset))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in `dataset`: ", paste(missing_cols, collapse = ", "))
  }
  
  trait_vec <- dataset[[trait_name]]
  n_use <- sum(!is.na(trait_vec))
  
  # y-axis title logic
  y_lab <- NULL
  if (is.null(y_title) == FALSE) y_lab <- as.character(y_title)
  
  add_common_layers <- function(p) {
    p +
      ggpmisc::stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
      ggpmisc::stat_poly_eq(
        formula = y ~ x,
        ggpmisc::use_label("eq", "R2", "p"),
        parse = TRUE,
        label.x = "left",
        label.y = "top",
        size = 3.5
      ) +
      ggplot2::annotate(
        "text",
        x = Inf, y = -Inf,
        label = paste0("n = ", n_use),
        hjust = 1.05, vjust = -0.6, size = 3.5
      ) +
      ggplot2::theme_bw()
  }
  
  p1 <- ggplot2::ggplot(
    data = dataset,
    ggplot2::aes(
      x = `Pd_Total Load` / (`Pn_Total Load` + `Ps_Total Load`),
      y = !!trait_quo
    )
  ) +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::scale_x_continuous(expression(Total~load[M]~"="~italic(P[d]/(P[n] + P[s])))) +
    ggplot2::labs(y = y_lab)
  p1 <- add_common_layers(p1)
  
  p2 <- ggplot2::ggplot(
    data = dataset,
    ggplot2::aes(
      x = `Pd_Heterozygous Load` / (`Pn_Heterozygous Load` + `Ps_Heterozygous Load`),
      y = !!trait_quo
    )
  ) +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::scale_x_continuous(expression(Heterozygotic~load[M]~"="~italic(P[d]/(P[n] + P[s])))) +
    ggplot2::labs(y = y_lab)
  p2 <- add_common_layers(p2)
  
  p3 <- ggplot2::ggplot(
    data = dataset,
    ggplot2::aes(
      x = `Pd_Homozygous Load` / (`Pn_Homozygous Load` + `Ps_Homozygous Load`),
      y = !!trait_quo
    )
  ) +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::scale_x_continuous(expression(Homozygotic~load[M]~"="~italic(P[d]/(P[n] + P[s])))) +
    ggplot2::labs(y = y_lab)
  p3 <- add_common_layers(p3)
  
  p_comb <- cowplot::plot_grid(p1, p2, p3, nrow = 1)
  
  outdir <- "./figures/Genetic_load"
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  ggplot2::ggsave(
    filename = file.path(outdir, paste0(trait_name, ".png")),
    plot = p_comb,
    width = 10, height = 3.5, dpi = 600
  )
  
  invisible(NULL)
}


colnames(Boltonia_metadata_decurrens_new)
# Example:
plot_genetic_load(Boltonia_metadata_decurrens_new, Total_Flower.2024, y_title = "Number of capitula 2024")

plot_genetic_load(Boltonia_metadata_decurrens_new, Stem.Length.2025, y_title = "Flowering stem length 2025")

plot_genetic_load(Boltonia_metadata_decurrens_new, Num.Stems.2025, y_title = "Number of flowering stem 2025")

plot_genetic_load(Boltonia_metadata_decurrens_new, Stem.Length.2024, y_title = "Flowering stem length 2024")

plot_genetic_load(Boltonia_metadata_decurrens_new, FlowerDays.2024, y_title = "Days of first flower 2024")

plot_genetic_load(Boltonia_metadata_decurrens_new, FlowerDays.2025, y_title = "Days of first flower 2025")

plot_genetic_load(Boltonia_metadata_decurrens_new, FlowerDays.total, y_title = "Days of first flower 2024-2025")

plot_genetic_load(Boltonia_metadata_decurrens_new, leafLong_19844, y_title = paste("Leaf length", as.Date(19844)))

plot_genetic_load(Boltonia_metadata_decurrens_new, leafLong_19851, y_title = paste("Leaf length", as.Date(19851)))

plot_genetic_load(Boltonia_metadata_decurrens_new, leafLong_19858, y_title = paste("Leaf length", as.Date(19858)))

plot_genetic_load(Boltonia_metadata_decurrens_new, leafWide_19844, y_title = paste("Leaf width", as.Date(19844)))

plot_genetic_load(Boltonia_metadata_decurrens_new, leafWide_19851, y_title = paste("Leaf width", as.Date(19851)))

plot_genetic_load(Boltonia_metadata_decurrens_new, leafWide_19858, y_title = paste("Leaf width", as.Date(19858)))



