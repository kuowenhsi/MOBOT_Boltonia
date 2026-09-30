library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(ggpmisc)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

## 1. read data
pi_df <- read_csv("./data/GWAS/Boltonia_decurrens_imputed_snpeff_annotated_pi.txt.population_site_pi.csv")
markers <- read_tsv("./data/GWAS/GLM_sig_markers_all.tsv", comment = "")

## if the first column is named #CHROM, rename it
markers <- markers %>%
  rename(CHROM = `#CHROM`)

## make sure join columns have the same type
pi_df <- pi_df %>%
  mutate(
    CHROM = as.character(CHROM),
    POSITION = as.numeric(POSITION)
  )

markers <- markers %>%
  mutate(
    CHROM = as.character(CHROM),
    POS = as.numeric(POS)
  )

## 2. merge the two datasets
pi_gwas <- pi_df %>%
  left_join(markers, by = c("CHROM" = "CHROM", "POSITION" = "POS"))

## check result
print(pi_gwas)
## columns should now include:
## Pop, CHROM, POSITION, PI, REF, ALT, GWAS_P, GWAS_BETA, GWAS_SE, GWAS_TRAIT

## optional: save merged table
write_csv(pi_gwas, "population_site_pi_with_GWAS_trait.csv")

##################
library(data.table)
set.seed(123)

pi_dir <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/GWAS/Boltonia_decurrens_imputed_neutral_pi.txt_site_pi_work/vcftools_outputs"

pi_files <- list.files(
  path = pi_dir,
  pattern = "\\.sites\\.pi$",
  full.names = TRUE
)

## use the first file as the reference for shared site sampling
ref_file <- pi_files[1]

sampled_sites <- fread(ref_file, select = c("CHROM", "POS")) %>%
  as_tibble() %>%
  mutate(
    CHROM = as.character(CHROM),
    POSITION = as.numeric(POS)
  ) %>%
  select(CHROM, POSITION) %>%
  slice_sample(n = min(10000, nrow(.)))

## function to read one file and keep only the sampled shared sites
read_selected_sites <- function(f, sampled_sites_tbl) {
  pop_name <- basename(f) %>%
    str_remove("\\.sites\\.pi$")
  
  fread(f) %>%
    as_tibble() %>%
    mutate(
      CHROM = as.character(CHROM),
      POSITION = as.numeric(POS),
      PI = as.numeric(PI),
      Pop = pop_name
    ) %>%
    select(Pop, CHROM, POSITION, PI) %>%
    inner_join(sampled_sites_tbl, by = c("CHROM", "POSITION"))
}

pi_downsampled_shared <- map_dfr(
  pi_files,
  read_selected_sites,
  sampled_sites_tbl = sampled_sites
)

## check
pi_downsampled_shared %>% count(Pop)

## save
write_csv(pi_downsampled_shared, "all_populations_shared_1000_sites_pi.csv")
write_csv(sampled_sites, "shared_1000_sites_used.csv")


p <- ggplot(data = pi_downsampled_shared, aes(x = Pop, y = PI))+
  geom_violin(fill = "gray85", color = NA)+
  # geom_boxplot(width = 0.1, outlier.shape = NA, fill = "gray99", median.color = "red")+
  # geom_point(color = "gray30")+
  stat_summary(geom = "line", fun = "median", group = 1,color = "red", linewidth = 0.6)+
  theme_bw()
p


pi_downsampled_shared_s <- pi_downsampled_shared %>%
  group_by(Pop) %>%
  summarise(PI_neutral_median = median(PI),PI_neutral_25 = quantile(PI, 0.25), PI_neutral_75 = quantile(PI, 0.75),
            PI_neutral_mean = mean(PI), .groups = "drop")

pi_downsampled_shared_s

##################

glm_path <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Pi_TajD/Pop_Index_nonoverlapping/"

Pi_input <- sort(list.files("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Pi_TajD/Pop_Index_nonoverlapping", pattern = ".windowed.pi" ))

Pi_window_data <- bind_rows(lapply(paste0(glm_path, Pi_input), fread), .id = "Pop_Index") %>%
  rename(chr = CHROM)%>%
  mutate(POSITION = (BIN_START + BIN_END)/2)%>%
  group_by(Pop_Index)%>%
  summarize(PI_window_median = median(PI), PI_window_25 = quantile(PI, 0.25), PI_window_75 = quantile(PI, 0.75), PI_window_mean = mean(PI), PI_window_sum = sum(PI),.groups = "drop")%>%
  mutate(Pop_Index = as.integer(Pop_Index))%>%
  mutate(Pop_Name = paste("Pop", str_pad(Pop_Index, width = 2, pad = "0"), sep = "_"))

Pi_window_data
##################
library(tidyverse)
library(robustbase)


Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx")%>%
  group_by(Pop, Pop_Index)%>%
  summarise(n = n(), FlowerDays.2025.sd = sd(FlowerDays.2025, na.rm = TRUE), FlowerDays.2025.qn = Qn(FlowerDays.2025, na.rm = TRUE),
            FlowerDays.2025.mad = mad(FlowerDays.2025, na.rm = TRUE),
            Stem.Length.2025.sd = sd(Stem.Length.2025, na.rm = TRUE),Stem.Length.2025.qn = Qn(Stem.Length.2025, na.rm = TRUE),
            Stem.Length.2025.mad = mad(Stem.Length.2025, na.rm = TRUE),
            Num.Stems.2025.sd = sd(Num.Stems.2025, na.rm = TRUE), Num.Stems.2025.qn = Qn(Num.Stems.2025, na.rm = TRUE),
            Num.Stems.2025.mad = mad(Num.Stems.2025, na.rm = TRUE))%>%
  drop_na()%>%
  ungroup()%>%
  arrange(Pop_Index)


stem_data <-filter(pi_gwas, GWAS_TRAIT == "Stem_Length")

stem_data_s <-filter(pi_gwas, GWAS_TRAIT == "Stem_Length")%>%
  mutate(weight = abs(GWAS_BETA))%>%
  group_by(Pop)%>%
  summarise(
    stem_n_sites = n(),
    stem_weighted_mean_PI = weighted.mean(PI, w = weight, na.rm = TRUE),
    stem_additive_genetic_variance = sum(PI*(GWAS_BETA)^2),
    stem_mean_PI = mean(PI, na.rm = TRUE),
    stem_median_PI = median(PI, na.rm = TRUE),
    .groups = "drop"
  )

functional_PI <- Boltonia_metadata %>%
  left_join(stem_data_s, by = c("Pop_Index" = "Pop"))%>%
  mutate(Pop_Index = as.integer(factor(Pop_Index)))%>%
  arrange(Pop_Index)%>%
  mutate(shape_number = (Pop_Index + 3)%%4 + 21)%>%
  left_join(Pi_window_data, by = "Pop_Index") %>%
  left_join(pi_downsampled_shared_s, by = c("Pop_Name" = "Pop"))

cor.test(functional_PI$Stem.Length.2025.qn, functional_PI$stem_additive_genetic_variance, method = "spearman")
cor.test(functional_PI$Stem.Length.2025.qn, functional_PI$stem_additive_genetic_variance, method = "pearson") 

p <- ggplot(data = functional_PI[-c(1,2),], aes(x = stem_additive_genetic_variance, y = Stem.Length.2025.qn))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  labs(x = "Additive variance of GWAS loci", y = "Variation of stem length")+
  scale_y_continuous(limits = c(12, 38))+
  theme_bw()+
  theme(axis.title = element_text(size = 12))

p

ggsave("./figures/GWAS_diversity/stemLength_functional.png", width = 3.5, height = 3.5, dpi = 600)

p <- ggplot(data = functional_PI, aes(x = stem_additive_genetic_variance, y = Stem.Length.2025.qn))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = stem_additive_genetic_variance, y = Stem.Length.2025.mad))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI[-c(1:2),], aes(x = stem_weighted_mean_PI, y = Stem.Length.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = stem_median_PI, y = Stem.Length.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p


p <- ggplot(data = functional_PI, aes(x = PI_window_median, y = Stem.Length.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI[-c(1,2),], aes(x = PI_window_sum, y = Stem.Length.2025.qn))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  labs(x = "Additive variance of neutral loci", y = "Variation of stem length")+
  scale_y_continuous(limits = c(12, 38))+
  theme_bw()+
  theme(axis.title = element_text(size = 12))

p

ggsave("./figures/GWAS_diversity/stemLength_neutral.png", width = 3.5, height = 3.5, dpi = 600)

p <- ggplot(data = functional_PI, aes(x = PI_neutral_median, y = Stem.Length.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = PI_neutral_mean, y = Stem.Length.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI[,], aes(x = PI_window_mean, y = stem_mean_PI))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()+
  scale_y_continuous(limits = c(0.1, 0.4))

p


unique(pi_gwas$GWAS_TRAIT)
p <- ggplot(data = filter(pi_gwas, GWAS_TRAIT == "Stem_Length"), aes(x = Pop, y = PI))+
  geom_violin(fill = "gray85", color = NA)+
  # geom_boxplot(width = 0.1, outlier.shape = NA, fill = "gray99", median.color = "red")+
  geom_point(aes(size = abs(GWAS_BETA)), color = "gray30")+
  stat_summary(geom = "line", fun = "median", group = 1,color = "red", linewidth = 0.6)+
  geom_line(data = stem_data_s, aes(y = stem_weighted_mean_PI), group = 1, color = "green4")+
  theme_bw()
p

#########

flower_data_s <-filter(pi_gwas, GWAS_TRAIT == "FlowerDays_2025")%>%
  mutate(weight = abs(GWAS_BETA))%>%
  group_by(Pop)%>%
  summarise(
    flower_n_sites = n(),
    flower_weighted_mean_PI = weighted.mean(PI, w = weight, na.rm = TRUE),
    flower_additive_genetic_variance = sum(PI*(GWAS_BETA)^2),
    flower_mean_PI = mean(PI, na.rm = TRUE),
    flower_median_PI = median(PI, na.rm = TRUE),
    .groups = "drop"
  )

functional_PI <- Boltonia_metadata %>%
  left_join(flower_data_s, by = c("Pop_Index" = "Pop"))%>%
  mutate(Pop_Index = as.integer(factor(Pop_Index)))%>%
  arrange(Pop_Index)%>%
  mutate(shape_number = (Pop_Index + 3)%%4 + 21)%>%
  left_join(Pi_window_data, by = "Pop_Index") %>%
  left_join(pi_downsampled_shared_s, by = c("Pop_Name" = "Pop"))


p <- ggplot(data = functional_PI, aes(x = flower_additive_genetic_variance, y = FlowerDays.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = flower_additive_genetic_variance, y = FlowerDays.2025.qn))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  labs(x = "Additive variance of GWAS loci", y = "Variation of flower days")+
  scale_y_continuous(limits = c(12, 40))+
  theme_bw()+
  theme(axis.title = element_text(size = 12))

p

ggsave("./figures/GWAS_diversity/flowerDays_functional.png", width = 3.5, height = 3.5, dpi = 600)

p <- ggplot(data = functional_PI, aes(x = flower_additive_genetic_variance, y = FlowerDays.2025.mad))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = flower_median_PI, y = FlowerDays.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = PI_window_sum, y = FlowerDays.2025.qn))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  labs(x = "Additive variance of neutral loci", y = "Variation of flower days")+
  scale_y_continuous(limits = c(12, 40))+
  theme_bw()+
  theme(axis.title = element_text(size = 12))

p

ggsave("./figures/GWAS_diversity/flowerDays_neutral.png", width = 3.5, height = 3.5, dpi = 600)

p <- ggplot(data = functional_PI, aes(x = PI_neutral_mean, y = FlowerDays.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = PI_window_mean, y = flower_mean_PI))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

unique(pi_gwas$GWAS_TRAIT)
p <- ggplot(data = filter(pi_gwas, GWAS_TRAIT == "FlowerDays_2025"), aes(x = Pop, y = PI))+
  geom_violin(fill = "gray85", color = NA)+
  # geom_boxplot(width = 0.1, outlier.shape = NA, fill = "gray99", median.color = "red")+
  geom_point(aes(size = abs(GWAS_BETA)), color = "gray30")+
  stat_summary(geom = "line", fun = "median", group = 1,color = "red", linewidth = 0.6)+
  geom_line(data = flower_data_s, aes(y = flower_weighted_mean_PI), group = 1, color = "green4")+
  theme_bw()
p

################


num_data_s <-filter(pi_gwas, GWAS_TRAIT == "Num_Stems")%>%
  mutate(weight = abs(GWAS_BETA))%>%
  group_by(Pop)%>%
  summarise(
    num_n_sites = n(),
    num_weighted_mean_PI = weighted.mean(PI, w = weight, na.rm = TRUE),
    num_additive_genetic_variance = sum(PI*(GWAS_BETA)^2),
    num_mean_PI = mean(PI, na.rm = TRUE),
    num_median_PI = median(PI, na.rm = TRUE),
    .groups = "drop"
  )

functional_PI <- Boltonia_metadata %>%
  left_join(num_data_s, by = c("Pop_Index" = "Pop"))%>%
  mutate(Pop_Index = as.integer(factor(Pop_Index)))%>%
  arrange(Pop_Index)%>%
  mutate(shape_number = (Pop_Index + 3)%%4 + 21)%>%
  left_join(Pi_window_data, by = "Pop_Index") %>%
  left_join(pi_downsampled_shared_s, by = c("Pop_Name" = "Pop"))


p <- ggplot(data = functional_PI, aes(x = num_weighted_mean_PI, y = Num.Stems.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI[-c(7,12),], aes(x = num_additive_genetic_variance, y = Num.Stems.2025.qn))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  labs(x = "Additive genetic variance of GWAS signals", y = "Robust scale estimator of stem numbers")+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = num_median_PI, y = Num.Stems.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI[-c(7,12),], aes(x = PI_window_mean, y = Num.Stems.2025.qn))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  labs(x = "Neutral genetic diversity", y = "Robust scale estimator of flower days")+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = PI_neutral_mean, y = Num.Stems.2025.sd))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p

p <- ggplot(data = functional_PI, aes(x = PI_window_mean, y = num_mean_PI))+
  stat_smooth(method = "loess")+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  geom_point(aes(shape = I(shape_number), fill = Pop), show.legend = FALSE, size = 4)+
  ggrepel::geom_text_repel(aes(label = Pop_Index), size = 4, show.legend = FALSE)+
  theme_bw()

p



unique(pi_gwas$GWAS_TRAIT)
p <- ggplot(data = filter(pi_gwas, GWAS_TRAIT == "Num_Stems"), aes(x = Pop, y = PI))+
  geom_violin(fill = "gray85", color = NA)+
  # geom_boxplot(width = 0.1, outlier.shape = NA, fill = "gray99", median.color = "red")+
  geom_point(aes(size = abs(GWAS_BETA)), color = "gray30")+
  stat_summary(geom = "line", fun = "median", group = 1,color = "red", linewidth = 0.6)+
  geom_line(data = num_data_s, aes(y = num_weighted_mean_PI), group = 1, color = "green4")+
  theme_bw()
p


#########

filter(pi_gwas, GWAS_TRAIT == "FlowerDays_total")
