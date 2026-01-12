library(readxl)
library(writexl)
library(tidyverse)
library(ggrepel)
library(ggpmisc)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_metadata <- Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx", na = c("NA", "", "NA (NA)"))%>%
  filter(!is.na(Pop_Index))%>%
  mutate(Pop_Index = factor(Pop_Index, levels = str_sort(unique(Pop_Index))))%>%
  arrange(Pop_Index)%>%
  mutate(Pop_Name = factor(Pop_Name, levels = unique(Pop_Name)))%>%
  left_join(read_csv("./data/Boltonia_buf_climate_data_20250421.csv")[,c(1,36)], by = "Sample_Name")


Boltonia_Cass <- Boltonia_metadata %>%
  filter(Pop_Index == "Pop_07")

p <- ggplot(data = Boltonia_Cass, aes(x = Sample_Species, y = FlowerDays.2025))+
  geom_point(position = position_jitter(width = 0.1))+
  geom_boxplot(width = 0.5, fill = "#FFFFFF50", outlier.shape = NA)+
  theme_bw()+
  scale_x_discrete("Beardstown (1997)")+
  scale_y_continuous("Days until first flower in 2025")

p

ggsave("B_asteroides_flowering_compare.png", width = 4, height = 4)


SIFT_total <- read_csv("./data/SIFT_result/SIFT_total_20251013.csv")[,c(1:4, 29)] %>%
  pivot_wider(names_from = "Load_type", values_from = 2:4)

ROH_HET <- read_csv("./data/ROH_HET/ROH_data_40_HET.csv")[1:3]
  

Boltonia_metadata_decurrens <- Boltonia_metadata %>%
  filter(Sample_Species == "B. decurrens")%>%
  left_join(SIFT_total, by = "Sample_Name")%>%
  left_join(ROH_HET, by = "Sample_Name")


str_sort(unique(Boltonia_metadata$Pop_Index))

p3 <- ggplot()+
  geom_violin(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = FlowerDays.2025,fill = Pop), color = NA, show.legend = FALSE)+
  geom_point(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = FlowerDays.2025), size = 0.2)+
  stat_summary(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = FlowerDays.2025), geom = "line", fun = "median", group = 1)+
  geom_boxplot(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = FlowerDays.2025), width = 0.2, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2)+
  scale_x_continuous("Days until first flower in 2025")+
  scale_y_discrete("")+
  theme_bw()+
  theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

p3

Boltonia_metadata_decurrens_t <- Boltonia_metadata_decurrens %>%
  filter(MaternalLine != "2011-2644-1")%>%
  group_by(Pop_Index, wc2.1_30s_bio_7)%>%
  summarise(FlowerDays.2025 = median(FlowerDays.2025, na.rm = TRUE), n = n())%>%
  mutate(Pop_Label = str_remove(Pop_Index, "Pop_"))


p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = wc2.1_30s_bio_7, y = FlowerDays.2025))+
  geom_point(color = "gray80", size = 0.5)+
  # geom_boxplot(data = Boltonia_metadata_decurrens, aes(group = wc2.1_30s_bio_7),width = 0.05, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2, position = position_identity())+
  geom_line(data = Boltonia_metadata_decurrens_t, group = 1, color = "red", alpha = 0.75)+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  geom_text(data = Boltonia_metadata_decurrens_t, aes(label = str_remove(Pop_Index, "Pop_")), size = 3,
            position = position_nudge(y = c(5,-5,-5,-5, 5,
                                            -5,-5,-5,5,10,
                                            -5,-10,-5,10,10,
                                            -5,-5)))+
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_y_continuous(expand = c(0.1,0.1,0.3,0.1))+
  labs(x = "BIO7 Temperature Annual Range (°C)", y = "Days of first flower 2025")+
  theme_bw()


p

ggsave("./figures/phenotypes/FlowerDays.2025_bio7_small.png", width = 4, height = 4, dpi = 600)

##########################
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = wc2.1_30s_bio_7, y = FlowerDays.2024))+
  geom_point(color = "gray80", size = 0.5)+
  geom_boxplot(data = Boltonia_metadata_decurrens, aes(group = wc2.1_30s_bio_7),width = 0.05, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2, position = position_identity())+
  stat_summary(data = Boltonia_metadata_decurrens, geom = "line", fun = "median", group = 1, color = "red")+
  geom_text(data = Boltonia_metadata_decurrens%>%
              group_by(Pop_Index) %>%
              filter(FlowerDays.2024 == min(FlowerDays.2024, na.rm = TRUE)), aes(label = str_remove(Pop_Index, "Pop_")),
            position = position_nudge(y = -10))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_y_continuous(expand = c(0.1,0.1,0.3,0.1))+
  labs(x = "BIO7 Temperature Annual Range (°C)")+
  theme_bw()

p

ggsave("./figures/phenotypes/FlowerDays.2024_bio7.png", width = 10, height = 3.5, dpi = 600)

##########################
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = wc2.1_30s_bio_7, y = Stem.Length.2024))+
  geom_point(color = "gray80", size = 0.5)+
  geom_boxplot(data = Boltonia_metadata_decurrens, aes(group = wc2.1_30s_bio_7),width = 0.05, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2, position = position_identity())+
  stat_summary(data = Boltonia_metadata_decurrens, geom = "line", fun = "median", group = 1, color = "red")+
  geom_text(data = Boltonia_metadata_decurrens%>%
              group_by(Pop_Index) %>%
              filter(Stem.Length.2024 == min(Stem.Length.2024, na.rm = TRUE)), aes(label = str_remove(Pop_Index, "Pop_")),
            position = position_nudge(y = -10))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_y_continuous(expand = c(0.1,0.1,0.3,0.1))+
  labs(x = "BIO7 Temperature Annual Range (°C)")+
  theme_bw()

p

ggsave("./figures/phenotypes/Stem.Length.2024_bio7.png", width = 10, height = 3.5, dpi = 600)

##########################
##########################
Boltonia_metadata_decurrens_t <- Boltonia_metadata_decurrens %>%
  filter(MaternalLine != "2011-2644-1")%>%
  group_by(Pop_Index, wc2.1_30s_bio_7)%>%
  summarise(Stem.Length.2025 = median(Stem.Length.2025, na.rm = TRUE), n = n())%>%
  mutate(Pop_Label = str_remove(Pop_Index, "Pop_"))


p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = wc2.1_30s_bio_7, y = Stem.Length.2025))+
  geom_point(color = "gray80", size = 0.5)+
  # geom_boxplot(data = Boltonia_metadata_decurrens, aes(group = wc2.1_30s_bio_7),width = 0.05, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2, position = position_identity())+
  stat_summary(data = Boltonia_metadata_decurrens, geom = "line", fun = "median", group = 1, color = "red")+
  geom_text(data = Boltonia_metadata_decurrens_t, aes(label = str_remove(Pop_Index, "Pop_")), size = 3,
            position = position_nudge(y = c(5,-5,-5,5, 5,
                                            -5,-5,-5,5,10,
                                            -5,-15,-5,10,10,
                                            -10,-5)))+
  stat_poly_line(formula = y ~ x, se = FALSE, color = "orange") +
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_y_continuous("Stem length 2025", expand = c(0.1,0.1,0.3,0.1))+
  labs(x = "BIO7 Temperature Annual Range (°C)")+
  theme_bw()

p

ggsave("./figures/phenotypes/Stem.Length.2025_bio7_small.png", width = 4, height = 4, dpi = 600)


##########################
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = wc2.1_30s_bio_7, y = Num.Stems.2025))+
  geom_point(color = "gray80", size = 0.5)+
  geom_boxplot(data = Boltonia_metadata_decurrens, aes(group = wc2.1_30s_bio_7),width = 0.05, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2, position = position_identity())+
  stat_summary(data = Boltonia_metadata_decurrens, geom = "line", fun = "median", group = 1, color = "red")+
  geom_text(data = Boltonia_metadata_decurrens%>%
              group_by(Pop_Index) %>%
              filter(Num.Stems.2025 == min(Num.Stems.2025, na.rm = TRUE)), aes(label = str_remove(Pop_Index, "Pop_")),
            position = position_nudge(y = -10))+
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_y_continuous(expand = c(0.1,0.1,0.3,0.1))+
  labs(x = "BIO7 Temperature Annual Range (°C)")+
  theme_bw()

p

ggsave("./figures/phenotypes/Num.Stems.2025_bio7.png", width = 10, height = 3.5, dpi = 600)
##

Boltonia_metadata_decurrens_s <- Boltonia_metadata_decurrens %>%
  group_by(Pop, Pop_Index, Pop_Name)%>%
  summarize(is.flower.2024 = sum(!is.na(FlowerDays.2024)), is.flower.2025 = sum(!is.na(FlowerDays.2025)))


p1 <- ggplot(data = Boltonia_metadata_decurrens_s, aes(y = Pop_Name, x = is.flower.2024/is.flower.2025))+
  geom_col(aes(fill = Pop), width = 0.5)+
  geom_line(group =1)+
  scale_x_continuous("Flower ratio in 2024", expand = c(0, 0), limits = c(0, 1))+
  scale_y_discrete("")+
  theme_bw()+
  theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), plot.margin = margin(0.1, 0.1, 0.1, 0, unit = "in"))

p1

p2 <- ggplot(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = FlowerDays.2024))+
  geom_violin(aes(fill = Pop), color = NA, show.legend = FALSE)+
  geom_point(size = 0.2)+
  stat_summary(geom = "line", fun = "median", group = 1)+
  geom_boxplot(width = 0.2, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2)+
  scale_x_continuous("Days of first flower 2024")+
  scale_y_discrete("")+
  theme_bw()+
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0.1, 0.1, 0.1, 0, unit = "in"))

p2



p3 <- ggplot(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = FlowerDays.2025))+
  geom_violin(aes(fill = Pop), color = NA, show.legend = FALSE)+
  geom_point(size = 0.2)+
  stat_summary(geom = "line", fun = "median", group = 1)+
  geom_boxplot(width = 0.2, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2)+
  scale_x_continuous("Days of first flower 2025")+
  scale_y_discrete("")+
  theme_bw()+
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0.1, 0.1, 0.1, 0, unit = "in"))

p3

p4 <- ggplot(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = Stem.Length.2024))+
  geom_violin(aes(fill = Pop), color = NA, show.legend = FALSE)+
  geom_point(size = 0.2)+
  stat_summary(geom = "line", fun = "median", group = 1)+
  geom_boxplot(width = 0.2, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2)+
  scale_x_continuous("Stem length 2024")+
  scale_y_discrete("")+
  theme_bw()+
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0.1, 0.1, 0.1, 0, unit = "in"))

p4

p5 <- ggplot(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = Stem.Length.2025))+
  geom_violin(aes(fill = Pop), color = NA, show.legend = FALSE)+
  geom_point(size = 0.2)+
  stat_summary(geom = "line", fun = "median", group = 1)+
  geom_boxplot(width = 0.2, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2)+
  scale_x_continuous("Stem length 2025")+
  scale_y_discrete("")+
  theme_bw()+
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0.1, 0.1, 0.1, 0, unit = "in"))

p5


p6 <- ggplot(data = Boltonia_metadata_decurrens, aes(y = Pop_Name, x = Num.Stems.2025))+
  geom_violin(aes(fill = Pop), color = NA, show.legend = FALSE)+
  geom_point(size = 0.2)+
  stat_summary(geom = "line", fun = "median", group = 1)+
  geom_boxplot(width = 0.2, fill = NA, outlier.shape = NA, median.color = "red", linewidth = 0.2)+
  scale_x_continuous("Number of stems 2025")+
  scale_y_discrete("")+
  theme_bw()+
  theme(legend.position = "none", panel.grid.major.x = element_blank(), axis.title.y = element_blank(),
        panel.grid.minor.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0.1, 0.1, 0.1, 0, unit = "in"))
p6


p_comb <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, nrow = 1, rel_widths = c(5, 3, 3, 3, 3, 3))

p_comb


ggsave("./figures/phenotypes/total_flowerday_stemlength.png", width = 12, height = 6, dpi = 600)


p_comb <- cowplot::plot_grid(p1, p3, p5, nrow = 1, rel_widths = c(4, 3, 3))

p_comb


ggsave("./figures/phenotypes/Second_flowerday_stemlength.png", width = 10, height = 6, dpi = 600)


library(ggpmisc)


n_use <- sum(complete.cases(Boltonia_metadata_decurrens$Stem.Length.2025, Boltonia_metadata_decurrens$FlowerDays.2025))
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Stem.Length.2025, y = FlowerDays.2025))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste0("n = ", n_use),
           hjust = 1.05, vjust = -0.6, size = 3.5) +
  scale_x_continuous("Stem length 2025")+
  scale_y_continuous("Days of first flower 2025", expand = c(0.1, 0, 0.3, 0))+
  theme_bw()
p
ggsave("./figures/phenotypes/Stem.Length.2025_Flower.2025.png", width = 4, height = 4, dpi = 600)

###########

n_use <- sum(complete.cases(Boltonia_metadata_decurrens$Stem.Length.2025, Boltonia_metadata_decurrens$Num.Stems.2025))
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Stem.Length.2025, y = Num.Stems.2025))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste0("n = ", n_use),
           hjust = 1.05, vjust = -0.6, size = 3.5) +
  scale_x_continuous("Stem length 2025")+
  scale_y_continuous("Number of stems 2025", expand = c(0.1, 0, 0.3, 0))+
  theme_bw()
p
ggsave("./figures/phenotypes/Stem.Length.2025_Num.Stems.2025.png", width = 4, height = 4, dpi = 600)

###############
n_use <- sum(complete.cases(Boltonia_metadata_decurrens$Num.Stems.2025, Boltonia_metadata_decurrens$FlowerDays.2025))
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Num.Stems.2025, y = FlowerDays.2025))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste0("n = ", n_use),
           hjust = 1.05, vjust = -0.6, size = 3.5) +
  scale_x_continuous("Number of stems 2025")+
  scale_y_continuous("Days of first flower 2025", expand = c(0.1, 0, 0.3, 0))+
  theme_bw()
p
ggsave("./figures/phenotypes/Num.Stems.2025_Flower.2025.png", width = 4, height = 4, dpi = 600)



n_use <- sum(complete.cases(Boltonia_metadata_decurrens$Stem.Length.2024, Boltonia_metadata_decurrens$FlowerDays.2024))
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Stem.Length.2024, y = FlowerDays.2024))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste0("n = ", n_use),
           hjust = 1.05, vjust = -0.6, size = 3.5) +
  scale_x_continuous("Stem length 2024")+
  scale_y_continuous("Days of first flower 2024", expand = c(0.1, 0, 0.3, 0))+
  theme_bw()
p

ggsave("./figures/phenotypes/Stem.Length.2024_Flower.2024.png", width = 4, height = 4, dpi = 600)


n_use <- sum(complete.cases(Boltonia_metadata_decurrens$FlowerDays.2024, Boltonia_metadata_decurrens$FlowerDays.2024))
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = FlowerDays.2024, y = FlowerDays.2025))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste0("n = ", n_use),
           hjust = 1.05, vjust = -0.6, size = 3.5) +
  scale_x_continuous("Days of first flower 2024")+
  scale_y_continuous("Days of first flower 2025", expand = c(0.1, 0, 0.3, 0))+
  theme_bw()
p

ggsave("./figures/phenotypes/Flower.2024_Flower.2025.png", width = 4, height = 4, dpi = 600)

n_use <- sum(complete.cases(Boltonia_metadata_decurrens$Stem.Length.2024, Boltonia_metadata_decurrens$Stem.Length.2025))
p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Stem.Length.2024, y = Stem.Length.2025))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  annotate("text",
           x = Inf, y = -Inf,
           label = paste0("n = ", n_use),
           hjust = 1.05, vjust = -0.6, size = 3.5) +
  scale_x_continuous("Stem length 2024")+
  scale_y_continuous("Stem length 2025", expand = c(0, 0, 0.2, 0))+
  theme_bw()
p

ggsave("./figures/phenotypes/Stem.Length.2024_Stem.Length.2025.png", width = 4, height = 4, dpi = 600)


Boltonia_pheno_2024 <- read_csv("./data/Boltonia_merged_data_tidy_20251010.csv")

unique(Boltonia_pheno_2024$num_traits)
Boltonia_numFlwrB_2024 <- Boltonia_pheno_2024 %>%
  filter(num_traits == "numFlwrB") %>%
  mutate(Days = as.numeric(Date - as.Date("2024-02-29")))


p1 <- ggplot(data = Boltonia_numFlwrB_2024, aes(x = Days, y = num_values))+
  geom_boxplot(aes(group = Days), outlier.size = 0.1, median.color = "red")+
  stat_summary(geom = "line", fun = "median", group = 1, color = "red")+
  scale_x_continuous(name = "Days after planting", limits = c(60, 210))+
  scale_y_continuous(name = "Tight bud")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

p1

Boltonia_numRayF_2024 <- Boltonia_pheno_2024 %>%
  filter(num_traits == "numRayF") %>%
  mutate(Days = as.numeric(Date - as.Date("2024-02-29")))


p2 <- ggplot(data = Boltonia_numRayF_2024, aes(x = Days, y = num_values))+
  geom_boxplot(aes(group = Days), outlier.size = 0.1, median.color = "red")+
  stat_summary(geom = "line", fun = "median", group = 1, color = "red")+
  scale_x_continuous(name = "Days after planting", limits = c(60, 210))+
  scale_y_continuous(name = "Ray florets emerged")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

p2


Boltonia_numDiscF_2024 <- Boltonia_pheno_2024 %>%
  filter(num_traits == "numDiscF")%>%
  mutate(Days = as.numeric(Date - as.Date("2024-02-29")))


p3 <- ggplot(data = Boltonia_numDiscF_2024, aes(x = Days, y = num_values))+
  geom_boxplot(aes(group = Days), outlier.size = 0.1, median.color = "red")+
  stat_summary(geom = "line", fun = "median", group = 1, color = "red")+
  scale_x_continuous(name = "Days after planting", limits = c(60, 210))+
  scale_y_continuous(name = "Full anthesis")+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

p3

Boltonia_numDeadF_2024 <- Boltonia_pheno_2024 %>%
  filter(num_traits == "numDeadF")%>%
  mutate(Days = as.numeric(Date - as.Date("2024-02-29")))


p4 <- ggplot(data = Boltonia_numDeadF_2024, aes(x = Days, y = num_values))+
  geom_boxplot(aes(group = Days), outlier.size = 0.1, median.color = "red")+
  stat_summary(geom = "line", fun = "median", group = 1, color = "red")+
  scale_x_continuous(name = "Days after planting", limits = c(60, 210))+
  scale_y_continuous(name = "Senesced")+
  theme_bw()

p4

p_comb <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 1, rel_heights = c(1,1,1,1.15))

p_comb

ggsave("./figures/phenotypes/flower_date_2024.png", width = 6, height = 8, dpi = 600)


Boltonia_numRayF_2024_t <- Boltonia_pheno_2024 %>%
  filter(num_traits == "numRayF") %>%
  group_by(Sample_Name) %>%
  summarize(Total_Flower.2024 = sum(num_values, na.rm = TRUE))%>%
  left_join(Boltonia_metadata_decurrens, by = "Sample_Name")


p <- ggplot(data = Boltonia_numRayF_2024_t, aes(x = FlowerDays.2024, y = Total_Flower.2024))+
  geom_rect(xmin=100, xmax = 115, ymin=-Inf, ymax=Inf, fill = "#FFBF00")+
  geom_point(size = 0.5)+
  stat_smooth(color = "red")+
  labs(x = "Days of first flower 2024", y = "Total inflorescence 2024")+
  theme_bw()

p

ggsave("./figures/phenotypes/tradeoff_flowerday_2024.png", width = 4, height = 4, dpi = 600)

p <- ggplot(data = Boltonia_numRayF_2024_t, aes(x = Stem.Length.2024, y = Total_Flower.2024))+
  geom_rect(xmin=52, xmax=65, ymin=-Inf, ymax=Inf, fill = "#FFBF00")+
  geom_point(size = 0.5)+
  stat_smooth(color = "red")+
  labs(x = "Stem length 2024", y = "Total inflorescence 2024")+
  theme_bw()

p

ggsave("./figures/phenotypes/tradeoff_stemlength_2024.png", width = 4, height = 4, dpi = 600)


###############

p <- ggplot(data = filter(Boltonia_numRayF_2024_t, Total_Flower.2024 > 0), aes(x = `Pd_Total Load`/(`Pn_Total Load` + `Ps_Total Load`), y = Total_Flower.2024))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()

p


p <- ggplot(data = filter(Boltonia_numRayF_2024_t, Total_Flower.2024 > 0), aes(x = `Pd_Total Load`/(`Pn_Total Load` + `Ps_Total Load`), y = Total_Flower.2024))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()

p

p <- ggplot(data = filter(Boltonia_numRayF_2024_t, Total_Flower.2024 > 0), aes(x = `Pd_Heterozygous Load`/(`Pn_Heterozygous Load` + `Ps_Heterozygous Load`), y = Total_Flower.2024))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()

p

p <- ggplot(data = filter(Boltonia_numRayF_2024_t, Total_Flower.2024 > 0), aes(x = `Pd_Homozygous Load`/(`Pn_Homozygous Load` + `Ps_Homozygous Load`), y = Total_Flower.2024))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()

p


p <- ggplot(data = filter(Boltonia_numRayF_2024_t, Total_Flower.2024 > 0), aes(x = ROH_frac, y = Total_Flower.2024))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()

p

p <- ggplot(data = filter(Boltonia_numRayF_2024_t, Total_Flower.2024 > 0), aes(x = Heterozygosity, y = Total_Flower.2024))+
  geom_point(size = 0.5)+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()

p



################################################
# Genetic Load
colnames(Boltonia_metadata_decurrens)

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Total Load`/(`Pn_Total Load` + `Ps_Total Load`), y = Stem.Length.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p


p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Heterozygous Load`/(`Pn_Heterozygous Load` + `Ps_Heterozygous Load`), y = Stem.Length.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Homozygous Load`/(`Pn_Homozygous Load` + `Ps_Homozygous Load`), y = Stem.Length.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p


#########

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Total Load`/(`Pn_Total Load` + `Ps_Total Load`), y = Stem.Length.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Heterozygous Load`/(`Pn_Heterozygous Load` + `Ps_Heterozygous Load`), y = Stem.Length.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Homozygous Load`/(`Pn_Homozygous Load` + `Ps_Homozygous Load`), y = Stem.Length.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

#########

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Total Load`/(`Pn_Total Load` + `Ps_Total Load`), y = FlowerDays.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Heterozygous Load`/(`Pn_Heterozygous Load` + `Ps_Heterozygous Load`), y = FlowerDays.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Homozygous Load`/(`Pn_Homozygous Load` + `Ps_Homozygous Load`), y = FlowerDays.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p
##########

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Total Load`/(`Pn_Total Load` + `Ps_Total Load`), y = FlowerDays.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Heterozygous Load`/(`Pn_Heterozygous Load` + `Ps_Heterozygous Load`), y = FlowerDays.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Homozygous Load`/(`Pn_Homozygous Load` + `Ps_Homozygous Load`), y = FlowerDays.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

#############ROH HET

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = ROH_frac, y = FlowerDays.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = ROH_frac, y = FlowerDays.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()
p


p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = ROH_frac, y = Stem.Length.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = ROH_frac, y = Stem.Length.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()
p

######


p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Heterozygosity, y = FlowerDays.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Heterozygosity, y = FlowerDays.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()
p


p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Heterozygosity, y = Stem.Length.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = Heterozygosity, y = Stem.Length.2024))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  theme_bw()
p

##########


p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Total Load`/(`Pn_Total Load` + `Ps_Total Load`), y = Num.Stems.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Heterozygous Load`/(`Pn_Heterozygous Load` + `Ps_Heterozygous Load`), y = Num.Stems.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = `Pd_Homozygous Load`/(`Pn_Homozygous Load` + `Ps_Homozygous Load`), y = Num.Stems.2025))+
  geom_point()+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red") +
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous(expression(load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()
p

#################
colnames(Boltonia_metadata_decurrens)
Boltonia_metadata_decurrens_sum <- Boltonia_metadata_decurrens %>%
  group_by(Pop, Pop_Index)%>%
  summarize_if(is.numeric, .funs = "median", na.rm = TRUE)%>%
  ungroup()%>%
  arrange(Adapted_Latitude)%>%
  mutate(Pop_index = 1:n())%>%
  mutate(Pop_Label = paste(Pop_index, Pop, sep = " - "))%>%
  mutate(shape_number = (Pop_index + 3)%%4 + 21)



p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = ROH_frac, y = `Pd_Total Load`/(`Pn_Total Load` + `Ps_Total Load`)))+
  geom_point(color = "gray85")+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red", show.legend = FALSE) +
  geom_point(data = Boltonia_metadata_decurrens_sum, aes(shape = I(shape_number), fill = Pop),size = 3,show.legend = TRUE)+
  geom_text_repel(data = Boltonia_metadata_decurrens_sum, aes(label = as.character(Pop_index)), size = 3, max.overlaps = 15, min.segment.length = 0.3, force = 1.2)+
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous("Fraction of ROH (>40Kbp)", limits = c(0.06, 0.47))+
  scale_y_continuous(expression(Total~load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()+
  theme(legend.position = "none")
p

ggsave("./figures/phenotypes/ROH_GeneticLoad_Total.png", width = 4, height = 4, dpi = 600)

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = ROH_frac, y = `Pd_Heterozygous Load`/(`Pn_Heterozygous Load` + `Ps_Heterozygous Load`)))+
  geom_point(color = "gray85")+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red", show.legend = FALSE) +
  geom_point(data = Boltonia_metadata_decurrens_sum, aes(shape = I(shape_number), fill = Pop),size = 3,show.legend = TRUE)+
  geom_text_repel(data = Boltonia_metadata_decurrens_sum, aes(label = as.character(Pop_index)), size = 3, max.overlaps = 20, min.segment.length = 0.3, force = 1.2)+
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous("Fraction of ROH (>40Kbp)", limits = c(0.06, 0.47))+
  scale_y_continuous(expression(Heterozygotic~load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()+
  theme(legend.position = "none")
p

ggsave("./figures/phenotypes/ROH_GeneticLoad_Heterozygote.png", width = 4, height = 4, dpi = 600)

p <- ggplot(data = Boltonia_metadata_decurrens, aes(x = ROH_frac, y = `Pd_Homozygous Load`/(`Pn_Homozygous Load` + `Ps_Homozygous Load`)))+
  geom_point(color = "gray85")+
  # Draw the fitted line from a linear model
  stat_poly_line(formula = y ~ x, se = FALSE, color = "red", show.legend = FALSE) +
  geom_point(data = Boltonia_metadata_decurrens_sum, aes(shape = I(shape_number), fill = Pop),size = 3,show.legend = TRUE)+
  geom_text_repel(data = Boltonia_metadata_decurrens_sum, aes(label = as.character(Pop_index)), size = 3, max.overlaps = 20, min.segment.length = 0.3, force = 1.2)+
  # Add equation and R^2 as an annotation
  stat_poly_eq(
    formula = y ~ x,
    use_label("eq", "R2", "p"),
    parse = TRUE,
    label.x = "left",   # left side
    label.y = "top",     # near the top
    size = 3.5
  ) +
  scale_x_continuous("Fraction of ROH (>40Kbp)", limits = c(0.06, 0.47))+
  scale_y_continuous(expression(Homozygotic~load[M]~"="~italic(P[d]/(P[n] + P[s]))))+
  theme_bw()+
  theme(legend.position = "none")

p

ggsave("./figures/phenotypes/ROH_GeneticLoad_Homozygote.png", width = 4, height = 4, dpi = 600)


