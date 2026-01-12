# Load necessary libraries
library(tidyverse)
library(broom)
library(vegan)
library(FactoMineR)
library(ggrepel)
library(ggforce)
library(sp)
library(sf)
library(ggh4x)

# Set working directory
setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20250815.xlsx", na = c("NA", "", "NA (NA)"))%>%
  mutate(Pop = case_when(Pop == "Cooper Park (1995)" ~ "Cooper Park (2000)", TRUE ~ Pop))%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))

Boltonia_metadata_Pop_Index <- Boltonia_metadata %>%
  group_by(Pop)%>%
  summarize(Adapted_Latitude = mean(Adapted_Latitude), Adapted_Longitude = mean(Adapted_Longitude))%>%
  ungroup()%>%
  drop_na()%>%
  arrange(Adapted_Latitude, Pop)%>%
  mutate(Pop_Index = paste("Pop", str_pad(1:n(), width = 2, pad = "0"), sep = "_"))%>%
  mutate(Pop_Name = paste(1:n(), Pop, sep = "-"))%>%
  select(Pop_Name, Pop, Pop_Index ,Adapted_Latitude, Adapted_Longitude)

# write_csv(Boltonia_metadata_Pop_Index, "./data/Boltonia_metadata_Pop_Index.csv")

Boltonia_metadata <- Boltonia_metadata %>%
  left_join(select(Boltonia_metadata_Pop_Index, 1:3), by = "Pop")%>%
  select(Sample_Name, Pop, Pop_Index, everything())

# Read and process FST data
fst_input <- read_tsv("./data/Fst_by_pop/Boltonia_decurrens_imputed_maf_mildLDpruned_fst.fst.summary", col_names = c("POP1", "POP2", "FST"), skip = 1)%>%
  mutate(L_FST = FST / (1 - FST))

fst_input_sym <- fst_input %>%
  mutate(POP1 = factor(POP1, levels = paste("Pop", str_pad(1:17, 2, pad = "0"), sep = "_"), labels = 1:17))%>%
  mutate(POP2 = factor(POP2, levels = c(paste("Pop", str_pad(1:17, 2, pad = "0"), sep = "_")), labels = 1:17))%>%
  complete(POP1, POP2)%>%
  mutate(FST = ifelse(POP1 == POP2, 0, FST))

# Read and process FST data
del_fst_input <- read_tsv("./data/Fst_by_pop/Boltonia_decurrens_imputed_SIFTpredictions_NONSYNONYMOUS_DELETERIOUS.fst.summary", col_names = c("POP1", "POP2", "FST"), skip = 1)%>%
  mutate(L_FST = FST / (1 - FST))

sum(fst_input$FST - del_fst_input$FST)

del_fst_input_sym <- del_fst_input %>%
  mutate(POP1 = factor(POP1, levels = paste("Pop", str_pad(1:17, 2, pad = "0"), sep = "_"), labels = 1:17))%>%
  mutate(POP2 = factor(POP2, levels = c(paste("Pop", str_pad(1:17, 2, pad = "0"), sep = "_")), labels = 1:17))%>%
  complete(POP1, POP2)%>%
  mutate(FST = ifelse(POP1 == POP2, 0, FST))%>%
  filter(!is.na(FST))

q <- ggplot(fst_input_sym, aes(x = POP1, y = POP2, fill = FST)) +
  geom_tile(aes(fill = FST)) +
  geom_tile(data = del_fst_input_sym, aes(x = POP2, y = POP1, fill = FST))+
  theme_bw(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text.y.right = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right"
  ) +
  scale_x_discrete(name = "Population Index",expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.01)), 
                   name = "Neutral variant sites", 
                   sec.axis = dup_axis(name = "Deleterious nonsynonmous sites")) +
  scale_fill_gradientn(
    colors = rev(RColorBrewer::brewer.pal(9, "RdYlGn")),
    na.value = "black"
  ) +
  labs(fill = "Fst")

q

ggsave("./figures/Fst_by_Heat_20251009.png", width = 5, height = 4)

# Calculate quantiles for FST
quantile(fst_input$FST)

# Get unique population names
pop_names <- unique(c(fst_input$POP1, fst_input$POP2))


# Split FST data by population and process outliers
fst_by_pop_list <- list()
for (i in pop_names) {
  fst_by_pop_list[[i]] <- fst_input[(fst_input$POP1 == i) | (fst_input$POP2 == i), ]
}
fst_by_pop <- bind_rows(fst_by_pop_list, .id = "REF") %>%
  group_by(REF) %>%
  mutate(OUTLIER = FST > (quantile(FST, probs = 0.75) + 1.5 * IQR(FST))) %>%
  mutate(OUTLIER_LABEL = case_when((OUTLIER == TRUE) & (REF == POP1) ~ POP2,
                                   (OUTLIER == TRUE) & (REF != POP1) ~ POP1,
                                   TRUE ~ as.character(NA))) %>%
  arrange(desc(FST), .by_group = TRUE) %>%
  mutate(count_REF = cumsum(OUTLIER)) %>%
  mutate(LABEL_x = case_when((OUTLIER == TRUE) & (count_REF %% 2 == 1) ~ 0.7,
                             (OUTLIER == TRUE) & (count_REF %% 2 == 0) ~ 1.3,
                             TRUE ~ as.numeric(NA)))%>%
  left_join(Boltonia_metadata_Pop_Index, by = c("REF" = "Pop_Index"))

fst_dist_p <- ggplot(data = fst_by_pop, aes(y = REF, x = FST))+
  geom_violin(aes(fill = Pop), alpha = 0.7, color = NA)+
  geom_boxplot(width = 0.1, alpha = 1, outlier.size = 0.5)+
  # geom_point(position = position_jitter(height = 0.2))+
  # geom_text(aes(x = LABEL_x, label = OUTLIER_LABEL), size = 3)+
  scale_y_discrete("")+
  scale_x_continuous("")+
  facet_nested(reorder(Pop_Name, desc(REF)) ~ ., scales = "free_y")+
  theme_bw()+
  theme(legend.position = "none", panel.spacing.y = unit(0,"line"), axis.text.y = element_blank(), panel.grid = element_blank(), axis.ticks.y = element_blank(), panel.border = element_rect(color = "gray80"), strip.background = element_rect(color = "gray80"), axis.ticks.x = element_line(color = "gray80"), strip.text.y.right = element_text(size =10, angle = 0))

fst_dist_p

ggsave("./figures/Fst_by_Pop_20251007.png", width = 5, height = 8)


# Read and process climate data
clim_data <- read_csv("./data/Boltonia_buf_climate_data_20250421.csv")
colnames(clim_data) <- str_remove_all(colnames(clim_data), "current_30arcsec_")

colnames(clim_data)[20:38] <- c(
  "Bio1 - Annual Mean Temperature",
  "Bio10 - Mean Temperature of Warmest Quarter",
  "Bio11 - Mean Temperature of Coldest Quarter",
  "Bio12 - Annual Precipitation",
  "Bio13 - Precipitation of Wettest Month",
  "Bio14 - Precipitation of Driest Month",
  "Bio15 - Precipitation Seasonality",
  "Bio16 - Precipitation of Wettest Quarter",
  "Bio17 - Precipitation of Driest Quarter",
  "Bio18 - Precipitation of Warmest Quarter",
  "Bio19 - Precipitation of Coldest Quarter",
  "Bio2 - Mean Diurnal Range",
  "Bio3 - Isothermality",
  "Bio4 - Temperature Seasonality",
  "Bio5 - Max Temperature of Warmest Month",
  "Bio6 - Min Temperature of Coldest Month",
  "Bio7 - Temperature Annual Range",
  "Bio8 - Mean Temperature of Wettest Quarter",
  "Bio9 - Mean Temperature of Driest Quarter"
)


# Convert to spatial format
clim_input_sf <- st_as_sf(clim_data, coords = c("Adapted_Longitude", "Adapted_Latitude"), crs = 4326)%>%
  left_join(Boltonia_metadata, by = "Sample_Name")%>%
  group_by(Pop, Pop_Index)%>%
  summarize_all(.funs = "mean")

# Calculate geographic distances
geo_dist <- st_distance(clim_input_sf, clim_input_sf)
rownames(geo_dist) <- clim_input_sf$Pop_Index
colnames(geo_dist) <- clim_input_sf$Pop_Index
geo_dist_df <- tidy(as.dist(geo_dist, diag = FALSE, upper = FALSE))
colnames(geo_dist_df) <- c("POP1", "POP2", "geo_dist")

colnames(clim_input_sf)
sapply(clim_input_sf[3:37] %>% st_drop_geometry, class)
# Calculate climate distances
clim_dist <- bind_cols(lapply(clim_input_sf[3:37] %>% st_drop_geometry, function(x) as.numeric(dist(scale(x)))))


river_path <- read_csv("./data/Fst_by_pop/river_path_distance.csv")

# Combine geographic, climate, and FST data
geo_clim_dist <- bind_cols(geo_dist_df, clim_dist) %>%
  rowwise()%>%
  mutate(POP1_ = sort(c(POP1, POP2))[[1]] , POP2_ = sort(c(POP1, POP2))[[2]])%>%
  ungroup()%>%
  dplyr::select(-POP1, -POP2)%>%
  dplyr::select(POP1 = POP1_, POP2 = POP2_, everything())%>%
  left_join(fst_input, by = c("POP1", "POP2"))%>%
  left_join(river_path, by = c("POP1", "POP2"))

length(unique(c(geo_clim_dist$POP1, geo_clim_dist$POP2)))

# # Prepare FST distance matrices
# fst_matrix <- matrix(NA, nrow = 17, ncol = 17)
# fst_matrix[lower.tri(fst_matrix)] <- geo_clim_dist$FST
# fst_dist <- as.dist(fst_matrix, diag = FALSE, upper = FALSE)
# 
# L_fst_matrix <- matrix(NA, nrow = 17, ncol = 17)
# L_fst_matrix[lower.tri(L_fst_matrix)] <- geo_clim_dist$L_FST
# L_fst_dist <- as.dist(L_fst_matrix, diag = FALSE, upper = FALSE)
# 
# river_path_m_matrix <- matrix(NA, nrow = 17, ncol = 17)
# river_path_m_matrix[lower.tri(river_path_m_matrix)] <- geo_clim_dist$river_path_m
# river_path_m_dist <- as.dist(river_path_m_matrix, diag = FALSE, upper = FALSE)
# river_path_m_dist
# 
# # Define functions for Mantel tests
# get_mantel_result <- function(x) {
#   result <- mantel(L_fst_dist, dist(x), permutations = 10e3)
#   return(tibble(statistic = result$statistic, signif = result$signif))
# }
# 
# get_mantel_result_geoDist <- function(x) {
#   result <- mantel(L_fst_dist, geo_dist, permutations = 10e3)
#   return(tibble(statistic = result$statistic, signif = result$signif, env_var = "Euclidean Distance"))
# }
# 
# get_mantel_result_riverDist <- function(x) {
#   result <- mantel(L_fst_dist, river_path_m_dist, permutations = 10e3)
#   return(tibble(statistic = result$statistic, signif = result$signif, env_var = "River Path Distance"))
# }
# 
# get_mantel.partial_result <- function(x) {
#   result <- mantel.partial(L_fst_dist, dist(x), geo_dist, permutations = 10e3, parallel = 8)
#   return(tibble(statistic = result$statistic, signif = result$signif))
# }
# 
# dim(clim_input_sf%>% st_drop_geometry)
# colnames(clim_input_sf%>% st_drop_geometry)
# # Perform Mantel tests and save results
# mantel_results <- bind_rows(lapply(clim_input_sf[4:38] %>% st_drop_geometry, get_mantel_result)) %>%
#   mutate(env_var = colnames(clim_input_sf)[4:38]) %>%
#   bind_rows(get_mantel_result_geoDist())%>%
#   bind_rows(get_mantel_result_riverDist())
# 
# mantel_results_sig <- mantel_results %>%
#   mutate(sig_color = case_when(signif >= 0.05 ~ "p >= 0.05",
#                                signif < 0.05 & signif >= 0.001 ~ "p < 0.05",
#                                signif < 0.001 ~ "p < 0.001"))
# 
# write_excel_csv(mantel_results_sig, "./data/Fst_by_pop/mantel_results_sig_20251008.csv")
mantel_results_sig <- read_csv("./data/mantel_results_sig_20250723.csv")

# # Perform partial Mantel tests and save results
# mantel.partial_results <- bind_rows(lapply(clim_input_sf[3:37]%>% st_drop_geometry, get_mantel.partial_result)) %>%
#   mutate(env_var = colnames(clim_input_sf)[3:37])
# 
# mantel.partial_results_sig <- mantel.partial_results %>%
#   mutate(sig_color = case_when(signif >= 0.05 ~ "p >= 0.05",
#                                signif < 0.05 & signif >= 0.001 ~ "p < 0.05",
#                                signif < 0.001 ~ "p < 0.001"))
# 
# write_excel_csv(mantel.partial_results_sig, "./data/mantel.partial_results_sig_20250723.csv")
mantel.partial_results_sig <- read_csv("./data/mantel.partial_results_sig_20250723.csv")


p <- ggplot(data = mantel_results_sig, aes(y = reorder(env_var, statistic), x = statistic))+
  geom_vline(xintercept = 0, color = "gray85")+
  geom_segment(aes(yend = reorder(env_var, statistic), xend = statistic), x = 0)+
  geom_point(aes(color = sig_color))+
  xlab(expression(Mantel~r["[DE]"]))+
  ylab("")+
  scale_color_manual("", values = c("red", "orange", "gray85"))+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = c(0.7, 0.1))
p

ggsave("./figures/IBD_IBE/geo_env_Mantel_test_result.png", width = 5, height = 8)


p <- ggplot(data = mantel.partial_results_sig, aes(y = reorder(env_var, statistic), x = statistic))+
  geom_vline(xintercept = 0, color = "gray85")+
  geom_segment(aes(yend = reorder(env_var, statistic), xend = statistic), x = 0)+
  geom_point(aes(color = sig_color))+
  xlab(expression(Mantel~r["[DE]"]))+
  ylab("")+
  scale_color_manual("", values = c("red", "orange", "gray85"))+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = c(0.7, 0.1))
p

ggsave("./figures/geo_env_Partial_Mantel_test_result.png", width = 5, height = 8)

0.37619088
0.00379962
# Plot geographic distance vs. FST
p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = geo_dist / 1000)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 0, y = 0.2, label = "Mantel r = 0.27\np = 0.028", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab("Euclidean distance (km)") +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("./figures/IBD_IBE/Isolation_by_EuclideanDistance.png", width = 3, height = 3, dpi = 600)



p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = river_path_m / 1000)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 0, y = 0.2, label = "Mantel r = 0.40\np = 0.0036", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab("River path distance (km)") +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("./figures/IBD_IBE/Isolation_by_RiverDistance.png", width = 3, height = 3, dpi = 600)

colnames(geo_clim_dist)

# Plot growing degree days vs. FST
p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = `Bio7 - Temperature Annual Range`)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 0, y = 0.2, label = "Mantel r = 0.49\np = 0.0047", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("Bio7 - Temperature Annual Range (standardized)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_text(size = 8))
p
ggsave("./figures/IBD_IBE/Isolation_by_Bio7-TemperatureAnnualRange.png", width = 3, height = 3, dpi = 600)

# Plot growing degree days vs. FST
p <- ggplot(data = geo_clim_dist, aes(y = L_FST, x = minTempWarmest)) +
  geom_point(alpha = 0.8, size = 0.5) +
  stat_smooth(method = "lm", color = "red") +
  geom_text(x = 0, y = 0.2, label = "Mantel r = 0.46\np = 0.0067", hjust = 0, vjust = 1, check_overlap = TRUE) +
  xlab(expression("minTempWarmest (standardized)")) +
  ylab(expression(F[ST] / (1 - F[ST]))) +
  theme_bw() +
  theme(panel.grid = element_blank())
p
ggsave("./figures/Isolation_by_minTempWarmest.png", width = 4, height = 4, dpi = 900)



