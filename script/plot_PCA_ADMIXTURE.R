# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(ggrepel)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")
list.files()

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

# Step 1: Load the PCA results
# Read the eigenvectors (PCA coordinates) file
eigenvec <- read_tsv("./data/PCA_all/Boltonia_all_ID_LD_PCA.eigenvec")%>% rename("Sample_Name" = `#IID`) %>% select(1:11)%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv("./data/PCA_all/Boltonia_all_ID_LD_PCA.eigenval", col_names = FALSE) %>%
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100

# Step 3: Load metadata CSV
metadata <- read_xlsx("./data/DNA_stock_Boltonia.xlsx")[1:4] %>%
  left_join(read_csv("./data/Boltonia_merged_data_20240925.csv")[c(1,7:12)], by = "index")%>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))%>%
  mutate(Adapted_Longitude = case_when(is.na(Longitude) ~ Google_longitude, TRUE ~ Longitude), Adapted_Latitude = case_when(is.na(Latitude) ~ Google_latitude, TRUE ~ Latitude))%>%
  mutate(Sample_Species = case_when(index <= 468 ~ "B. decurrens", TRUE ~ MaternalLine))%>%
  select(Sample_Name,Sample_Species, everything())

# Step 4: Left join PCA results with metadata
pca_data <- eigenvec %>%
  left_join(metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column

# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data %>%
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2])%>%
  mutate(shape_number = as.integer(factor(Sample_Species))%%4 + 21)

# Step 6: Plot PCA with variance explained on axes
p <- ggplot(data = pca_data, aes(x = PC1, y = PC2, fill = Sample_Species)) +  # Replace "Group" with your metadata column
  geom_point(size = 3, aes(shape = Sample_Species)) +
  # geom_point(data = pca_data%>%filter(County == "Cass"), size = 3, fill = "red") +
  theme_bw() +
  labs(
    title = "PCA of all species", 
    x = str_c("PC1 (", round(pca_data$PC1_variance[1], 2), "%)"),
    y = str_c("PC2 (", round(pca_data$PC2_variance[1], 2), "%)")
  ) +
  scale_shape_manual(values = c(21,22,23,24,21,22,23,24,21))+
  theme(legend.position = "right")
p

ggsave("./figures/PCA_all.png", width =6 , height = 4)

write_csv(pca_data, "./data/PCA_all_dataset.csv")

#########################################################
# Read the eigenvectors (PCA coordinates) file
eigenvec <- read_tsv("./data/PCA_hybrid/Boltonia_hybrid_ID_fillmissing_LD.eigenvec")%>% rename("Sample_Name" = `#FID`) %>% select(1:11)%>%
  mutate(Sample_Name = paste(Sample_Name, IID, sep = "_"))%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv("./data/PCA_hybrid/Boltonia_hybrid_ID_fillmissing_LD.eigenval", col_names = FALSE) %>%
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100

# Step 4: Left join PCA results with metadata
pca_data <- eigenvec %>%
  left_join(metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column

# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data %>%
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2])%>%
  mutate(ring_color = case_when(Sample_Name %in% c("Boltonia_477", "Boltonia_483", "Boltonia_479", "Boltonia_492") ~ "red", TRUE ~ "black"))
  

# Step 6: Plot PCA with variance explained on axes
p <- ggplot(data = pca_data, aes(x = PC1, y = PC2)) +  # Replace "Group" with your metadata column
  geom_point(size = 3, aes(shape = County, fill = County)) +
  # geom_point(data = pca_data%>%filter(County == "Cass"), size = 3, fill = "red") +
  theme_bw() +
  labs(
    title = "PCA of hybrid species", 
    x = str_c("PC1 (", round(pca_data$PC1_variance[1], 2), "%)"),
    y = str_c("PC2 (", round(pca_data$PC2_variance[1], 2), "%)")
  ) +
  scale_shape_manual(values = c(21,22,23,24,21,22,23,24,21, 21,22,23,24,21,22))+
  theme(legend.position = "right")
p

ggsave("./figures/PCA_hybrid.png", width =6 , height = 4)

########################################
# Read the eigenvectors (PCA coordinates) file
eigenvec <- read_tsv("./data/PCA_decurrens/Boltonia_decurrens_100kb0.8_pca.eigenvec")%>% rename("Sample_Name" = `#IID`) %>% select(1:11)%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv("./data/PCA_decurrens/Boltonia_decurrens_100kb0.8_pca.eigenval", col_names = FALSE) %>%
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100


# Step 4: Left join PCA results with metadata
pca_data <- eigenvec %>%
  left_join(metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column

# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data %>%
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2])%>%
  mutate(shape_number = as.integer(factor(Sample_Species))%%4 + 21)

pca_data_mean <- pca_data %>%
  group_by(County, MaternalLine)%>%
  summarize_all(.fun = "mean")%>%
  ungroup()%>%
  arrange(Adapted_Latitude)%>%
  mutate(County = factor(County, levels = unique(County)))%>%
  mutate(river_number = as.integer(County))

levels(pca_data_mean$County)

# Step 6: Plot PCA with variance explained on axes
p <- ggplot(data = pca_data_mean, aes(x = PC1, y = PC2, fill = County)) +  # Replace "Group" with your metadata column
  geom_point(data = pca_data, aes(x = PC1, y = PC2), color = "gray85", inherit.aes = FALSE)+
  geom_text_repel(aes(label = river_number), size = 3, position = position_nudge(x = c(0,0,0,0,0,0,0.005,-0.005,0,0.005,0,-0.01,0,0,0,0,0,0), y = c(0,0,0,0,0,0,0,0.005,0,0,0,0,0.005,0.005,0,0.005,0,0)))+
  geom_point(size = 2.2, aes(shape = County), position = position_nudge(x = c(0,0,0,0,0,0,0.005,-0.005,0,0.005,0,-0.01,0,0,0,0,0,0), y = c(0,0,0,0,0,0,0,0.005,0,0,0,0,0.005,0.005,0,0.005,0,0))) +
  theme_bw() +
  labs(
    title = "PCA of B. decurrens", 
    x = str_c("PC1 (", round(pca_data$PC1_variance[1], 2), "%)"),
    y = str_c("PC2 (", round(pca_data$PC2_variance[1], 2), "%)")
  ) +
  scale_fill_discrete(labels = paste(1:15, levels(pca_data_mean$County)))+
  scale_shape_manual(values = rep(c(21,22,23,24), length = 15), labels = paste(1:15, levels(pca_data_mean$County)))+
  theme(legend.position = "right")
p

ggsave("./figures/PCA_decurrens.png", width =5.5 , height = 4)

write_csv(pca_data, "./data/PCA_decurrens_dataset.csv")

# Get unique counties
counties <- sort(unique(pca_data$County))
n_categories <- length(counties)

# Define custom shapes and colors
custom_shapes <- c(16, 17, 15, 3, 7, 8, 4, 18, 0, 1, 2, 5, 6, 9, 10)[1:n_categories]
custom_colors <- c(
  "#E41A1C",  # red
  "#377EB8",  # blue
  "#4DAF4A",  # green
  "#984EA3",  # purple
  "#FF7F00",  # orange
  "#A65628",  # brown
  "#F781BF",  # pink
  "#999999",  # gray
  "#66C2A5",  # turquoise
  "#FC8D62",  # salmon
  "#8DA0CB",  # steel blue
  "#E78AC3",  # light pink
  "#A6D854",  # lime green
  "#FFD92F",  # yellow
  "#E5C494"   # tan
)

# Named vectors to map to specific counties
names(custom_shapes) <- counties
names(custom_colors) <- counties

# Plot
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = County, shape = County)) +
  geom_point(size = 3) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  labs(
    title = "PCA Plot by County",
    x = str_c("PC1 (", round(pca_data$PC1_variance[1], 2), "%)"),
    y = str_c("PC2 (", round(pca_data$PC2_variance[1], 2), "%)")
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  )

p
ggsave("Boltonia_functional_PCA.png", width = 10, height = 10, dpi = 600)

p <- ggplot(pca_data, aes(x = PC1, y = Google_latitude, color = County, shape = County)) +
  geom_point(size = 3) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  )
p

ggsave("Boltonia_PC1_latitude.png", width = 10, height = 10, dpi = 600)

p <- ggplot(pca_data, aes(x = PC5, y = Google_latitude, color = County, shape = County)) +
  geom_point(size = 3) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = custom_shapes) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  )
p

ggsave("Boltonia_PC5_latitude.png", width = 10, height = 10, dpi = 600)

qmat <- read_table("Boltonia_imputed_admixture.3.Q", col_names = FALSE) %>%
  mutate(Sample_Name = read_table("Boltonia_imputed_admixture.fam", col_names = FALSE) %>% pull(X2))%>%
  left_join(metadata %>% select(1:14, "Sample_Name"), by = "Sample_Name")%>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Ancestry", 
    values_to = "Proportion"
  )

p <- ggplot(qmat, aes(x = Sample_Name, y = Proportion, fill = Ancestry)) +
  geom_bar(stat = "identity", width = 1) +
  theme_bw() +
  ggh4x::facet_nested(.~reorder(paste0(County, "\n",MaternalLine), Google_latitude), scales = "free_x", space = "free_x")+
  labs(
    title = paste("ADMIXTURE Plot (K =", "3", ")"),
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

ggsave("Boltonia_ADMIXTURE_K_3.png", width = 35, height = 6, dpi = 600)



#########


plot_admixture <- function(qfile) {
  # Extract K from the filename using regex
  k_val <- stringr::str_extract(qfile, "(?<=\\.)(\\d+)(?=\\.Q)")  # grabs the number between "." and ".Q"
  
  # Derive base filename without extension
  base_name <- tools::file_path_sans_ext(qfile)
  
  # Read Q matrix and corresponding FAM file
  qmat <- read_table(qfile, col_names = FALSE) %>%
    mutate(Sample_Name = read_table(str_replace(qfile, "\\.\\d+\\.Q", ".fam"), col_names = FALSE) %>% pull(X2)) %>%
    mutate(Sample_Name = replace_126_127(Sample_Name))%>%
    left_join(metadata %>% select(1:14, "Sample_Name"), by = "Sample_Name") %>%
    pivot_longer(
      cols = starts_with("X"), 
      names_to = "Ancestry", 
      values_to = "Proportion"
    )
  
  # Plot
  p <- ggplot(qmat, aes(x = Sample_Name, y = Proportion, fill = Ancestry)) +
    geom_bar(stat = "identity", width = 1) +
    theme_bw() +
    ggh4x::facet_nested(~reorder(paste0(County, "\n", MaternalLine), Adapted_Latitude), 
                        scales = "free_x", space = "free_x") +
    labs(
      title = "",
      x = "",
      y = ""
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x.top = element_blank(),
      axis.ticks.x.bottom = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0, "lines"),
      plot.background = element_rect(fill = "white"),
      legend.position = "none"
    ) +
    scale_fill_brewer(palette = "Set3")+
    scale_y_continuous(expand = c(0, 0))
  
  print(p)
  
  # Save plot
  ggsave(
    filename = paste0("Boltonia_neutral_ADMIXTURE_K_", k_val, ".png"),
    plot = p,
    width = 35,
    height = 6,
    dpi = 600
  )
}



# Extract K from the filename using regex
k_val <- stringr::str_extract("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.2.Q", "(?<=\\.)(\\d+)(?=\\.Q)")  # grabs the number between "." and ".Q"

# Derive base filename without extension
base_name <- tools::file_path_sans_ext("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.2.Q")

# Read Q matrix and corresponding FAM file
qmat <- read_table("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.2.Q", col_names = FALSE) %>%
  mutate(Sample_Name = read_table(str_replace("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.2.Q", "\\.\\d+\\.Q", ".fam"), col_names = FALSE) %>% pull(X2)) %>%
  mutate(Sample_Name = replace_126_127(Sample_Name))%>%
  left_join(metadata %>% select(1:14, "Sample_Name"), by = "Sample_Name") %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Ancestry", 
    values_to = "Proportion"
  )


qmat_mean <- qmat %>%
  group_by(County, MaternalLine)%>%
  summarize(sample_number = n(), Adapted_Latitude = mean(Adapted_Latitude))%>%
  arrange(Adapted_Latitude)%>%
  mutate(County = factor(County, levels = unique(.$County)))%>%
  mutate(shape_number = (as.integer(County) + 3)%%4 + 21)



p <- ggplot(data = qmat_mean, aes(fill = County, shape = County))+
  geom_bar(data = qmat, aes(x = Sample_Name, y = Proportion), fill = NA,stat = "identity", width = 1) +
  geom_point(aes(x = sample_number/4, shape = I(shape_number)), y = 0.1, size = 15, angle = 90)+
  geom_text(aes(x = sample_number/4, label = paste(as.integer(County), County)), size = 18, angle = 90, hjust =0, y = 0.2)+
  ggh4x::facet_nested(~reorder(paste0(County, "\n", MaternalLine), Adapted_Latitude), 
                      scales = "free_x", space = "free_x") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal()+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    plot.background = element_blank(),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = c(0, 0))

p

ggsave(
  filename = "ADMIXTURE_labels.png",
  plot = p,
  width = 35,
  height = 6,
  dpi = 600
)

plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.2.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.3.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.4.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.5.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.6.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.7.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.8.Q")
plot_admixture("Boltonia_neutral_snps_admixture.3.Q")
plot_admixture("Boltonia_neutral_snps_admixture.4.Q")
plot_admixture("Boltonia_neutral_snps_admixture.5.Q")
plot_admixture("Boltonia_neutral_snps_admixture.6.Q")
plot_admixture("Boltonia_neutral_snps_admixture.7.Q")



plot_admixture("Boltonia_functional_snps_admixture.2.Q")
plot_admixture("Boltonia_functional_snps_admixture.3.Q")
plot_admixture("Boltonia_functional_snps_admixture.4.Q")
plot_admixture("Boltonia_functional_snps_admixture.5.Q")
plot_admixture("Boltonia_functional_snps_admixture.6.Q")
plot_admixture("Boltonia_functional_snps_admixture.7.Q")
plot_admixture("Boltonia_functional_snps_admixture.2.Q")
plot_admixture("Boltonia_functional_snps_admixture.2.Q")
plot_admixture("Boltonia_imputed_admixture.3.Q")
plot_admixture("Boltonia_imputed_admixture.4.Q")
plot_admixture("Boltonia_imputed_admixture.5.Q")
plot_admixture("Boltonia_imputed_admixture.6.Q")
plot_admixture("Boltonia_imputed_admixture.7.Q")
plot_admixture("Boltonia_imputed_admixture.8.Q")
