# Load necessary libraries
library(tidyverse)
library(RColorBrewer)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia")

list.files()

# Step 1: Load the PCA results
# Read the eigenvectors (PCA coordinates) file
eigenvec <- read_tsv("Boltonia_functional_snps_pca.eigenvec")%>% rename("Sample_Name" = `#IID`) %>% select(1:11)

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv("Boltonia_functional_snps_pca.eigenval", col_names = FALSE) %>%
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100

# Step 3: Load metadata CSV
metadata <- read_csv("Boltonia_merged_data_20240925.csv") %>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))

# Step 4: Left join PCA results with metadata
pca_data <- eigenvec %>%
  left_join(metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column

# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data %>%
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2])

# Step 6: Plot PCA with variance explained on axes
p <- ggplot(data = pca_data, aes(x = PC1, y = PC2, color = factor(County))) +  # Replace "Group" with your metadata column
  geom_point(size = 3, aes(shape = County)) +
  theme_minimal() +
  labs(
    title = "PCA Plot", 
    x = str_c("PC1 (", round(pca_data$PC1_variance[1], 2), "%)"),
    y = str_c("PC2 (", round(pca_data$PC2_variance[1], 2), "%)")
  ) +
  scale_color_discrete(name = "Group") +  # Replace "Group" with the actual metadata variable
  theme(legend.position = "right")
p

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
    ggh4x::facet_nested(~reorder(paste0(County, "\n", MaternalLine), Google_latitude), 
                        scales = "free_x", space = "free_x") +
    labs(
      title = paste("ADMIXTURE Plot based on neutral SNPs (K =", k_val, ")"),
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


plot_admixture("Boltonia_neutral_snps_admixture.2.Q")
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
