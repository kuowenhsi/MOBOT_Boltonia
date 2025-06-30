# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(plotly)

setwd("/Users/User/Desktop/REUProject/R/RforPCA&ADMIX")


eigenvec <- read_table("Boltonia_Chr_1_1-23679183_pca.eigenvec", col_names = FALSE)
colnames(eigenvec) <- c("FID", "IID", paste0("PC", 1:(ncol(eigenvec) - 2)))

#Swap DNA results for Boltonia_126 & 127
cols_to_swap <- !(colnames(eigenvec) %in% "IID")
row_126 <- which(eigenvec$IID == "Boltonia_126")
row_127 <- which(eigenvec$IID == "Boltonia_127")
tmp <- eigenvec[row_126, cols_to_swap]
eigenvec[row_126, cols_to_swap] <- eigenvec[row_127, cols_to_swap]
eigenvec[row_127, cols_to_swap] <- tmp

# Step 1: Load the PCA results
# Read the eigenvectors (PCA coordinates) file
eigenvec <- eigenvec |> rename("Sample_Name" = 'IID') |> select(1:11)

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv("Boltonia_Chr_1_1-23679183_pca.eigenval", col_names = FALSE) |>
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100

# Step 3: Load metadata CSV
metadata <- read_csv("Boltonia_merged_data_20240925.csv") |>
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))

# Step 4: Left join PCA results with metadata
pca_data <- eigenvec |>
  left_join(metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column


# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data |>
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2])

# Define custom shapes
custom_shapes <- rep(c(15, 16, 17, 18), length.out = n_distinct(pca_data$County))


# Plot
p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = County, shape = County)) +
  geom_point(size = 3) +
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
ggsave("../../Figures/Boltonia_functional_PCA.png", width = 10, height = 10, dpi = 600)

#creates an interactive plot of p
ggplotly(p)


#PC1 vs latitude
p <- ggplot(pca_data, aes(x = PC1, y = Google_latitude, color = County, shape = County)) +
  geom_point(size = 3) +
  scale_shape_manual(values = custom_shapes) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  )
p

ggsave("../../Figures/Boltonia_PC1_latitude.png", width = 10, height = 10, dpi = 600)

#PC5 vs latitude
p <- ggplot(pca_data, aes(x = PC5, y = Google_latitude, color = County, shape = County)) +
  geom_point(size = 3) +
  scale_shape_manual(values = custom_shapes) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  )
p

ggsave("../../Figures/Boltonia_PC5_latitude.png", width = 10, height = 10, dpi = 600)
