# Load necessary libraries
library(tidyverse)
library(pals)
library(plotly)

setwd("/Users/User/Desktop/MOBOT_Boltonia")
filename <- "./../REUProject_LargeFiles/Data/PCadapt_output/Boltonia_decurrens_100kb0.8_pca"
metadata <- read_csv("./Data/population_ID.csv")
PCX <- "1"
PCY <- "2"

eigenvec <- read_table(paste0(filename, ".eigenvec"), col_names = TRUE)
colnames(eigenvec) <- c("IID", paste0("PC", 1:(ncol(eigenvec) - 1)))

 #Swap DNA results for Boltonia_126 & 127
 cols_to_swap <- !(colnames(eigenvec) %in% "IID")
 row_126 <- which(eigenvec$IID == "Boltonia_126")
 row_127 <- which(eigenvec$IID == "Boltonia_127")
 tmp <- eigenvec[row_126, cols_to_swap]
 eigenvec[row_126, cols_to_swap] <- eigenvec[row_127, cols_to_swap]
 eigenvec[row_127, cols_to_swap] <- tmp

# Step 1: Load the PCA results
# Read the eigenvectors (PCA coordinates) file
eigenvec <- eigenvec |> select(1:11)

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv(paste0(filename, ".eigenval"), col_names = FALSE) |>
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100

# Step 4: Left join PCA results with metadata
metadata <- metadata %>%
  mutate(IID = str_c("Boltonia_", str_pad(as.character(IID), width = 3, pad = "0")))
pca_data <- eigenvec |>
  left_join(metadata, by = "IID")  # Adjust "IID" as needed for the correct column
pca_data <- pca_data |> 
  select(-Pop)

# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data |>
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2],
         PC3_variance = variance_explained[3],
         PC4_variance = variance_explained[4])

# Define custom shapes
custom_shapes <- rep(c(21, 22, 23, 24, 25), length.out = n_distinct(pca_data$Sample_Group))

 #adjust

adj_xy <- tribble(
   ~Sample_Group, ~adj_X_shift, ~adj_Y_shift,
   "scott",           -0.02,       0.004,
   "saint_clair",    -0.025,       0.005,
   "madison",         -0.016,        0.007,
   "jersey",           0.02,        -0.005,
   "cass",             0.02,       0.004,
   "morgan",          -0.02,       0.01,
   "tazewell",         -0.025,       0,
   "peoria",          0.02,        0,
   "frederick",       -0.021,        -0.006,
   "schuyler",        -0.025,        -0.001,
   "woodford",         -0.02,       0,
   "marshal",         0.02,        0,
   "fulton1",           0.02,        0.01,
   "fulton2",           -0.02,        -0.01,
   "fulton3",           0.022,        -0.005,
   "alton",            0.017,        -0.002,
   "hennepin",        0.02,        0,
 )

 #set centers

 centroids <- pca_data |>
   group_by(Sample_Group)|>
   summarise(mean_PC1 = mean(PC1), mean_PC2 = mean(PC2), mean_PC3 = mean(PC3), mean_PC4 = mean(PC4), .groups = "drop")
 pca_data <- pca_data |>
   left_join(centroids, by = "Sample_Group") |>
   mutate(
     dist = sqrt(
       (.data[[paste0("PC", PCX)]]      - .data[[paste0("mean_PC", PCX)]])^2 +
         (.data[[paste0("PC", PCY)]]      - .data[[paste0("mean_PC", PCY)]])^2
     )
   )
 closest_points <- pca_data |>
   group_by(Sample_Group) |>
   slice_min(dist, n = 1, with_ties = FALSE) |>
   ungroup()

 closest_points <- closest_points |>
   left_join(adj_xy, by = c("Sample_Group")) |>
   mutate(
     adj_X = .data[[paste0("PC", PCX)]] + adj_X_shift,
     adj_Y = .data[[paste0("PC", PCY)]] + adj_Y_shift
   ) |>
   select(-adj_X_shift, -adj_Y_shift)

#custom order based on latitude
 custom_order <- c("hennepin", "marshal", "woodford", "peoria", "tazewell", "fulton3", "fulton1", "fulton2", "frederick", "schuyler", "cass", "morgan", "scott", "jersey", "alton", "madison", "saint_clair")
 pca_data$Sample_Group <- factor(pca_data$Sample_Group, levels = custom_order)

# Plot
p <- ggplot() +
  geom_point(data = pca_data, 
             aes(x = .data[[paste0("PC", PCX)]], 
                 y = .data[[paste0("PC", PCY)]],
                 color = Sample_Group,
                 shape = Sample_Group,
                 fill = Sample_Group
             ), 
             alpha = 0.4, 
             size = 2.5
  ) +
   geom_point(data = closest_points,
              aes( x = .data[[paste0("PC", PCX)]],
                   y = .data[[paste0("PC", PCY)]], 
                   fill = Sample_Group, 
                   shape = Sample_Group
              ),
              color = "black",   # black outline
              size = 3.3,          # slightly bigger for emphasis
              stroke = 0.83       # thickness of black outline
   ) +
   geom_segment(
     data = closest_points, 
     aes( x = .data[[paste0("PC", PCX)]],
          y = .data[[paste0("PC", PCY)]], 
          xend = adj_X, 
          yend = adj_Y),
     color = "black"
   ) +
   geom_label(
     data = closest_points,
     aes(adj_X, adj_Y, label = Sample_Group),
     size = 3,
     fill = "white",     # white background
     color = "black",
     label.size = 0
   ) +
  scale_shape_manual(values = custom_shapes) +
  scale_fill_manual(values = cols25())  +
  scale_color_manual(values = cols25()) +
  labs(
    title = "PCA Plot by Sample Group",
    x = str_c("PC", PCX, " (", round(pca_data[[str_c("PC", PCX, "_variance")]][1], 2), "%)"),
    y = str_c("PC", PCY, " (", round(pca_data[[str_c("PC", PCY, "_variance")]][1], 2), "%)")
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  ) + 
  coord_fixed() +
  scale_y_reverse()

p
ggsave(paste0("./Figures/Boltonia_Decurrens_PCA_", PCX, "-", PCY, ".png"), 
       width = 8, 
       height = 8, 
       dpi = 300, 
       bg = "white")


ggplotly(p)






#PC1 vs latitude
p <- ggplot(pca_data, aes(x = PC1, y = Google_latitude, color = Sample_Group, shape = Sample_Group)) +
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
p <- ggplot(pca_data, aes(x = PC5, y = Google_latitude, color = Sample_Group, shape = Sample_Group)) +
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
