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
# metadata <- read_xlsx("./data/DNA_stock_Boltonia.xlsx")[1:4] %>%
#   left_join(read_csv("./data/Boltonia_merged_data_20240925.csv")[c(1,7:12)], by = "index")%>%
#   mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))%>%
#   mutate(Adapted_Longitude = case_when(is.na(Longitude) ~ Google_longitude, TRUE ~ Longitude), Adapted_Latitude = case_when(is.na(Latitude) ~ Google_latitude, TRUE ~ Latitude))%>%
#   mutate(Sample_Species = case_when(index <= 468 ~ "B. decurrens", TRUE ~ MaternalLine))%>%
#   select(Sample_Name,Sample_Species, everything())


Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx")%>%
  mutate(Pop_Index = ifelse(is.na(Pop_Index), str_remove(Sample_Name, "Boltonia_"), Pop_Index))%>%
  mutate(Pop_Index = ifelse(Sample_Species == "B. asteroides (sympatric)", "asteroides", Pop_Index))


# Step 4: Left join PCA results with metadata
pca_data <- eigenvec %>%
  left_join(Boltonia_metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column

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
eigenvec <- read_tsv("./data/PCA_hybrid/Boltonia_Hybrid_samples_maf0.01_ld100kb0.8_PCA.eigenvec")%>% rename("Sample_Name" = `#IID`) %>% select(1:11)%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv("./data/PCA_hybrid/Boltonia_Hybrid_samples_maf0.01_ld100kb0.8_PCA.eigenval", col_names = FALSE) %>%
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100

# Step 4: Left join PCA results with metadata
pca_data <- eigenvec %>%
  left_join(Boltonia_metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column

# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data %>%
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2])%>%
  mutate(shape_number = as.integer(factor(Sample_Species))%%4 + 21)%>%
  mutate(ring_color = case_when(Sample_Name %in% c("Boltonia_477", "Boltonia_483", "Boltonia_479", "Boltonia_492") ~ "gray65", TRUE ~ "NA"))
  
# 1. Compute median coordinates per Pop_Index
pca_labels <- pca_data %>%
  group_by(Pop_Index) %>%
  summarise(
    PC1 = median(PC1, na.rm = TRUE),
    PC2 = median(PC2, na.rm = TRUE),
    PC3 = median(PC3, na.rm = TRUE),
    PC4 = median(PC4, na.rm = TRUE),
    .groups = "drop"
  )%>%
  mutate(Pop_Index = str_remove(Pop_Index, "Pop_"))

# Step 6: Plot PCA with variance explained on axes
p <- ggplot(data = pca_data, aes(x = PC1, y = PC2)) +  # Replace "Group" with your metadata column
  geom_point(size = 3, aes(shape = I(shape_number), fill = I(ring_color), color = Sample_Species), stroke = 1) +
  geom_text_repel(
    data = pca_labels,
    aes(label = Pop_Index),
    size = 2.5,
    max.overlaps = 50,
    show.legend = FALSE
  ) +
  # geom_point(data = pca_data%>%filter(County == "Cass"), size = 3, fill = "red") +
  theme_bw() +
  labs(
    x = str_c("PC1 (", round(pca_data$PC1_variance[1], 2), "%)"),
    y = str_c("PC2 (", round(pca_data$PC2_variance[1], 2), "%)")
  ) +
  theme(legend.position = c(0.2,0.2), legend.title = element_blank(), legend.background = element_rect(fill = NA), legend.text = element_text(size = 8), legend.key = element_rect(fill = NA), legend.key.size = unit(0.01, "in"))
p

ggsave("./figures/PCA_hybrid.svg", width =4 , height = 4)

########################################
# Read the eigenvectors (PCA coordinates) file
eigenvec <- read_tsv("./data/PCA_decurrens/Boltonia_decurrens_imputed_Low_LD_pca.eigenvec")%>% rename("Sample_Name" = `#IID`) %>% select(1:11)

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv("./data/PCA_decurrens/Boltonia_decurrens_imputed_Low_LD_pca.eigenval", col_names = FALSE) %>%
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100

p <- ggplot(data = tibble(PC = 1:20, Eigenvalue = eigenval[1:20]), aes(x = PC, y = Eigenvalue))+
  geom_line(group = 1)+
  geom_point()+
  geom_hline(yintercept = 5.5, linetype = 2 , color = "red")+
  theme_bw()

p

ggsave("./figures/PCA_decurrens/PCA_decurrens_screeplot.png", width =4 , height = 4)

# Step 4: Left join PCA results with metadata
pca_data <- eigenvec %>%
  left_join(Boltonia_metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column

# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data %>%
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2],
         PC3_variance = variance_explained[3],
         PC4_variance = variance_explained[4])%>%
  mutate(shape_number = as.integer(factor(Sample_Species))%%4 + 21)

pca_data_mean <- pca_data %>%
  group_by(Pop)%>%
  summarize_all(.fun = "mean")%>%
  ungroup()%>%
  arrange(Adapted_Latitude)%>%
  mutate(Pop_index = 1:n())%>%
  mutate(Pop_Label = paste(Pop_index, Pop, sep = " - "))%>%
  mutate(shape_number = (Pop_index + 3)%%4 + 21) %>%
  mutate(adj_PC1 = c(0,0,0,0,0,0,0.005,-0.005,0,0.005,0,-0.01,0,0,0,0,0),
         adj_PC2 = c(0,0,0,0,0,0,0,0.005,0,0,0,0,0.005,0.005,0,0.005,0),
         adj_PC3 = c(0,0,0,0,0,0,0,0,0,0,0,0.001,-0.001,0,0.005,0,0),
         adj_PC4 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.005,0,-0.001,0))

levels(pca_data_mean$County)

# Step 6: Plot PCA with variance explained on axes
p <- ggplot(data = pca_data_mean, aes(x = PC1, y = PC2, fill = Pop)) +  # Replace "Group" with your metadata column
  geom_point(data = pca_data, aes(x = PC1, y = PC2), color = "gray85", inherit.aes = FALSE)+
  geom_text(aes(x = PC1 + adj_PC1, y = PC2 + adj_PC2, label = as.character(Pop_index)),
            position = position_nudge(x = 1.3 * c(0.007,0.007,0.007,0.007,0.007,0.007,0.007,0.007,0.007,0,0,-0.01,0.01,0.01,0.01,0.01,0.01),
                                      y = 1.3 * c(0,0,0,0,0.003,0.003, 0.003,0.003,0.003,0.005,-0.005,0,0,0,0,0.001,0)))+
  geom_point(aes(x = PC1 + adj_PC1, y = PC2 + adj_PC2, shape = I(shape_number)),size = 3) +
  theme_bw() +
  labs(
    x = str_c("PC1 (", round(pca_data$PC1_variance[1], 2), "%)"),
    y = str_c("PC2 (", round(pca_data$PC2_variance[1], 2), "%)")
  ) +
  scale_fill_discrete(labels = pca_data_mean$Pop_Label)+
  theme(legend.position = "none")
p

ggsave("./figures/PCA_decurrens/PCA_decurrens.png", width =4 , height = 4)



# Step 6: Plot PCA with variance explained on axes
p <- ggplot(data = pca_data_mean, aes(x = PC3, y = PC4, fill = Pop)) +  # Replace "Group" with your metadata column
  geom_point(data = pca_data, aes(x = PC3, y = PC4), color = "gray85", inherit.aes = FALSE)+
  geom_text(aes(x = PC3 + adj_PC3, y = PC4 + adj_PC4, label = as.character(Pop_index)),
            position = position_nudge(x = 0.0095 * c(0,1,1,1,1,1,0,0,0,1,1,0,-1,-1,1,-1,-1),
                                      y = 0.0095 * c(1,1,1,1,1,1,-1,1.2,-1,-0.3,-0.1,1.2,0.1,0.3,-1,1,1)))+
  geom_point(aes(x = PC3 + adj_PC3, y = PC4 + adj_PC4, shape = I(shape_number)),size = 3) +
  theme_bw() +
  labs(
    x = str_c("PC3 (", round(pca_data$PC3_variance[1], 2), "%)"),
    y = str_c("PC4 (", round(pca_data$PC4_variance[1], 2), "%)")
  ) +
  scale_fill_discrete(labels = pca_data_mean$Pop_Label)+
  theme(legend.position = "none")
p

ggsave("./figures/PCA_decurrens/PCA-PC3PC4_decurrens.png", width =4 , height = 4)

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

########################################################

# Files like: Bdecurrens_admix_20250928_K1_cv5.log ... K16 ...
files <- list.files(
  path = "./data/ADMIXTURE_decurrens/",
  pattern = "^Bdecurrens_admix_20250928_K\\d+_cv5\\.log$",
  full.names = TRUE
)

# Regex: capture K and the numeric value (supports decimals and scientific notation)
rx <- "^\\s*CV error \\(K=(\\d+)\\):\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?\\d+)?)\\s*$"

CV_error <- do.call(
  rbind,
  lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    hits  <- grep(rx, lines, perl = TRUE)
    if (length(hits) == 0) return(NULL)          # no matching line in this file
    keep  <- lines[hits]                         # keep matching line(s); often length 1
    m     <- regexec(rx, keep, perl = TRUE)
    cap   <- regmatches(keep, m)
    
    # Build one row per matching line
    do.call(rbind, lapply(cap, function(x) {
      data.frame(
        file     = basename(f),
        K        = as.integer(x[2]),
        cv_error = as.numeric(x[3]),
        line     = x[1],
        stringsAsFactors = FALSE
      )
    }))
  })
)

p <- ggplot(data = CV_error, aes(x = K, y = cv_error)) +
  geom_line()+
  geom_point()+
  theme_bw()
p


ggsave("./figures/ADMIXTURE_decurrens/CV_errors.png", width = 5, height = 5, dpi = 600)
#########################################################


qmat <- read_table("./data/ADMIXTURE_decurrens/Boltonia_decurrens_imputed_Low_LD_admixture.1.Q", col_names = FALSE) %>%
  mutate(Sample_Name = read_table("./data/ADMIXTURE_decurrens/Boltonia_decurrens_imputed_Low_LD_admixture.fam", col_names = FALSE) %>% pull(X2))%>%
  left_join(Boltonia_metadata, by = "Sample_Name")%>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Ancestry", 
    values_to = "Proportion"
  )


qmat_mean <- qmat %>%
  group_by(Pop)%>%
  summarize(sample_number = n(), Adapted_Latitude = mean(Adapted_Latitude))%>%
  ungroup()%>%
  arrange(Adapted_Latitude)%>%
  mutate(Pop_index = 1:n())%>%
  mutate(Pop_Label = paste(Pop_index, Pop, sep = " - "))%>%
  mutate(shape_number = (Pop_index + 3)%%4 + 21)

qmat <- qmat %>%
  left_join(qmat_mean %>%select(Pop, Pop_index), by = "Pop")


p <- ggplot(data = qmat_mean, aes(fill = Pop, shape = Pop))+
  geom_bar(data = qmat, aes(y = Sample_Name, x = Proportion), fill = NA,stat = "identity", width = 1) +
  geom_point(aes(y = sample_number/2, shape = I(shape_number)), x = 0.1, size = 5)+
  geom_text(aes(y = sample_number/2, label = Pop_Label, size = 6, angle = 0, hjust =0, x = 0.2))+
  ggh4x::facet_nested(reorder(Pop, desc(Pop_index))~., 
                      scales = "free_y", space = "free_y") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.background = element_rect(color = "white", fill = "#d9d9d9"),
    legend.position = "none"
  ) +
  scale_x_continuous(expand = c(0, 0))+
  coord_cartesian(clip = FALSE)

p

ggsave(
  filename = "./figures/ADMIXTURE_decurrens/ADMIXTURE_labels.png",
  plot = p,
  width = 4,
  height = 10,
  dpi = 600
)



#########################################################

base <- "./data/ADMIXTURE_decurrens/Boltonia_decurrens_imputed_Low_LD_admixture"

# sample names (IID is column 2 in .fam)
samples <- read_table(paste0(base, ".fam"),
                      col_names = FALSE, col_types = cols(.default = "c")) %>%
  transmute(Sample_Name = X2)

read_one_q <- function(K) {
  qfile <- sprintf("%s.%d.Q", base, K)
  
  # Read Q matrix; number of columns should be K
  q <- read_table(qfile, col_names = FALSE,
                  col_types = cols(.default = col_double()))
  
  # Name columns Q1..QK deterministically, then pivot
  q_names <- paste0("Q", seq_len(ncol(q)))  # safer than seq_len(K)
  colnames(q) <- q_names
  
  # Optional sanity check
  if (ncol(q) != K) {
    warning(sprintf("File %s has %d columns but K = %d", basename(qfile), ncol(q), K))
  }
  
  q %>%
    bind_cols(samples) %>%
    pivot_longer(
      cols = all_of(q_names),
      names_to = "Ancestry",
      values_to = "Proportion"
    ) %>%
    mutate(
      K = K,
      Ancestry = factor(Ancestry, levels = q_names)
    )
}

qmat <-
  map_dfr(c(3,4,7,9), read_one_q) %>%
  left_join(Boltonia_metadata, by = "Sample_Name") %>%
  relocate(Sample_Name, K, Ancestry, Proportion) %>%
  left_join(qmat_mean %>%select(Pop, Pop_index), by = "Pop")
#########################################################


p <- ggplot(qmat, aes(y = Sample_Name, x = Proportion, fill = Ancestry)) +
  geom_bar(stat = "identity", width = 1) +
  theme_bw() +
  ggh4x::facet_nested(reorder(Pop, desc(Pop_index))~K, scales = "free_y", space = "free_y")+
  labs(
    x = "",
    y = ""
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.x.top = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.y = unit(0, "lines"),
    panel.border = element_rect(color = "white"),
    strip.background = element_rect(color = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "none",
    strip.text.y.right = element_text(angle = 0)
  ) +
  scale_fill_brewer(palette = "Set3")+
  scale_x_continuous(expand = c(0,0))

p

ggsave("./figures/ADMIXTURE_decurrens/Boltonia_ADMIXTURE_K_3479.png", width = 8, height = 10, dpi = 600)


#########################






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


plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.2.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.3.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.4.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.5.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.6.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.7.Q")
plot_admixture("./data/ADMIXTURE_decurrens/Boltonia_decurrens_100kb0.8.8.Q")

