# Load necessary libraries
library(tidyverse)
library(readxl)
library(admixtools)


setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")
list.files()

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

# Step 1: Load the PCA results
# Read the eigenvectors (PCA coordinates) file
eigenvec <- read_tsv("./data/Hybrid_Index/Dsamples_B484_B485_nomiss_FMISS001_PCA.eigenvec")%>% rename("Sample_Name" = `#FID`) %>% select(1:11)%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))

# Read the eigenvalues (variance explained by each PC)
eigenval <- read_tsv("./data/Hybrid_Index/Dsamples_B484_B485_nomiss_FMISS001_PCA.eigenval", col_names = FALSE) %>%
  pull(X1)  # Extract eigenvalues as a vector

# Step 2: Calculate the percentage of variance explained by each principal component
total_variance <- sum(eigenval)
variance_explained <- eigenval / total_variance * 100

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx")%>%
  # mutate(Pop_Index = ifelse(Sample_Name %in% paste0("Boltonia_", 475:483), "asteroides", Pop_Index))%>%
  # mutate(Pop_Index = ifelse(Pop_Index %in% c("Pop_01", "Pop_02", "Pop_03"), "P2", "P1"))%>%
  mutate(Pop_Index = ifelse(str_detect(Sample_Species, "asteroides"), "asteroides", Pop_Index))%>%
  mutate(Pop_Index = ifelse(Sample_Species == "B. apalachicolensis", "apalachicolensis", Pop_Index))%>%
  mutate(Pop_Index = ifelse(Sample_Name %in% c("Boltonia_492", "Boltonia_479", "Boltonia_483", "Boltonia_477", "Boltonia_467"), "hybrid", Pop_Index))
  
  


# Step 4: Left join PCA results with metadata
pca_data <- eigenvec %>%
  left_join(Boltonia_metadata, by = "Sample_Name")  # Adjust "IID" as needed for the correct column

# Step 5: Add the percentage of variance explained to the PCA data for labeling
pca_data <- pca_data %>%
  mutate(PC1_variance = variance_explained[1], 
         PC2_variance = variance_explained[2])%>%
  mutate(shape_number = as.integer(factor(Sample_Species))%%4 + 21)

# Step 6: Plot PCA with variance explained on axes
p <- ggplot(data = pca_data, aes(x = PC1, y = PC2, color = Pop_Index)) +  # Replace "Group" with your metadata column
  geom_point(size = 3, aes(shape = Sample_Species)) +
  # geom_point(data = pca_data%>%filter(County == "Cass"), size = 3, fill = "red") +
  theme_bw() +
  labs(
    title = "PCA of all species", 
    x = str_c("PC1 (", round(pca_data$PC1_variance[1], 2), "%)"),
    y = str_c("PC2 (", round(pca_data$PC2_variance[1], 2), "%)")
  ) +
  theme(legend.position = "right")
p

# ggsave("./figures/PCA_all.png", width =6 , height = 4)

mydata.fam <- read_tsv("./data/Hybrid_Index/Boltonia_fstats_all.famoriginal", col_names = FALSE)%>%
  left_join(Boltonia_metadata[,c(1,3)], by = c("X2" = "Sample_Name"))%>%
  mutate(X1 = Pop_Index)

write_tsv(mydata.fam[,1:6], "./data/Hybrid_Index/Boltonia_fstats_all.fam", col_names = FALSE)


prefix <- "./data/Hybrid_Index/Boltonia_fstats_all_FMISS001"  # clover_genotypes.bed/bim/fam

res <- f4(
  prefix,
  pop1   = paste("Pop", str_pad(1:17, width = 2, pad = "0"), sep = "_"),
  pop2   = paste("Pop", str_pad(1:17, width = 2, pad = "0"), sep = "_"),
  pop3   = "asteroides",
  pop4   = "apalachicolensis",
  f4mode = FALSE  # D-statistic = ABBA–BABA test
)


res


p <- ggplot(data = res, aes(x = pop2, y = z))+
  geom_point(aes(color = pop1))+
  geom_boxplot(width = 0.2, outlier.shape = NA, fill = NA)+
  annotate(geom = "text", x = 1, y = Inf, label = "MAF > 0", vjust = 1.5, hjust = 0)+
  theme_bw()

p


####

prefix <- "./data/Hybrid_Index/Dsamples_B484_B485_nomiss_FMISS001_MAF001"  # clover_genotypes.bed/bim/fam

res <- f4(
  prefix,
  pop1   = paste("Pop", str_pad(1:17, width = 2, pad = "0"), sep = "_"),
  pop2   = paste("Pop", str_pad(1:17, width = 2, pad = "0"), sep = "_"),
  pop3   = "asteroides",
  pop4   = "apalachicolensis",
  f4mode = FALSE  # D-statistic = ABBA–BABA test
)


res


p <- ggplot(data = res, aes(x = pop2, y = z))+
  geom_point(aes(color = pop1))+
  geom_boxplot(width = 0.2, outlier.shape = NA, fill = NA)+
  annotate(geom = "text", x = 1, y = Inf, label = "MAF > 0.01", vjust = 1.5, hjust = 0)+
  theme_bw()

p

#####

prefix <- "./data/Hybrid_Index/Dsamples_B484_B485_nomiss_FMISS001_MAF005"  # clover_genotypes.bed/bim/fam

res <- f4(
  prefix,
  pop1   = paste("Pop", str_pad(1:17, width = 2, pad = "0"), sep = "_"),
  pop2   = paste("Pop", str_pad(1:17, width = 2, pad = "0"), sep = "_"),
  pop3   = "asteroides",
  pop4   = "apalachicolensis",
  f4mode = FALSE  # D-statistic = ABBA–BABA test
)


res


p <- ggplot(data = res, aes(x = pop2, y = z))+
  geom_point(aes(color = pop1))+
  geom_boxplot(width = 0.2, outlier.shape = NA, fill = NA)+
  annotate(geom = "text", x = 1, y = Inf, label = "MAF > 0.05", vjust = 1.5, hjust = 0)+
  theme_bw()

p

###

prefix <- "./data/Hybrid_Index/Dsamples_B484_B485_nomiss_FMISS001_MAF01"  # clover_genotypes.bed/bim/fam

res <- f4(
  prefix,
  pop1   = paste("Pop", str_pad(1:17, width = 2, pad = "0"), sep = "_"),
  pop2   = paste("Pop", str_pad(1:17, width = 2, pad = "0"), sep = "_"),
  pop3   = "asteroides",
  pop4   = "apalachicolensis",
  f4mode = FALSE, blgsize = 500 
)


res


p <- ggplot(data = res, aes(x = pop2, y = z))+
  geom_point(aes(color = pop1))+
  geom_boxplot(width = 0.2, outlier.shape = NA, fill = NA)+
  annotate(geom = "text", x = 1, y = Inf, label = "MAF > 0.1, 18590 SNPs", vjust = 1.5, hjust = 0)+
  labs(x = "P2", y = "Z score of D (P3 -> P2)", color = "P1")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

p
library(ape)
library(ggtree)

## Newick for ((P1, P2), asteroides), apalachicolensis
tree_text <- "(((P2,P1),P3_asteroides),O_apalachicolensis);"
tr <- read.tree(text = tree_text)

tr

p_tree <- ggtree(tr) +
  geom_tiplab() +
  scale_x_continuous(expand = c(0,0,2,0))+
  theme_tree()+
  theme(panel.background = element_rect(fill = NA), plot.background = element_rect(fill = NA, color = NA))
p_tree

library(patchwork)
p_with_tree <- p + inset_element(
  p_tree,
  left   = 0.7,  # adjust these four numbers to move/resize
  bottom = 0,
  right  = 1,
  top    = 0.4
)

p_with_tree


ggsave("./figures/Hybrid_Index/ABBA_MAF01_20251204.png", width = 10, height = 7, dpi = 600)
