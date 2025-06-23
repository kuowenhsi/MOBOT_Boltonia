library(vcfR)
library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

# --- INPUT FILES ---
vcf_file <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/Boltonia_functional_snps.vcf.gz"
popmap_file <- "popmap.txt"  # 2 columns: sample <tab> population

# --- LOAD VCF ---
vcf <- read.vcfR(vcf_file)

# Extract genotype matrix
gt <- extract.gt(vcf, element = "GT", as.numeric = FALSE)
head(gt)

# Convert GT to allele counts (0, 1, 2)
gt_counts <- apply(gt, 2, function(x) {
  sapply(x, function(g) {
    if (is.na(g) || g %in% c("./.", ".")) return(NA)
    alleles <- unlist(strsplit(g, "[/|]"))
    sum(as.numeric(alleles))
  })
})
gt_counts <- t(gt_counts)  # individuals in rows

# Calculate allele frequencies per SNP
allele_freqs <- apply(gt_counts, 2, function(geno) {
  allele_sum <- sum(geno, na.rm = TRUE)
  total_alleles <- 2 * sum(!is.na(geno))
  freq_alt <- allele_sum / total_alleles
  freq_ref <- 1 - freq_alt
  c(ref = freq_ref, alt = freq_alt)
})

# Determine minor allele per site
minor_allele_is_alt <- allele_freqs["alt", ] <= allele_freqs["ref", ]

# For each SNP, count minor alleles per individual
minor_allele_matrix <- mapply(function(col, is_alt_minor) {
  if (is_alt_minor) {
    return(gt_counts[, col])  # alt allele count is minor
  } else {
    return(2 - gt_counts[, col])  # ref allele count is minor
  }
}, col = seq_along(minor_allele_is_alt), is_alt_minor = minor_allele_is_alt)



minor_allele_matrix <- as.data.frame(t(minor_allele_matrix))
rownames(minor_allele_matrix) <- colnames(gt_counts)
colnames(minor_allele_matrix) <- rownames(gt_counts)

minor_allele_matrix[1:10, 1:10]

# Total genetic load per individual (sum of minor alleles)
genetic_load <- rowSums(minor_allele_matrix, na.rm = TRUE)
head(genetic_load)

genetic_load_df <- tibble(sample = names(genetic_load),
                          genetic_load = genetic_load)

head(genetic_load_df)


Realized_load <- t(minor_allele_matrix)%>%
  as_tibble()%>%
  summarize_all(.funs = function(x){sum(x)/(2 * (sum(x != 0)))})%>%
  pivot_longer(cols = everything(), names_to = "Sample", values_to = "Realized_load")


Realized_load
class(Realized_load)

# --- LOAD POPMAP ---

popmap <- read_csv("./data/Boltonia_merged_data_20240925.csv") %>%
  mutate(Adapted_Longitude = case_when(is.na(Longitude) ~ Google_longitude, TRUE ~ Longitude), Adapted_Latitude = case_when(is.na(Latitude) ~ Google_latitude, TRUE ~ Latitude))%>%
  dplyr::select(Sample_Name = index, County, Population = MaternalLine, Adapted_Longitude, Adapted_Latitude) %>% 
  mutate(Sample_Name = paste("Boltonia", str_pad(Sample_Name, 3, pad = "0"), sep = "_"))


# Merge and summarize by population
genetic_load_merged <- left_join(Realized_load, popmap, by = c("Sample" = "Sample_Name"))

load_summary <- genetic_load_merged %>%
  group_by(County, Population) %>%
  summarise(mean_load = mean(genetic_load),
            sd_load = sd(genetic_load),
            n = n(),
            se_load = sd_load / sqrt(n))

# --- OUTPUT ---
write.csv(genetic_load_merged, "genetic_load_per_individual.csv", row.names = FALSE)
write.csv(load_summary, "genetic_load_per_population.csv", row.names = FALSE)

# --- OPTIONAL: PLOT ---
ggplot(genetic_load_merged, aes(x = Population, y = Realized_load)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Genetic Load per Population (Functional SNPs)",
       y = "Minor Allele Count (Load)", x = "Population") +
  ggh4x::facet_nested(.~reorder(County, Adapted_Latitude), scales = "free_x",  space = "free_x")+
  theme_bw()

ggsave("General_genetic_load_per_population.png", width = 12, height = 5, dpi = 600)


write_tsv(genetic_load_merged%>% select(Sample, Population), col_names = FALSE, "popmap.txt")
length(unique(genetic_load_merged$Population))
