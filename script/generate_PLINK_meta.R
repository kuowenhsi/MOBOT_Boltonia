library(tidyverse)


setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

input_psam <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/alloutgroup/VCF_decurrens/Boltonia_decurrens_100kb0.8.psam")

Boltonia_Flower <- read_csv("./data/Boltonia_merged_data_tidy_20240925.csv")%>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, pad = "0"), sep = "_"))%>%
  filter(num_traits == "numDiscF")%>%
  group_by(Sample_Name, MaternalLine, County)%>%
  summarize(Cul_Flowers = sum(num_values, na.rm = TRUE))%>%
  mutate(Flowered_2024 = case_when(Cul_Flowers == 0 ~ 1, TRUE ~ 2))%>%
  mutate(Cul_Flowers = case_when(Cul_Flowers == 0 ~ "NA", TRUE ~ as.character(Cul_Flowers)))%>%
  mutate(MaternalLine = paste0("M", MaternalLine), County = str_replace_all(County, " ", "_"))

Boltonia_Stem <- read_csv("./data/Boltonia_stemLength_data_20240925.csv")%>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, pad = "0"), sep = "_"))%>%
  select(Sample_Name, Stem_Length = mean_stemLength)

Boltonia_Climate <- read_csv("./data/Boltonia_buf_climate_data_20250421.csv")%>%
  select(-2, -3)

Boltonia_Variance <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/alloutgroup/VCF_decurrens/Boltonia_decurrens_100kb0.8_pca.eigenval", col_names = "Variance")%>%
  mutate(Variance_Percent = Variance/sum(Variance), PC = seq(n()))

ggplot(data = filter(Boltonia_Variance, PC < 11), aes(x = PC, y = Variance_Percent))+
  geom_point()+
  geom_line()


Boltonia_PCA <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/alloutgroup/VCF_decurrens/Boltonia_decurrens_100kb0.8_pca.eigenvec")%>%
  select(Sample_Name = `#IID`, PC1, PC2, PC3, PC4)

Boltonia_meta <- Boltonia_PCA %>%
  left_join(Boltonia_Flower, by = "Sample_Name")%>%
  left_join(Boltonia_Stem, by = "Sample_Name")%>%
  left_join(Boltonia_Climate, by = "Sample_Name")%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))

output_psam <- input_psam %>%
  left_join(Boltonia_meta, by = c("#IID" = "Sample_Name"))

write_tsv(output_psam, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-MissouriBotanicalGarden/General - IMLS National Leadership Grant 2023/Genotyping/Boltonia/alloutgroup/VCF_decurrens/Boltonia_decurrens_100kb0.8_meta.psam")

