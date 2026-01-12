library(tidyverse)


setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

input_psam <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/PLINK_meta/Boltonia_decurrens_imputed_Low_LD_maf.psam")

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx", na = c("NA", "", "NA (NA)"))%>%
  mutate(Pop = case_when(Pop == "Cooper Park (1995)" ~ "Cooper Park (2000)", TRUE ~ Pop))%>%
  mutate(Flowered_2024 = case_when(is.na(FlowerDays.2024) ~ 1, TRUE ~ 2))

Boltonia_Pop_ABCD <- tibble(Pop_Index = c("Pop_01", "Pop_02", "Pop_03", "Pop_04", "Pop_05", 
                                          "Pop_06", "Pop_07", "Pop_08", "Pop_09", "Pop_10", 
                                          "Pop_11", "Pop_12", "Pop_13", "Pop_14", "Pop_15", "Pop_16", "Pop_17" ),
                            Pop_ABCD = c("A", "A", "A", "B", "C",
                                         "C", "C", "C", "C", "D",
                                         "D", "E", "E", "E", "F", "F", "F"))


Boltonia_Variance <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/PCA_decurrens/Boltonia_decurrens_imputed_Low_LD_pca.eigenval", col_names = "Variance")%>%
  mutate(Variance_Percent = Variance/sum(Variance), PC = seq(n()))

ggplot(data = filter(Boltonia_Variance, PC < 11), aes(x = PC, y = Variance_Percent))+
  geom_point()+
  geom_line()


Boltonia_PCA <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/PCA_decurrens/Boltonia_decurrens_imputed_Low_LD_pca.eigenvec")%>%
  select(Sample_Name = `#IID`, PC1, PC2,PC3,PC4)

Boltonia_meta <- Boltonia_metadata[,c(1,26:34)] %>%
  left_join(Boltonia_PCA, by = "Sample_Name")%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))%>%
  select(c(-2, -5))

output_psam <- input_psam %>%
  left_join(Boltonia_meta, by = c("#IID" = "Sample_Name"))%>%
  select(`#IID`, SEX, PC1, PC2, PC3, PC4, everything())

write_tsv(output_psam, "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/PLINK_meta/Boltonia_decurrens_imputed_metapheno.psam")

