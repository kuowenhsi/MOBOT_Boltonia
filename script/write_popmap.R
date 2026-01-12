library(tidyverse)


setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

input_psam <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/PLINK_meta/Boltonia_decurrens_imputed_Low_LD_maf.psam")

Boltonia_Pop_ABCD <- tibble(Pop_Index = c("Pop_01", "Pop_02", "Pop_03", "Pop_04", "Pop_05", 
                                          "Pop_06", "Pop_07", "Pop_08", "Pop_09", "Pop_10", 
                                          "Pop_11", "Pop_12", "Pop_13", "Pop_14", "Pop_15", "Pop_16", "Pop_17" ),
                            Pop_ABCD = c("A", "A", "A", "B", "C",
                                         "C", "C", "C", "C", "D",
                                         "D", "E", "E", "E", "F", "F", "F"))


Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx", na = c("NA", "", "NA (NA)"))%>%
  mutate(Flowered_2024 = case_when(is.na(FlowerDays.2024) ~ 1, TRUE ~ 2))%>%
  left_join(Boltonia_Pop_ABCD, by = "Pop_Index")


asteroides_samples_SAMPLES <- read_tsv("asteroides_samples_SAMPLES.txt", col_names = "Sample_Name")%>%
  left_join(Boltonia_metadata, by = "Sample_Name")%>%
  filter(Pop_Index != "Pop_01")%>%
  filter(!(Sample_Name %in% c("Boltonia_253", "Boltonia_025","Boltonia_476", "Boltonia_477", "Boltonia_479", "Boltonia_483", "Boltonia_492")))%>% # Boltonia_253 and Boltonia_025 are two introgressed individuals from Pop_08
  mutate(IS_asteroides = str_detect(Sample_Species, "asteroides"))%>%
  select(Sample_Name, IS_asteroides)%>%
  mutate(Sample_Species = ifelse(IS_asteroides, "asteroides", "decurrens"))%>%
  select(Sample_Name, Sample_Species)

write_tsv(asteroides_samples_SAMPLES, "asteroides_samples_SAMPLES.tsv", col_names = FALSE)

popmap2 <- Boltonia_metadata %>%
  mutate(Sample_Name = replace_126_127(Sample_Name))%>%
  inner_join(input_psam, by = c("Sample_Name" = "#IID"))%>%
  select(Sample_Name, Pop_ABCD)
  
write_tsv(popmap2, "popmap2.tsv", col_names = FALSE)

mutate(Pop = case_when(Pop == "Cooper Park (1995)" ~ "Cooper Park (2000)", TRUE ~ Pop))%>%