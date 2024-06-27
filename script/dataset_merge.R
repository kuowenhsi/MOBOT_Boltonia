library(tidyverse)
library(writexl)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

list.files()
list.files(path = "./data")

pheno_data <- read_csv("./data/Boltonia_Phenotype_data_20240612.csv")

mid_data <- read_csv("./data/Boltonia_tag_merged.csv")%>%
  select(TemporaryID, MaternalLine, PlantingDate, FirstLeafDate, TransplantDate)%>%
  separate(col = MaternalLine, into = c("MaternalLine", "FlowerHead"), sep = " ")%>%
  bind_rows(tibble(TemporaryID = "8-2-11", MaternalLine = "2011-2659-1", FlowerHead = "3C", PlantingDate = "2/29/24", FirstLeafDate = NA, TransplantDate = "4/12/24"))

LCMS_data <- read_csv("./data/Boltonia_LCMS_20240624.csv")%>%
  mutate(MaternalLine = str_c(Accession, Planting, sep = "-"))%>%
  select(-Accession, -Planting)%>%
  select(MaternalLine, everything())

merged_data <- pheno_data %>%
  left_join(mid_data, by = c("label" = "TemporaryID"))%>%
  left_join(LCMS_data, by = "MaternalLine")%>%
  mutate(Google_cood = "")%>%
  select("index", "label","MaternalLine", "FlowerHead", "Id","Country","State","County", "Latitude", "Longitude","Google_cood","Locality","Location Details","PlantingDate","FirstLeafDate","TransplantDate", everything())%>%
  arrange(MaternalLine)

colnames(merged_data)

write_xlsx(merged_data, "./data/Boltonia_merged_data_20240627.xlsx")
