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
  select("index", "label","MaternalLine", "FlowerHead", "Id","Country","State","County", "Latitude", "Longitude","Locality","Location Details","PlantingDate","FirstLeafDate","TransplantDate", everything())%>%
  arrange(MaternalLine)

Google_Sage <- read_csv("./data/Merged_Boltonia_data_20240626_Sage.csv")%>%
  filter(!is.na(MaternalLine))%>%
  select(MaternalLine, Google_cood)%>%
  group_by(MaternalLine)%>%
  summarize(Google_cood = unique(Google_cood))
  
merged_data_google <- merged_data %>%
  left_join(Google_Sage, by = "MaternalLine")%>%
  separate(col = Google_cood, into = c("Google_latitude", "Google_longitude"), sep = ",")%>%
  select("index", "label","MaternalLine", "FlowerHead", "Id","Country","State","County", "Latitude", "Longitude", "Google_latitude", "Google_longitude", everything())

colnames(merged_data)
write_csv(merged_data, "./data/Boltonia_merged_data_20240627.xlsx")
write_xlsx(merged_data, "./data/Boltonia_merged_data_20240627.xlsx")
