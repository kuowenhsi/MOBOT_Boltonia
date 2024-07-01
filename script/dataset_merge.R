## This is script from Sage

library(tidyverse)
library(writexl)

setwd("C:/Users/nyanw/Downloads/REU/MOBOT_Boltonia")

list.files()
list.files(path = "./data")

pheno_data <- read_csv("./data/Boltonia_Phenotype_data_20240612.csv")

mids_data <- read_csv("./data/Boltonia_tag_merged.csv")%>%
  select(TemporaryID, MaternalLine, PlantingDate, FirstLeafDate, TransplantDate)%>%
  separate(col = MaternalLine, into = c("MaternalLine", "FlowerHead"), sep = " ")%>%
  bind_rows(tibble(TemporaryID = "8-2-11", MaternalLine = "2011-2659-1", FlowerHead = "3C", 
                   PlantingDate = "2/29/24", FirstLeafDate = NA, TransplantDate = "4/12/24"))

LCMS_data <- read_csv("./data/Boltonia_LCMS_20240624.csv")%>%
  mutate(MaternalLine = str_c(Accession, Planting, sep = "-"))%>%
  select(-Accession, -Planting)%>%
  select(MaternalLine, everything())

merged_data <- pheno_data %>%
  left_join(mids_data, by = c("label" = "TemporaryID"))%>%
  left_join(LCMS_data, by = "MaternalLine")%>%
  select( "MaternalLine", "FlowerHead", "PlantingDate", "FirstLeafDate", "TransplantDate", "Id", 
          "Country", "State", "County", "Latitude", "Longitude", "Locality", "Location Details", everything())%>%
  arrange(MaternalLine)

gps_cood <- read.csv("./data/Merged_Boltonia_data_20240626.csv")%>%
  filter(!is.na(MaternalLine))%>%
  select(MaternalLine, Google_cood)%>%
  group_by(MaternalLine)%>%
  summarize(Google_cood = unique(Google_cood))

merged_data_gps <- merged_data %>%
  left_join(gps_cood, by = "MaternalLine")%>%
  separate(col = Google_cood, into = c("Google_Latitude", "Google_Longitude"), sep = ",")%>%
  select("index", "label", "MaternalLine", "FlowerHead", "PlantingDate", "FirstLeafDate", "TransplantDate", "Id", 
         "Country", "State", "County", "Latitude", "Longitude", "Google_Latitude", "Google_Longitude", everything())

colnames(merged_data)

write_xlsx(merged_data_gps, "gps_cood_20240627.xlsx")
write_csv(merged_data_gps, "gps_cood_20240627.csv")

write_xlsx(merged_data, "./data/Merged_Boltonia_data_20240627.csv")
