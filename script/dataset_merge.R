## This is script from Sage

library(tidyverse)
setwd("C:/Users/nyanw/Downloads/REU/MOBOT_Boltonia")

list.files()
list.files(path = "./data")

pheno_data <- read_csv("./data/Boltonia_Phenotype_data_20240612.csv")

mids_data <- read_csv("./data/Boltonia_tag_merged.csv")%>%
  select(TemporaryID, MaternalLine, PlantingDate, FirstLeafDate, TransplantDate)%>%
  separate(col = MaternalLine, into = c("MaternalLine", "FlowerHead"), sep = " ")

LCMS_data <- read_csv("./data/Boltonia_LCMS_20240624.csv")%>%
  mutate(MaternalLine = str_c(Accession, Planting, sep = "-"))%>%
  select(-Accession, -Planting)%>%
  select(MaternalLine, everything())

merged_data <- pheno_data %>%
  left_join(mids_data, by = c("label" = "TemporaryID"))%>%
  left_join(LCMS_data, by = "MaternalLine")%>%
  filter(is.na(Id))
