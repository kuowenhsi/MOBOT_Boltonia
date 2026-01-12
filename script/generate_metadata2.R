library(readxl)
library(writexl)
library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20250815.xlsx", na = c("NA", "", "NA (NA)"))%>%
  mutate(PlantingDate = as.Date(PlantingDate, "%m/%d/%y"), FirstLeafDate = as.Date(FirstLeafDate, "%m/%d/%y"))%>%
  mutate(Pop = case_when(Pop == "Cooper Park (1995)" ~ "Cooper Park (2000)", TRUE ~ Pop))

Boltonia_metadata_Pop_Index <- Boltonia_metadata %>%
  group_by(Pop)%>%
  summarize(Adapted_Latitude = mean(Adapted_Latitude), Adapted_Longitude = mean(Adapted_Longitude))%>%
  ungroup()%>%
  drop_na()%>%
  arrange(Adapted_Latitude, Pop)%>%
  mutate(Pop_Index = paste("Pop", str_pad(1:n(), width = 2, pad = "0"), sep = "_"))%>%
  mutate(Pop_Name = paste(1:n(), Pop, sep = "-"))%>%
  select(Pop_Name, Pop, Pop_Index ,Adapted_Latitude, Adapted_Longitude)

# write_csv(Boltonia_metadata_Pop_Index, "./data/Boltonia_metadata_Pop_Index.csv")

Boltonia_metadata <- Boltonia_metadata %>%
  left_join(select(Boltonia_metadata_Pop_Index, 1:3), by = "Pop")%>%
  select(Sample_Name, Pop, Pop_Index, everything())

Boltonia_pheno_2025 <- read_excel("./data/BoltoniaPhenotypeData_2025.xlsx", na = c("NA", "", "NA (NA)"))[,2:7]%>%
  select(Sample_Name, Disc.Flower.Date.2025 = Disc.Flower.Date, Stem.Length.2025 = Stem.Length, Num.Stems.2025 = Num.Stems)

Boltonia_pheno_2024 <- read_csv("./data/Boltonia_merged_data_tidy_20251010.csv")

Boltonia_FirstFloweringDate_2024 <- read_csv("./data/Boltonia_FirstFloweringDate_2024.csv")%>%
  select(-PlantingDate, -FirstLeafDate)

Boltonia_StemLength_2024 <- read_csv("./data/Boltonia_stemLength_data_20251010.csv")%>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, pad = "0"), sep = "_"))%>%
  select(Sample_Name, Stem.Length.2024 = median_stemLength)

Boltonia_metadata_pheno <- Boltonia_metadata %>%
  left_join(Boltonia_pheno_2025, by = "Sample_Name") %>%
  left_join(Boltonia_FirstFloweringDate_2024, by = "Sample_Name") %>%
  left_join(Boltonia_StemLength_2024, by = "Sample_Name")%>%
  mutate(Disc.Flower.Date.2025 = as.Date(Disc.Flower.Date.2025))%>%
  mutate(FlowerDays.2024 = as.numeric(Disc.Flower.Date.2024 -PlantingDate),
         FlowerDays.2025 = as.numeric(Disc.Flower.Date.2025 -PlantingDate))%>%
  rowwise()%>%
  mutate(FlowerDays.total = min(c(FlowerDays.2024, FlowerDays.2025), na.rm = TRUE))%>%
  ungroup()%>%
  mutate(FlowerDays.total = ifelse(FlowerDays.total == Inf, as.numeric(NA), FlowerDays.total))
str(Boltonia_metadata_pheno)


write_xlsx(Boltonia_metadata_pheno, "Boltonia_all_metadata_20251010.xlsx")
