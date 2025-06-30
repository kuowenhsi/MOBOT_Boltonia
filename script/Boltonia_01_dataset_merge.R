library(tidyverse)
library(writexl)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

list.files()
list.files(path = "./data")

pheno_data <- read_csv("./data/Boltonia_Phenotype_data_20240925.csv", na = c("", "NA", "N/A", "DNR")) %>%
  rename(latBuds.1 = `# latBuds.1`) %>%
  select(-ends_with(".12"))
  
flower_data <- read_csv("./data/Boltonia_Phenotype_data_20240925_flower.csv", na = c("", "NA", "N/A", "DNR"))

pheno_data_date <- pheno_data %>%
  select(starts_with("Date."))%>%
  .[1,] %>%
  mutate_all(function(x){str_replace(x, "/24$", "/2024")})%>%
  mutate_all(function(x){as.Date(x, "%m/%d/%Y")}) %>%
  mutate_all(function(x){as.numeric(x)})%>%
  pivot_longer(cols = everything(), names_prefix = "Date.", values_to = "Date_value", names_to = "Date_index")
  
pheno_data_vector <- pheno_data_date$Date_value
names(pheno_data_vector) <- pheno_data_date$Date_index

flower_data_date <- flower_data %>%
  select(starts_with("Date."))%>%
  .[1,] %>%
  mutate_all(function(x){str_replace(x, "/24$", "/2024")})%>%
  mutate_all(function(x){as.Date(x, "%m/%d/%Y")}) %>%
  mutate_all(function(x){as.numeric(x)})%>%
  pivot_longer(cols = everything(), names_prefix = "Date.", values_to = "Date_value", names_to = "Date_index")

flower_data_vector <- flower_data_date$Date_value
names(flower_data_vector) <- flower_data_date$Date_index


append_date_to_colname <- function(x, date_vector){
  date_index <- sapply(x, function(x){str_split_i(x, "[.]", 2)})
  date_value <- date_vector[date_index]
  trait_name <- sapply(x, function(x){str_split_i(x, "[.]", 1)})
  return(paste(trait_name, date_value, sep = "_"))
} 

append_date_to_colname(c("Date.13", "trait.2"), pheno_data_vector)

pheno_data_renamed <- pheno_data %>%
  select(-starts_with(c("Date.", "firstFlwr.", "index2"))) %>%
  rename_with(function(x){append_date_to_colname(x, pheno_data_vector)}, .cols = 3:ncol(.))

## only merge NOT duplicated data

flower_data_vector_NOT_duplicated <- flower_data_vector[!(flower_data_vector %in% pheno_data_vector)]%>%
  as.character()

flower_data_renamed <- flower_data %>%
  select(-starts_with(c("Date.", "firstFlwr.", "index2"))) %>%
  rename_with(function(x){append_date_to_colname(x, flower_data_vector)}, .cols = 3:ncol(.))%>%
  select(1, 2, ends_with(flower_data_vector_NOT_duplicated))

pheno_data_renamed_merged <- pheno_data_renamed %>%
  left_join(flower_data_renamed, by = c("label", "index"))%>%
  select(1, 2, sort(colnames(.)[c(-1, -2)]))


mid_data <- read_csv("./data/Boltonia_tag_merged.csv")%>%
  select(TemporaryID, MaternalLine, PlantingDate, FirstLeafDate, TransplantDate)%>%
  separate(col = MaternalLine, into = c("MaternalLine", "FlowerHead"), sep = " ")%>%
  bind_rows(tibble(TemporaryID = "8-2-11", MaternalLine = "2011-2659-1", FlowerHead = "3C", PlantingDate = "2/29/24", FirstLeafDate = NA, TransplantDate = "4/12/24"))

LCMS_data <- read_csv("./data/Boltonia_LCMS_20240624.csv")%>%
  mutate(MaternalLine = str_c(Accession, Planting, sep = "-"))%>%
  select(-Accession, -Planting)%>%
  select(MaternalLine, everything())

merged_data <- pheno_data_renamed_merged %>%
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
  select("index", "label","MaternalLine", "FlowerHead", "Id","Country","State","County", "Latitude", "Longitude", "Google_latitude", "Google_longitude", everything())%>%
  mutate(Latitude = case_when(is.na(Latitude) ~ as.numeric(Google_latitude), TRUE ~ Latitude), Longitude = case_when(is.na(Longitude) ~ as.numeric(Google_longitude), TRUE ~ Longitude))%>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, pad = "0"), sep = "_"))%>%
  select(Sample_Name, everything())%>%
  arrange(Sample_Name)

colnames(merged_data)
write_csv(merged_data_google, "./data/Boltonia_merged_data_20250626.csv")
write_xlsx(merged_data_google, "./data/Boltonia_merged_data_20250626.xlsx")
