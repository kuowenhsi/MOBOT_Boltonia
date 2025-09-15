library(treedataverse)
library(readxl)
library(writexl)
library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

mis_ID <- read_csv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Boltonia_asteroides_cass.csv", col_names = FALSE) %>% pull(X1)

all_fastq <- read_tsv("all_fastq_list.txt", col_names = FALSE)%>%
  select(Sample_Name = X1)%>%
  mutate(fastq = TRUE)

all_GVCF <- read_tsv("all_GVCF_list.txt", col_names = FALSE)%>%
  select(Sample_Name = X1)%>%
  mutate(Sample_Name = str_remove(Sample_Name, ".vcf.gz"),GVCF = TRUE)

metadata <- read_xlsx("./data/DNA_stock_Boltonia.xlsx")[1:4] %>%
  left_join(read_csv("./data/Boltonia_merged_data_20240925.csv")[-c(2:4)], by = "index")%>%
  select(index, MaternalLine, FlowerHead, Country, State, County, Latitude, Longitude, Google_latitude, Google_longitude,Locality, Location_Details = `Location Details`, PlantingDate, FirstLeafDate, Collection_Date = `Collection Date`, Collector)%>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))%>%
  mutate(Adapted_Longitude = case_when(is.na(Longitude) ~ Google_longitude, TRUE ~ Longitude), Adapted_Latitude = case_when(is.na(Latitude) ~ Google_latitude, TRUE ~ Latitude))%>%
  mutate(Sample_Species = case_when(index <= 468 ~ "B. decurrens", TRUE ~ MaternalLine))%>%
  mutate(Sample_Species = case_when(Sample_Name %in% mis_ID ~ "B. asteroides (sympatric)", TRUE ~ Sample_Species))%>%
  left_join(all_fastq, by = "Sample_Name")%>%
  left_join(all_GVCF, by = "Sample_Name")%>%
  mutate(Pop = paste0(Locality, str_split_i(Location_Details, ",", 1)))%>%
  mutate(Pop = str_remove(Pop, "NA"))%>%
  mutate(Pop = case_when(str_starts(Pop, "Cooper") ~ "Cooper Park", TRUE ~ Pop))%>%
  mutate(Collection_Date = as.Date(Collection_Date, format = "%d %b %Y"))%>%
  mutate(Pop = paste0(Pop, " (", year(Collection_Date), ")"))%>%
  select(Sample_Name, Pop, Sample_Species, fastq, GVCF,everything())

metadata_summary <- metadata %>%
  group_by(Pop, MaternalLine, County, Locality, Location_Details, Collection_Date, Collector)%>%
  summarize(Adapted_Longitude = mean(Adapted_Longitude), Adapted_Latitude = mean(Adapted_Latitude), n=n())

year(metadata_summary$Collection_Date)

write_xlsx(metadata, "Boltonia_all_metadata_20250815.xlsx")
