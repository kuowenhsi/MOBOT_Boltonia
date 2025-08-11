library(treedataverse)
library(readxl)
library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

mis_ID <- read_csv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/Boltonia_asteroides_cass.csv", col_names = FALSE) %>% pull(X1)

Sample_all <- read_csv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/PCA_all_dataset.csv")%>%
  pull(Sample_Name)

metadata <- read_xlsx("./data/DNA_stock_Boltonia.xlsx")[1:4] %>%
  left_join(read_csv("./data/Boltonia_merged_data_20240925.csv")[c(1,7:12)], by = "index")%>%
  mutate(Sample_Name = paste("Boltonia", str_pad(index, 3, "left","0"), sep = "_"))%>%
  mutate(Adapted_Longitude = case_when(is.na(Longitude) ~ Google_longitude, TRUE ~ Longitude), Adapted_Latitude = case_when(is.na(Latitude) ~ Google_latitude, TRUE ~ Latitude))%>%
  mutate(Sample_Species = case_when(index <= 468 ~ "B. decurrens", TRUE ~ MaternalLine))%>%
  mutate(Sample_Species = case_when(Sample_Name %in% mis_ID ~ "B. asteroides (sympatric)", TRUE ~ Sample_Species))%>%
  select(Sample_Name,Sample_Species, everything())%>%
  filter(Sample_Name %in% Sample_all)


