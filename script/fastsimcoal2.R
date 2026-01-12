library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20251010.xlsx")
%>%
  select(Sample_Name, Pop_Index)%>%
  left_join(read_tsv("popmap2.tsv", col_names = c("Sample_Name", "Pop_Index2")), by = "Sample_Name")

SFS_popmap <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/fastsimcoal2/SFS_popmap.tsv", col_names = c("Sample_Name"))%>%
  left_join(Boltonia_metadata, by = "Sample_Name")%>%
  select(Sample_Name, Pop_Index,Pop_Index2)


write_tsv(SFS_popmap, "./data/fastsimcoal2/SFS_popmap_pop2.tsv", col_names = FALSE)

SFS_popmap_s <- SFS_popmap %>%
  group_by(Pop_Index, Pop_Index2)%>%
  summarize(count = n())
