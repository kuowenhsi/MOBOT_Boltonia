library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

replace_126_127 <- function(x){
  case_when(x == "Boltonia_126" ~ "Boltonia_127", x == "Boltonia_127" ~ "Boltonia_126", TRUE ~ x)
}

Boltonia_metadata <- readxl::read_excel("Boltonia_all_metadata_20250815.xlsx", na = c("NA", "", "NA (NA)"))%>%
  mutate(Pop = case_when(Pop == "Cooper Park (1995)" ~ "Cooper Park (2000)", TRUE ~ Pop))%>%
  mutate(Sample_Name = replace_126_127(Sample_Name))

Boltonia_metadata_Pop_Index <- Boltonia_metadata %>%
  group_by(Pop)%>%
  summarize(Adapted_Latitude = mean(Adapted_Latitude))%>%
  ungroup()%>%
  drop_na()%>%
  arrange(Adapted_Latitude, Pop)%>%
  mutate(Pop_Index = paste("Pop", str_pad(1:n(), width = 2, pad = "0"), sep = "_"))%>%
  select(Pop, Pop_Index,Adapted_Latitude)

Boltonia_metadata <- Boltonia_metadata %>%
  left_join(select(Boltonia_metadata_Pop_Index, 1:2), by = "Pop") %>%
  select(Sample_Name, Pop, Pop_Index, everything())

for (i in Boltonia_metadata_Pop_Index$Pop_Index){
  Pop_Index_Samples <- Boltonia_metadata %>%
    filter(!is.na(GVCF), Sample_Species == "B. decurrens")%>%
    filter(Pop_Index == i)%>%
    arrange(FlowerHead)%>%
    group_by(FlowerHead) %>%
    slice_sample(n = 1) %>%
    ungroup()%>%
    select(Sample_Name)
  
  print(i)
  print(length(Pop_Index_Samples$Sample_Name))
  # write_tsv(Pop_Index_Samples, paste("./data/GONE/Boltonia", i, "Sample_List.txt", sep = "_"),col_names = FALSE)
}

Boltonia_Pop_07_decurrens_Sample_List <-Boltonia_metadata %>%
  filter(Pop_Index == "Pop_07", Sample_Species == "B. decurrens")%>%
  arrange(FlowerHead)%>%
  group_by(FlowerHead) %>%
  slice_sample(n = 1) %>%
  ungroup()%>%
  select(Sample_Name)

write_tsv(Boltonia_Pop_07_decurrens_Sample_List, "./data/GONE/Boltonia_Pop_07_decurrens_Sample_List.txt",col_names = FALSE)
  
Boltonia_Pop_07_asteroides_Sample_List <-Boltonia_metadata %>%
  filter(Pop_Index == "Pop_07", Sample_Species == "B. asteroides (sympatric)")%>%
  arrange(FlowerHead)%>%
  group_by(FlowerHead) %>%
  slice_sample(n = 1) %>%
  ungroup()%>%
  select(Sample_Name)

write_tsv(Boltonia_Pop_07_asteroides_Sample_List, "./data/GONE/Boltonia_Pop_07_asteroides_Sample_List.txt",col_names = FALSE)

Boltonia_All_Sample_List <- Boltonia_metadata %>%
  filter(Sample_Species == "B. decurrens", !is.na(GVCF))%>%
  group_by(Pop, Pop_Index,FlowerHead)%>%
  slice_sample(n = 1) %>%
  ungroup()%>%
  select(Sample_Name)

write_tsv(Boltonia_All_Sample_List, "./data/GONE/Boltonia_All_Sample_List.txt",col_names = FALSE)


# install.packages(c("data.table", "stringr", "dplyr"))  # if needed

library(data.table)

# 1) Point to your folder
ne_dir <- "/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/GONE/GONE2_results"

# 2) Get only the Ne files
ne_files <- list.files(ne_dir, pattern = "_GONE2_Ne$", full.names = TRUE)

# 3) Read & combine; keep comments out, and add Pop & source file
ne_all <- rbindlist(
  lapply(ne_files, function(f) {
    dt <- fread(f, data.table = FALSE)
    pop_id <- str_match(basename(f), "Pop_(\\d+)")[,2]
    dt$Pop_Index <- paste("Pop", str_pad(pop_id, width = 2, pad = "0"), sep = "_")
    dt$file <- basename(f)
    dt
  }),
  use.names = TRUE, fill = TRUE
)

GONE_data <- ne_all %>%
  left_join(Boltonia_metadata_Pop_Index, by = "Pop_Index")

GONE_metapopulation <- read_tsv("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia/data/GONE/GONE2_results/GONE2_result_Pop_All_GONE2_Ne_mix", skip = 11)


p <-ggplot(data = GONE_data, aes(x = Generation, y = Ne_diploids))+
  geom_line(aes(color = Pop_Index))+
  scale_x_continuous(limits = c(0, 150))+
  theme_bw()+
  theme(legend.position = "none")

p

ggsave("./figures/GONE2_output/per_population.png", width = 4, height = 4, dpi = 600) 


p <-ggplot(data = GONE_metapopulation, aes(x = generation, y = Ne_metapop))+
  geom_line(linewidth = 2)+
  scale_x_continuous(limits = c(0, 150))+
  theme_bw()

p

ggsave("./figures/GONE2_output/meta_population.png", width = 4, height = 4, dpi = 600) 
