library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

boltonia_data <- read_csv("./data/Boltonia_merged_data_20240925.csv")

boltonia_data
str(boltonia_data)

# date_info <- boltonia_data %>%
#   select(str_c("Date.", 1:8))%>%
#   .[1,]%>%
#   pivot_longer(cols = starts_with("Date"), names_to = "Date_index", values_to = "Date")%>%
#   mutate(Date_index = str_remove(Date_index, "Date."))%>%
#   mutate(Date = str_replace(Date, pattern = "/24$", replacement = "/2024"))

# check the dataset

boltonia_data %>% select(starts_with("stemLength"))%>%
  str()
boltonia_data%>%select(starts_with("numFlwrBud"))%>%
  str()
boltonia_data%>%select(starts_with("flwrBud"))%>%
  str()
boltonia_data%>%select(starts_with("numFlwrB"))%>%
  str()
boltonia_data%>%
  str()


# correct the dataset
boltonia_data_corrected <- boltonia_data %>%
  mutate(flwrBud_19844 = case_when(is.na(flwrBud_19844) ~ as.numeric(NA),
                               !is.na(as.numeric(flwrBud_19844)) ~ as.numeric(flwrBud_19844),
                               TRUE ~ 1))%>%
  mutate(flwrBud_19851 = case_when(is.na(flwrBud_19851) ~ as.numeric(NA),
                                   flwrBud_19851 == "Y" ~ 1,
                               TRUE ~ 0))%>%
  rename(numFlwrB_19844 = numFlwrBud_19844, numFlwrB_19851 = flwrBud_19851)%>%
  select(-flwrBud_19844)%>%
  select(1:17, sort(colnames(.)[18:178]), everything())
  

boltonia_data_corrected$numFlwrB_19844
boltonia_data_corrected$numFlwrB_19851

# check the dataset again
boltonia_data_corrected%>%select(starts_with("flwrBud"))%>%
  str()
boltonia_data_corrected%>%select(starts_with("numFlwrB"))%>%
  str()
boltonia_data_corrected%>%
  str()

colnames(boltonia_data)
colnames(boltonia_data_corrected)

traits <- colnames(boltonia_data_corrected)[18:236]
unique_traits <- unique(str_split_i(traits, "_", 1))
unique_traits
num_uniq_traits <- unique_traits[!(unique_traits %in% c("notes", "Surv"))]
num_uniq_traits

chr_uniq_traits <- unique_traits[(unique_traits %in% c("notes", "Surv"))]
chr_uniq_traits

boltonia_data_1_num <- boltonia_data_corrected %>%
  select(1:17, starts_with(num_uniq_traits))%>%
  pivot_longer(cols = starts_with(num_uniq_traits), names_sep = "_", names_to = c("num_traits", "Date"),values_to = "num_values")%>%
  arrange(index, num_traits, Date)%>%
  mutate(Date = as.Date(as.integer(Date)))

boltonia_data_1_chr <- boltonia_data_corrected %>%
  select(1, matches(paste0(chr_uniq_traits, "_")))%>%
  pivot_longer(cols = starts_with(chr_uniq_traits), names_sep = "_", names_to = c("chr_traits", "Date"),values_to = "chr_values")%>%
  arrange(index, chr_traits, Date)%>%
  mutate(Date = as.Date(as.integer(Date)))%>%
  pivot_wider(names_from = chr_traits, values_from = chr_values)

boltonia_data_1 <- boltonia_data_1_num %>%
  left_join(boltonia_data_1_chr, by = c("index", "Date"))


# check the result
unique(boltonia_data_1$label)
unique(boltonia_data_1$Date)

write_csv(boltonia_data_1, "./data/Boltonia_merged_data_tidy_20240925.csv")
