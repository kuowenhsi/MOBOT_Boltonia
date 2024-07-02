library(tidyverse)

setwd("C:/Users/nyanw/Downloads/REU/MOBOT_Boltonia")

boltonia_data <- read_csv("gps_cood_20240627.csv", na = c(" ", "NA", "N/A", "DNR"))

boltonia_data
str(boltonia_data)

date_info <- boltonia_data %>%
  select(str_c("Date.", 1:8))%>%
  .[1,]%>%
  pivot_longer(cols = starts_with("Date"), names_to = "Date_index", values_to = "Date")%>%
  mutate(Date_index = str_remove(Date_index, "Date."))%>%
  mutate(Date = str_replace(Date, pattern = "/24$", replacement = "/2024"))

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
  mutate(flwrBud.2 = case_when(is.na(flwrBud.2) ~ as.numeric(NA),
                               !is.na(as.numeric(flwrBud.2)) ~ as.numeric(flwrBud.2),
                               TRUE ~ 1))%>%
  mutate(flwrBud.1 = case_when(is.na(flwrBud.1) ~ as.numeric(NA),
                               flwrBud.1 == "Y" ~ 1,
                               TRUE ~ 0))%>%
  rename(numFlwrB.1 = numFlwrBud.1, numFlwrB.2 = flwrBud.2)%>%
  select(-flwrBud.1)

boltonia_data_corrected$flwrBud.1
boltonia_data_corrected$flwrBud.2

# check the dataset again
boltonia_data_corrected%>%select(starts_with("flwrBud"))%>%
  str()
boltonia_data_corrected%>%select(starts_with("numFlwrB"))%>%
  str()
boltonia_data$flwrBud.1
boltonia_data_corrected%>%
  str()



boltonia_data_sub_1 <- boltonia_data_corrected %>%
  select(label, index, starts_with("stemLength"), starts_with("leafLong"), starts_with("leafWide"), 
         starts_with("numStem"), starts_with("numFlwrB"), starts_with("numRos"), starts_with("numLvs"), 
         starts_with("numLSt"), starts_with("numRayF"), starts_with("numDiscF"))%>%
  mutate_at(vars(starts_with("stemLength"), starts_with("leafLong"), starts_with("leafWide"), 
                 starts_with("numStem"), starts_with("numFlwrB"), starts_with("numRos"), starts_with("numLvs"), 
                 starts_with("numLSt"), starts_with("numRayF"), starts_with("numDiscF")), .funs = "as.double")%>%
  pivot_longer(cols = c(starts_with("stemLength"), starts_with("leafLong"), starts_with("leafWide"), 
                        starts_with("numStem"), starts_with("numFlwrB"), starts_with("numRos"), starts_with("numLvs"), 
                        starts_with("numLSt"), starts_with("numRayF"), starts_with("numDiscF")), values_to = "values")%>%
  arrange(index, name)%>%
  separate(col = "name", into = c("variable_name", "Date_index"), sep = "[.]")%>%
  left_join(date_info, by = "Date_index")%>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"))


# check the result

unique(boltonia_data_sub_1$variable_name)
unique(boltonia_data_sub_1$label)
unique(boltonia_data_sub_1$Date_index)
unique(boltonia_data_sub_1$Date)

write_csv(boltonia_data_sub_1, "tidy_boltonia_data.csv")
