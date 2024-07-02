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
  colnames()
boltonia_data%>%select(starts_with("numFlwrBud"))%>%
  str()
boltonia_data%>%select(starts_with("flwrBud"))%>%
  str()

print(boltonia_data$flwrBud.2)


# correct the dataset
boltonia_data_corrected <- boltonia_data %>%
  mutate(flwrBud.2 = case_when(is.na(flwrBud.2) ~ as.numeric(NA),
                               !is.na(as.numeric(flwrBud.2)) ~ as.numeric(flwrBud.2),
                               TRUE ~ 1))


# check the dataset again
boltonia_data_corrected%>%select(starts_with("flwrBud"))%>%
  str()

print(boltonia_data_corrected$flwrBud.2)






boltonia_data_sub_1 <- boltonia_data %>%
  select(label, index, starts_with("stemLength"), starts_with("leafLong"), starts_with("leafWide"), starts_with("numStem"),
         starts_with("numFlwrBud"), starts_with("numRos"))%>%
  mutate_at(vars(matches("leafLong"), matches("stemLength"), matches("leafWide"), matches("numStem"),
                 matches("numFlwrBud"), matches("numRos")), .funs = "as.double")%>%
  pivot_longer(cols = c(matches("leafLong"), matches("stemLength"), matches("leafWide"), matches("numStem"),
                        matches("numFlwrBud"), matches("numRos")), values_to = "values")%>%
  arrange(index, name)%>%
  separate(col = "name", into = c("variable_name", "Date_index"), sep = "[.]")%>%
  left_join(date_info, by = "Date_index")
