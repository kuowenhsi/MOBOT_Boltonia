# This is for Wen

library(tidyverse)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")


correct_stem <- function(x){
  stem_present <- FALSE
  output <- as.vector(rep(NA, length(x)), mode = "numeric")
  
  for (i in 1:length(x)){
    if (stem_present == FALSE){
      if (is.na(x[[i]])){
        stem_present <- FALSE
      } else{
        stem_present <- TRUE
        output[[i]] <- x[[i]]
      } 
    } else{
      if (is.na(x[[i]])) {
        stem_present <- FALSE
        output[1:i] <- as.numeric(rep(NA, i))
      } else{
        stem_present <- TRUE
        output[[i]] <- x[[i]]
      }
    }
  }
  return(output)
}



list.files()

boltonia_data <- read_csv("./data/Boltonia Phenotype data_20240612.csv", na = c("", "NA", "N/A", "DNR"))
str(boltonia_data)
boltonia_data$flwrBud.1

# does plant grow with time?

p <- ggplot(data = boltonia_data, mapping = aes(x = label, y = stemLength.1))+
  geom_col(mapping = aes(fill = stemLength.1))+
  scale_fill_distiller(palette = "RdYlBu")
p

convert_Y_TRUE <- function(x){
  output <- case_when(x == "Y" ~ TRUE, TRUE ~ FALSE)
  return(output)
}


boltonia_data_sub <- boltonia_data %>%
  filter(index <= 10) %>%
  mutate(stemLength.2 = as.numeric(stemLength.2), label = factor(label, levels = label))%>%
  # mutate(Surv.2 = case_when(Surv.2 == "Y" ~ TRUE, TRUE ~ FALSE))%>%
  mutate_at(vars(matches("Surv.")), .funs = "convert_Y_TRUE")
## modify the mutate all Surv.X columns to logical data

str(boltonia_data_sub)

date_info <- boltonia_data_sub %>%
  select(str_c("Date.", 1:8))%>% # add something
  .[1,]%>%
  pivot_longer(cols = starts_with("Date"), names_to = "Date_index", values_to = "Date")%>%
  mutate(Date_index = str_remove(Date_index, "Date."))%>%
  mutate(Date = str_replace(Date, pattern = "/24$", replacement = "/2024"))

boltonia_data_sub_l <- boltonia_data_sub %>%
  select(label, index, starts_with("stemLength"), starts_with("leafLong"), starts_with("leafWide"))%>%
  mutate_at(vars(matches("leafLong")),.funs = "as.double")%>%
  mutate_at(vars(matches("leafWide")),.funs = "as.double")%>%
  pivot_longer(cols = c(starts_with("leafLong"), starts_with("stemLength"), starts_with("leafWide")), values_to = "values")%>%
  separate(col = "name", into = c("variable_name", "Date_index"), sep = "[.]")%>%
  left_join(date_info, by = "Date_index")%>%
  mutate(Date = as.Date(Date, "%m/%d/%Y"))%>%
  group_by(label, index, variable_name)%>%
  mutate(values = correct_stem(values))

p <- ggplot(data = filter(boltonia_data_sub_l, variable_name == "stemLength"), aes(x = Date, y = values))+
  geom_point()+
  geom_line(aes(group = label), color = "gray85")+
  geom_boxplot(aes(group = Date), fill = NA, color = "gray65", width = 0.5)+
  ggtitle("Stem length")+
  theme_bw()
p

p <- ggplot(data = filter(boltonia_data_sub_l, variable_name == "leafLong"), aes(x = Date, y = values))+
  geom_point()+
  geom_line(aes(group = label), color = "gray85")+
  geom_boxplot(aes(group = Date), fill = NA, color = "gray65", width = 0.5)+
  scale_x_date(breaks = unique(boltonia_data_sub_l$Date), labels = c("W1", "W2", "W3", "W4", "W5"))+
  ggtitle("Leaf length")+
  theme_bw()
p

p <- ggplot(data = filter(boltonia_data_sub_l, variable_name == "leafWide"), aes(x = Date, y = values))+
  geom_boxplot(aes(group = Date), fill = NA, color = "pink", width = 0.5)+
  geom_line(aes(group = label), color = "gray85")+
  geom_point()+
  ggtitle("Leaf Wide")+
  theme_bw()
p

#########






