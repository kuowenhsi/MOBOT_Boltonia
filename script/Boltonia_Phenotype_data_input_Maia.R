# This is for Maia

library(tidyverse)

setwd("C:/Users/maial/Downloads/MBG REU")

list.files()

bolt_data <- read_csv("Boltonia_Phenotype_data_20240610_Maia_Washington.csv")

class(NA)

class(bolt_data)

str(bolt_data)

install.packages("ggplot2")
library(ggplot2)

p <- ggplot(data = bolt_data, mapping = aes(x = label, y = stemLength.1))+ geom_col(mapping = aes(fill = stemLength.1))+
  scale_fill_distiller(palette = "PiYG")

p


############################################################

bolt_data$stemLength.2
bolt_data$stemLength.2[1:10]

bolt_data_50$stemLength.2 <- as.numeric(bolt_data_50$stemLength.2)

bolt_data_50 <- bolt_data %>% 
  filter(index <= 10) %>% 
  mutate(stemLength.2 = as.numeric(stemLength.2), label = factor(label, levels = label))

class(bolt_data_50$label)

bolt_data_50$label

bolt_data_50$label[5]

bolt_data_50$label[[5]] <- "5-2-9"

bolt_data_50$label

date_info <- bolt_data_50 %>% 
  select(str_c("Date.", 1:5)) %>% .[1,] %>% 
  pivot_longer(cols=starts_with("Date"), names_to = "Date_index", values_to = "Date") %>% 
  mutate(Date_index = str_remove(Date_index, "Date."))

bolt_data_50_l <- bolt_data_50 %>% 
  select(label, index, starts_with("stemLength"), starts_with("leafLong"), starts_with("leafWide")) %>% 
  mutate_at(vars(matches("leafLong")),.funs = "as.double") %>% 
  mutate_at(vars(matches("leafWide")),.funs = "as.double") %>% 
  pivot_longer(cols = c(starts_with("leafLong"), starts_with("stemLength"), starts_with("leafWide")), values_to = "values") %>% 
  separate(col = "name", into = c("variable_name", "Date_index"), sep = "[.]") %>% 
  left_join(date_info, by = "Date_index") %>% 
  mutate(Date = as.Date(Date, "%m/%d/%y"))

paste("Date", 1:5)

paste0("Date.", 1:5)

library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

# Assuming bolt_data_50_l is your dataset and it's already loaded

p <- ggplot(data = filter(bolt_data_50_l, variable_name == "stemLength"), aes(x = Date, y = values)) +
  geom_line(aes(group = label), color = "pink") +
  geom_point() +
  geom_boxplot(aes(group = Date), fill = NA, color = "deeppink", width = 0.5) +
  theme_bw() +
  ggtitle("Stem Length") +
  theme(
    plot.title = element_text(color = "deeppink", size = 14, face = "bold", hjust = 0.5)
  )

print(p)

library(ggplot2)
library(dplyr)

# Assuming bolt_data_50_l is your dataset and it's already loaded

p2 <- ggplot(data = filter(bolt_data_50_l, variable_name == "leafLong"), aes(x = Date, y = values)) +
  geom_line(aes(group = label), color = "pink") +
  geom_point() +
  geom_boxplot(aes(group = Date), fill = NA, color = "deeppink", width = 0.5) +
  theme_bw() +
  ggtitle("Leaf Length") +
  theme(
    plot.title = element_text(color = "deeppink", size = 14, face = "bold", hjust = 0.5)
  )

print(p2)

library(ggplot2)
library(dplyr)

# Assuming bolt_data_50_l is your dataset and it's already loaded

p3 <- ggplot(data = filter(bolt_data_50_l, variable_name == "leafWide"), aes(x = Date, y = values)) +
  geom_boxplot(aes(group = Date), fill = NA, color = "deeppink", width = 0.5) +
  scale_x_date(breaks = unique(bolt_data_50_l$Date)) +
  geom_line(aes(group = label), color = "pink") +
  geom_point() +
  theme_bw() +
  ggtitle("Leaf Width") +
  theme(
    plot.title = element_text(color = "deeppink", size = 14, face = "bold", hjust = 0.5)
  )

print(p3)

# X axis by week

p3 <- ggplot(data = filter(bolt_data_50_l, variable_name == "leafWide"), aes(x = Date, y = values)) +
  geom_boxplot(aes(group = Date), fill = NA, color = "deeppink", width = 0.5) +
  scale_x_date(breaks = unique(bolt_data_50_l$Date), labels = c("W1", "W2", "W3", "W4", "W5")) +
  geom_line(aes(group = label), color = "pink") +
  geom_point() +
  theme_bw() +
  ggtitle("Leaf Width") +
  theme(
    plot.title = element_text(color = "deeppink", size = 14, face = "bold", hjust = 0.5)
  )

print(p3)

# simplify the question

test_1 <- c(1, 3, 12, 21, 35, 50)
test_2 <- c(NA, NA, 2, 6, 8, 14)
test_3 <- c(0.8, 1.2, NA, NA, NA)
test_4 <- c(0.4, 1.6, NA, NA, NA, 0.7)
test_5 <- c(NA, NA, NA, NA, NA, NA)

stem_present <- FALSE

for(i in 1:length)(test_4)){
  if (stem_present) == FALSE{
    if(is.na(test_4[[i]])){
      stem_present <- FALSE
    } else{
      stem_present <- TRUE
    }
  }
}

bolt_data <- read.csv("Boltonia Phenotype data_20240624(Phenotype Data Entry).csv", na.strings = c("", "NA", "N/A", "DNR"))

str(bolt_data)

convert_Y_TRUE <- function(x){
  output <- case_when(x == "Y" ~ TRUE, TRUE ~ FALSE)
  return(output)
}

bolt_data_sub <- bolt_data %>% 
  filter(index <= 10) %>% 
  mutate(stemLength.2 = as.numeric(stemLength.2), label = factor(label, levels = label)) %>% 
# mutate(Surv.2 = case_when(Surv.2 == "Y" ~ TRUE, TRUE ~ FALSE)) %>% 
  mutate_at(vars(matches("Surv.")), .funs = "convert_Y_TRUE")
## modify the mutate all Surv.X columns to logical data

str(bolt_data_sub)

date_info <- bolt_data %>% 
  select(str_c("Date.", 1:8)) %>% .[1,] %>% 
  pivot_longer(cols=starts_with("Date"), names_to = "Date_index", values_to = "Date") %>% 
  mutate(Date_index = str_remove(Date_index, "Date."))

bolt_data_l <- bolt_data %>% 
  select(label, index, starts_with("stemLength"), starts_with("leafLong"), starts_with("leafWide")) %>% 
  mutate_at(vars(matches("leafLong")),.funs = "as.double") %>% 
  mutate_at(vars(matches("leafWide")),.funs = "as.double") %>% 
  pivot_longer(cols = c(starts_with("leafLong"), starts_with("stemLength"), starts_with("leafWide")), values_to = "values") %>% 
  separate(col = "name", into = c("variable_name", "Date_index"), sep = "[.]") %>% 
  left_join(date_info, by = "Date_index") %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y"))

paste("Date", 1:8)

paste0("Date.", 1:8)

