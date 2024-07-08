library(tidyverse)
library(ggplot2)

setwd("C:/Users/nyanw/Downloads/REU/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/tidy_boltonia_data.csv")

unique(Boltonia_data$variable_name)

Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())

# make flowering ratio by Date into a plot
data <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.01709402, 0.02350427, 0.09615385, 0.13247863, 0.14957265, 0.17094017))
ggplot(data, aes(x = Date, y = Values)) + 
  geom_bar(stat = "identity")

# Then plot flowering ratio by Date and County
###Saint Clair###
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", County == "Saint Clair") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
StClair_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.00000000, 0.00000000, 0.00000000, 0.04761905, 0.09523810, 0.09523810))
ggplot(StClair_Ratio, aes(x = Date, y = Value)) + 
  geom_bar(stat = "identity")
###Fulton###
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", County == "Fulton") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
Fulton_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.03846154, 0.02564103, 0.14102564, 0.16666667, 0.17948718, 0.19230769))
ggplot(Fulton_Ratio, aes(x = Date, y = Value)) + 
  geom_bar(stat = "identity")
###Frederick###
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", County == "Frederick") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
Frederick_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.00, 0.00, 0.08, 0.20, 0.24, 0.28))
ggplot(Frederick_Ratio, aes(x = name, y = value)) + 
  geom_bar(stat = "identity")
###Tazewell###
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", County == "Tazewell") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
Tazewell_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.00, 0.00, 0.08, 0.20, 0.24, 0.28))
ggplot(Tazewell_Ratio, aes(x = Date, y = Value)) + 
  geom_bar(stat = "identity")
###

# Then plot flowering ratio by Date and MaternalLine
Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF", MaternalLine == "2011-2598-1") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())
MaternalLine_Ratio <- data.frame(
  name = c("2024-05-15", "2024-05-22", "2024-05-29","2024-06-05", "2024-06-12", "2024-06-19"),
  value = c(0.00000000, 0.02631579, 0.15789474, 0.23684211, 0.23684211, 0.34210526))
ggplot(MaternalLine_Ratio, aes(x = name, y = value)) + 
  geom_bar(stat = "identity")

# if we do not want those dead plants in flowering ratio calculation
mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA)))

##########################

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-01")), aes(x = County, y = values))+
  geom_violin(aes(fill = County), color = NA, alpha = 0.7)+
  geom_point()

p
ggsave("./figures/county_figure.png", width = 10, height = 10, dpi = 600)


p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = Google_Latitude, y = values))+
  geom_point()
p


p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = Google_Longitude, y = values))+
  geom_point()
p

##MaternalLine plots#######
###########################
p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point()+
  labs(title = "Maternal Line & Stem Length")
p
ggsave("./figures/MaternalLine_stemlength.png", width = 15, height = 10, dpi = 600)

p <-ggplot(data = filter(Boltonia_data, variable_name == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point()+
  labs(title = "Maternal Line & Numbder of Flower Buds")
p
ggsave("./figures/MaternalLine_numFlwrB.png", width = 15, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numLSt", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point()
p
ggsave("./figures/MaternalLine_numLSt.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRos", Date == as.Date("2024-06-19")), aes(x = MaternalLine, y = values))+
  geom_point()
p
ggsave("./figures/MaternalLine_numRos", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_point()
p
ggsave("./figures/MaternalLine_leaflong.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafWide", Date == as.Date("2024-05-15")), aes(x = MaternalLine, y = values))+
  geom_point()
p
ggsave("./figures/MaternalLine_leafwide.png", width = 10, height = 10, dpi = 600)

##State plots##########
#######################

p <- ggplot(data = filter(Boltonia_data, variable_name == "stemLength", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()+
  labs(title = "County & Stem Length")
p
ggsave("./figures/County_stemlength.png", width = 15, height = 10, dpi = 600)

p <-ggplot(data = filter(Boltonia_data, variable_name == "numFlwrB", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()+
  labs(title = "County & Number of Flower Buds")
p
ggsave("./figures/County_numFlwrB.png", width = 15, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numLSt", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()
p
ggsave("./figures/County_numLSt.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "numRos", Date == as.Date("2024-06-19")), aes(x = County, y = values))+
  geom_point()
p
ggsave("./figures/County_numRos", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafLong", Date == as.Date("2024-05-15")), aes(x = County, y = values))+
  geom_point()
p
ggsave("./figures/County_leaflong.png", width = 10, height = 10, dpi = 600)

p <- ggplot(data = filter(Boltonia_data, variable_name == "leafWide", Date == as.Date("2024-05-15")), aes(x = County, y = values))+
  geom_point()
p



