library(tidyverse)
library(ggpubr)

setwd("/Users/kuowenhsi/Library/CloudStorage/OneDrive-WashingtonUniversityinSt.Louis/MOBOT/MOBOT_Boltonia")

Boltonia_data <- read_csv("./data/tidy_boltonia_data.csv")

unique(Boltonia_data$variable_name)

# make flowering ratio by Date into a plot

Boltonia_data_FlowerRatio <- Boltonia_data %>%
  filter(variable_name == "numDiscF") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date) %>%
  summarize(flowerRatio = sum(values)/n())


p <- ggplot(data = Boltonia_data_FlowerRatio, mapping = aes(x = Date, y = flowerRatio)) + 
  geom_col(fill = "orange")+
  scale_y_continuous(limits = c(0, 1))+
  theme_bw()

p

# Then plot flowering ratio by Date and County

Boltonia_data_FlowerRatio_county <- Boltonia_data %>%
  filter(variable_name == "numDiscF") %>%
  mutate(values = case_when(is.na(values) ~ 0, values == 0 ~ 0, values >= 1 ~ 1, TRUE ~ as.numeric(NA))) %>%
  group_by(Date, County) %>%
  summarize(flowerRatio = sum(values)/n(), mean_Google_latitude = mean(Google_Latitude), mean_Google_longitude = mean(Google_Longitude))%>%
  arrange(County, Date)


# plot

# County first
p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = County, y = flowerRatio))+
  geom_col(aes(group = Date, fill = Date), position = position_dodge())
p


p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = Date, y = flowerRatio))+
  geom_col()+
  facet_wrap(.~County)
p


# Date first
p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = as.character(Date), y = flowerRatio))+
  geom_col(aes(group = County, fill = County), position = position_dodge())
p

p <- ggplot(data = Boltonia_data_FlowerRatio_county, aes(x = County, y = flowerRatio))+
  geom_col()+
  facet_wrap(.~Date)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p


# by latitude

p <- ggplot(data = Boltonia_data_FlowerRatio_county, mapping = aes(x = mean_Google_latitude, y = flowerRatio))+
  geom_point()+
  stat_smooth(method = "lm", color = "blue")+
  stat_cor(method = "pearson", label.x.npc = 0, label.y.npc = 0.80)+
  stat_regline_equation(label.x.npc = 0, label.y.npc = 0.95)+
  facet_wrap(.~Date)+
  theme_bw()
p

# by longitude

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



